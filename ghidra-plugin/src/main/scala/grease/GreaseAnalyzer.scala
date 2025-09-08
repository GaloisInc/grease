package grease;

/*
 * Copyright        : (c) Galois, Inc. 2025
 */

import ghidra.app.services.AbstractAnalyzer;
import ghidra.app.services.AnalyzerType;
import ghidra.app.util.importer.MessageLog;
import ghidra.framework.options.Options;
import ghidra.program.model.address.AddressSetView;
import ghidra.program.model.listing.Program;
import ghidra.util.exception.CancelledException;
import ghidra.util.task.TaskMonitor;
import ghidra.framework.cmd.BackgroundCommand
import ghidra.framework.model.DomainObject
import ghidra.program.model.util.AcyclicCallGraphBuilder
import generic.concurrent.{ConcurrentGraphQ, QRunnable}
import ghidra.app.plugin.core.analysis.AutoAnalysisManager;
import ghidra.program.model.address.Address
import scala.util.Failure
import ghidra.program.model.listing.Listing
import ghidra.program.model.listing.CodeUnit
import ghidra.framework.options.OptionType
import ghidra.util.Msg
import scala.concurrent.duration._
import ghidra.app.services.AnalysisPriority
import ghidra.program.model.listing.CommentType
import java.io.File

class GreaseBackgroundCmd(
    val entrypoints: AddressSetView,
    timeout: Option[FiniteDuration],
    loadBase: Option[Long],
    rawMode: Boolean,
    overridesFile: Option[File],
    loopBound: Option[Int],
    useDebug: Boolean
) extends BackgroundCommand[Program] {

  override def applyTo(prog: Program, monitor: TaskMonitor): Boolean = {
    val bldr = AcyclicCallGraphBuilder(prog, entrypoints, true)
    val depGraph = bldr.getDependencyGraph(monitor)

    if depGraph.isEmpty() then return true

    val runnable: QRunnable[Address] = new QRunnable[Address]() {
      def run(item: Address, monitor: TaskMonitor) = {
        if item.isExternalAddress() then
          Msg.info(this, s"Skipping EXTERNAL function $item")
          return

        val targetBin = os.Path(prog.getExecutablePath())
        val res = GreaseWrapper(
          prog
        ).runGrease(
          GreaseConfiguration(
            targetBin,
            item,
            timeout,
            loadBase,
            rawMode,
            overridesFile,
            loopBound,
            useDebug
          )
        )

        res match
          case Failure(exception) => {
            Msg.warn(this, s"GREASE could not analyze ${item}, ${exception}")
            ParsedBatch.addComment(
              s"\n Failed to analyze function with GREASE",
              prog,
              item
            )
          }
          case scala.util.Success(items) => {
            for bug <- items.possibleBugs do bug.printToProg(prog)
          }

        monitor.increment()
      }
    }

    val pool = AutoAnalysisManager.getSharedAnalsysThreadPool()
    val queue = ConcurrentGraphQ(runnable, depGraph, pool, monitor)
    monitor.setMessage("Running grease")
    monitor.initialize(depGraph.size())
    queue.execute()

    true
  }

}

object GreaseAnalyzer {

  val TIMEOUT_OPT: String = "Timeout"
  val RAW_MODE_OPT: String = "Raw mode"
  val LOAD_BASE_OPT: String = "Load base"
  val USE_IMAGE_BASE_AS_LOAD_BASE_OPT: String = "Use image base as load base"
  val OVERRIDE_FILE: String = "Override file"
  val LOOP_BOUND_OPT: String = "Loop bound"
  val SHOULD_USE_DEBUG_OPT: String = "Use debug info for shapes"

  def getOption[T](options: Options, optName: String): Option[T] = {
    Option(options.getObject(optName, null))
      .map(x => {
        x.asInstanceOf[T]
      })
  }
}

/** An analyzer that runs GREASE on all functions in the binary and annotates
  * the database.
  */
class GreaseAnalyzer
    extends AbstractAnalyzer(
      "GREASE Analyzer",
      "A grease one shot analyzer that runs accross the entire codebase and collects a report ",
      AnalyzerType.FUNCTION_ANALYZER
    ) {

  setPriority(AnalysisPriority.DATA_TYPE_PROPOGATION.after())

  var timeoutDuration: Option[FiniteDuration] = None
  var loadBase: Option[Long] = None
  var loopBound: Option[Int] = None
  var shouldLoadRaw = false
  var useImageBaseAsLoadBase = true
  var overridesFile: Option[File] = None
  val supportedProcs: Set[String] = Set("ARM", "PowerPC", "x86")
  var shouldUseDebug = false

  setSupportsOneTimeAnalysis()

  override def getDefaultEnablement(program: Program): Boolean = {
    false
  }

  override def canAnalyze(program: Program): Boolean = {
    // TODO: we should probably check the lang
    val proc = program.getLanguage().getProcessor().toString()
    supportedProcs.contains(proc)
  }

  override def optionsChanged(x: Options, prog: Program): Unit = {
    timeoutDuration = GreaseAnalyzer
      .getOption[Long](x, GreaseAnalyzer.TIMEOUT_OPT)
      .filter(l => l != 0)
      .map(FiniteDuration(_, MILLISECONDS))
    shouldLoadRaw = GreaseAnalyzer
      .getOption[Boolean](x, GreaseAnalyzer.RAW_MODE_OPT)
      .getOrElse(false)
    loadBase = GreaseAnalyzer
      .getOption[Long](x, GreaseAnalyzer.LOAD_BASE_OPT)
      .filter(_ => !useImageBaseAsLoadBase)
    useImageBaseAsLoadBase = GreaseAnalyzer
      .getOption[Boolean](x, GreaseAnalyzer.USE_IMAGE_BASE_AS_LOAD_BASE_OPT)
      .getOrElse(true)
    overridesFile =
      GreaseAnalyzer.getOption[File](x, GreaseAnalyzer.OVERRIDE_FILE)
    loopBound = GreaseAnalyzer
      .getOption[Int](x, GreaseAnalyzer.LOOP_BOUND_OPT).filter(l => l != 0)
    shouldUseDebug = GreaseAnalyzer
      .getOption[Boolean](x, GreaseAnalyzer.SHOULD_USE_DEBUG_OPT)
      .getOrElse(false)
  }

  override def registerOptions(options: Options, program: Program): Unit = {
    options.registerOption(
      GreaseAnalyzer.RAW_MODE_OPT,
      OptionType.BOOLEAN_TYPE,
      false,
      null,
      "Load binary in GREASE in raw mode"
    )

    options.registerOption(
      GreaseAnalyzer.LOAD_BASE_OPT,
      OptionType.LONG_TYPE,
      0L,
      null,
      "Load binary in GREASE at this load base in raw mode (defaults to image base)"
    )

    options.registerOption(
      GreaseAnalyzer.TIMEOUT_OPT,
      OptionType.LONG_TYPE,
      0L,
      null,
      "Timeout for GREASE on each function in milliseconds"
    )

    options.registerOption(
      GreaseAnalyzer.USE_IMAGE_BASE_AS_LOAD_BASE_OPT,
      OptionType.BOOLEAN_TYPE,
      true,
      null,
      "Uses the image base of this Ghidra binary as the load base when in raw mode (this is the default, disable to use custom image base)"
    )

    options.registerOption(
      GreaseAnalyzer.OVERRIDE_FILE,
      OptionType.FILE_TYPE,
      null,
      null,
      "Override file to use in GREASE"
    )

    options.registerOption(
      GreaseAnalyzer.LOOP_BOUND_OPT,
      OptionType.INT_TYPE,
      0,
      null,
      "Loop bound limit for analysis"
    )

    options.registerOption(
      GreaseAnalyzer.SHOULD_USE_DEBUG_OPT,
      OptionType.BOOLEAN_TYPE,
      true,
      null,
      "Uses any available DWARF information to populate shapes with type signatures"
    )
  }

  @throws(classOf[CancelledException])
  override def added(
      program: Program,
      set: AddressSetView,
      monitor: TaskMonitor,
      log: MessageLog
  ): Boolean = {
    GreaseBackgroundCmd(
      set,
      timeoutDuration,
      loadBase,
      shouldLoadRaw,
      overridesFile,
      loopBound,
      shouldUseDebug
    )
      .applyTo(program, monitor)
    true
  }
}
