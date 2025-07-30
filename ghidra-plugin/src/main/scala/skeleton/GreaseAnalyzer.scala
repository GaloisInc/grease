package skeleton;

/*
 * Copyright        : (c) Galois, Inc. 2025
 * Maintainer       : GREASE Maintainers <grease@galois.com>
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

object GreaseBackgroundCmd {
  val GREASE_BOOKMARK_TYPE = "GREASE"

  def greaseBookmark(
      prog: Program,
      addr: Address,
      category: String,
      note: String
  ): Unit = {
    prog
      .getBookmarkManager()
      .setBookmark(addr, GREASE_BOOKMARK_TYPE, category, note)
  }

  def addComment(comm: String, prog: Program, toAddr: Address): Unit = {
    val prevCom = Option(
      prog
        .getListing()
        .getComment(CodeUnit.PRE_COMMENT, toAddr)
    )
    val nextCom =
      prevCom
        .getOrElse("") + comm
    prog
      .getListing()
      .setComment(toAddr, CodeUnit.PRE_COMMENT, nextCom)
  }
}

class GreaseBackgroundCmd(
    val entrypoints: AddressSetView,
    timeout: Option[FiniteDuration],
    loadBase: Option[Long],
    rawMode: Boolean
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
          GreaseConfiguration(targetBin, item, timeout, loadBase, rawMode)
        )

        res match
          case Failure(exception) => {
            Msg.warn(this, s"GREASE could not analyze ${item}, ${exception}")
            GreaseBackgroundCmd.addComment(
              s"\n Failed to analyze function with GREASE",
              prog,
              item
            )
          }
          case scala.util.Success(bugs) => {
            for bug <- bugs.possibleBugs do
              GreaseBackgroundCmd.addComment(
                s"\n Possible BUG: ${bug.description.render()}",
                prog,
                bug.appliedTo
              )
              GreaseBackgroundCmd.greaseBookmark(
                prog,
                bug.appliedTo,
                "Possible bug",
                "GREASE detected a potential bug"
              )
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
      "Grease Analyzer",
      "A grease one shot analyzer that runs accross the entire codebase and collects a report ",
      AnalyzerType.FUNCTION_ANALYZER
    ) {

  setPriority(AnalysisPriority.DATA_TYPE_PROPOGATION.after())

  var timeoutDuration: Option[FiniteDuration] = None
  var loadBase: Option[Long] = None
  var shouldLoadRaw = false
  var useImageBaseAsLoadBase = true
  val supportedProcs: Set[String] = Set("ARM", "PowerPC", "x86")

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
  }

  @throws(classOf[CancelledException])
  override def added(
      program: Program,
      set: AddressSetView,
      monitor: TaskMonitor,
      log: MessageLog
  ): Boolean = {
    GreaseBackgroundCmd(set, timeoutDuration, loadBase, shouldLoadRaw)
      .applyTo(program, monitor)
    true
  }
}
