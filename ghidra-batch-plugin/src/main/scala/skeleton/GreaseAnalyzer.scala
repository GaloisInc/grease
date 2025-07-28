/* ###
 * IP: GHIDRA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package skeleton;

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
import ghidra.framework.Application
import scala.util.Failure
import ghidra.program.model.listing.Listing
import ghidra.program.model.listing.CodeUnit
import ghidra.framework.options.OptionType
import scala.concurrent.duration._

class GreaseBackgroundCmd(
    val entrypoints: AddressSetView,
    timeout: Option[FiniteDuration],
    loadBase: Option[Long],
    rawMode: Boolean
) extends BackgroundCommand[Program] {

  override def applyTo(prog: Program, monitor: TaskMonitor): Boolean = {
    val bldr = AcyclicCallGraphBuilder(prog, entrypoints, true)
    val depgraph = bldr.getDependencyGraph(monitor)

    if depgraph.isEmpty() then return true

    val runnable: QRunnable[Address] = new QRunnable[Address]() {
      def run(item: Address, monitor: TaskMonitor) = {
        val targetBin = os.Path(prog.getExecutablePath())
        val res = GreaseWrapper(
          os.Path(Application.getOSFile("grease").getAbsoluteFile()),
          prog
        ).runGrease(
          GreaseConfiguration(targetBin, item, timeout, loadBase, rawMode)
        )

        res match
          case Failure(exception) => throw exception
          case scala.util.Success(bugs) => {
            for bug <- bugs.possibleBugs do
              val prevcom = prog
                .getListing()
                .getComment(CodeUnit.PRE_COMMENT, bug.appliedTo)
              val nextCom =
                prevcom + s"\n Possible BUG: ${bug.description.render()}"
              prog
                .getListing()
                .setComment(bug.appliedTo, CodeUnit.PRE_COMMENT, nextCom)
          }
      }
    }

    val pool = AutoAnalysisManager.getSharedAnalsysThreadPool()
    val queue = ConcurrentGraphQ(runnable, depgraph, pool, monitor)
    monitor.setMessage("Running grease")
    monitor.initialize(depgraph.size())
    queue.execute()

    true
  }

}

object GreaseAnalyzer {

  val TIMEOUT_OPT: String = "Timeout"
  val RAW_MODE_OPT: String = "Raw mode"
  val LOAD_BASE_OPT: String = "Load base"

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

  var timeoutDuration: Option[FiniteDuration] = None
  var loadBase: Option[Long] = None
  var shouldLoadRaw = false
  val supportedProcs: Set[String] = Set("ARM")

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
    loadBase = GreaseAnalyzer.getOption[Long](x, GreaseAnalyzer.LOAD_BASE_OPT)
  }

  override def registerOptions(options: Options, program: Program): Unit = {
    options.registerOption(
      GreaseAnalyzer.RAW_MODE_OPT,
      OptionType.BOOLEAN_TYPE,
      false,
      null,
      "Load binary in GREASE in raw mode"
    );

    options.registerOption(
      GreaseAnalyzer.LOAD_BASE_OPT,
      OptionType.LONG_TYPE,
      0L,
      null,
      "Load binary in GREASE at this load base in raw mode (defaults to image base)"
    );

    options.registerOption(
      GreaseAnalyzer.TIMEOUT_OPT,
      OptionType.LONG_TYPE,
      0L,
      null,
      "Timeout for GREASE on each function in milliseconds"
    );

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
