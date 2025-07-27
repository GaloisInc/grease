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

class GreaseBackgroundCmd(val entrypoints: AddressSetView)
    extends BackgroundCommand[Program] {

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
        ).runGrease(GreaseConfiguration(targetBin, item, None, false))

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

/** TODO: Provide class-level documentation that describes what this analyzer
  * does.
  */
class GreaseAnalyzer
    extends AbstractAnalyzer(
      "Grease Analyzer",
      "A grease one shot analyzer that runs accross the entire codebase and collects a report ",
      AnalyzerType.FUNCTION_ANALYZER
    ) {

  val supportedProcs: Set[String] = Set("ARM")
  setSupportsOneTimeAnalysis()

  override def getDefaultEnablement(program: Program): Boolean = {

    // TODO: Return true if analyzer should be enabled by default

    false
  }

  override def canAnalyze(program: Program): Boolean = {
    // TODO: we should probably check the lang
    val proc = program.getLanguage().getProcessor().toString()
    supportedProcs.contains(proc)
  }

  override def registerOptions(options: Options, program: Program): Unit = {

    // TODO: If this analyzer has custom options, register them here

    options.registerOption(
      "Option name goes here",
      false,
      null,
      "Option description goes here"
    );
  }

  @throws(classOf[CancelledException])
  override def added(
      program: Program,
      set: AddressSetView,
      monitor: TaskMonitor,
      log: MessageLog
  ): Boolean = {

    // TODO: Perform analysis when things get added to the 'program'.  Return true if the
    // analysis succeeded.

    GreaseBackgroundCmd(set).applyTo(program, monitor)
    true
  }
}
