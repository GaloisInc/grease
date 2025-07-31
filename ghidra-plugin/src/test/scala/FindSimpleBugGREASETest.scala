import ghidra.test.AbstractGhidraHeadlessIntegrationTest
import ghidra.test.AbstractGhidraHeadedIntegrationTest
import ghidra.app.plugin.core.analysis.AutoAnalysisManager
import ghidra.base.project.GhidraProject
import ghidra.test.TestEnv
import ghidra.program.model.listing.Program
import scala.compiletime.uninitialized
import java.io.File
import ghidra.util.task.TaskMonitor
import ghidra.util.task.TimeoutTaskMonitor
import java.util.concurrent.TimeUnit
import ghidra.program.model.listing.CodeUnit
import grease.GreaseAnalyzer
import org.junit.Before;
import org.junit.After;
import org.junit.Test;
import ghidra.framework.Application;
import ghidra.framework.ApplicationConfiguration
import ghidra.GhidraTestApplicationLayout
import generic.test.AbstractGTest
import utility.application.ApplicationLayout
import ghidra.framework.GModule
import java.{util => ju}
import ghidra.program.model.listing.CommentType
import scala.collection.JavaConverters._
import java.nio.file.FileSystems
import generic.jar.ResourceFile
import ju.Collections

class FindSimpleBugGREASETest extends AbstractGhidraHeadlessIntegrationTest {
  var proj: GhidraProject = uninitialized
  var env: TestEnv = uninitialized

  def loadProgram(proj: GhidraProject, name: String): Program = {
    val file = File(getClass.getResource(name).getFile())
    val prog = proj.importProgram(file)
    val analysisManager = AutoAnalysisManager.getAnalysisManager(prog)
    analysisManager.startAnalysis(TaskMonitor.DUMMY)
    analysisManager.waitForAnalysis(null, TaskMonitor.DUMMY)
    prog
  }

  override def createApplicationLayout(): ApplicationLayout = {
    new GhidraTestApplicationLayout(
      new File(AbstractGTest.getTestDirectoryPath())
    ) {
      override def findGhidraModules(): ju.Map[String, GModule] = {
        val initMap = ju.HashMap(super.findGhidraModules()).asScala
        val projectDir =
          FileSystems.getDefault().getPath("").toAbsolutePath().toFile()
        val modName = projectDir.getName()
        val module = GModule(applicationRootDirs, ResourceFile(projectDir))
        initMap.put(modName, module)
        Collections.unmodifiableMap(initMap.asJava)
      }

    }
  }
  @Before
  def before() = {
    env = new TestEnv()
    proj = env.getGhidraProject()
  }

  @After
  def after() = {
    env.dispose()
  }

  @Test
  def testSimpleBug() = {
    Application
      .getModuleRootDirectories()
      .forEach(x => {
        System.err.println("App search dir: " + x.getAbsolutePath())
      })

    assert(Option(getClass().getResource("testbins/buggy-armv7l")).isDefined)
    val prog = loadProgram(proj, "testbins/buggy-armv7l")
    assert(Option(prog).isDefined)
    val analysisManager = AutoAnalysisManager.getAnalysisManager(prog)
    analysisManager.scheduleOneTimeAnalysis(
      analysisManager.getAnalyzer("GREASE Analyzer"),
      prog.getMemory()
    )
    analysisManager.waitForAnalysis(
      null,
      TimeoutTaskMonitor.timeoutIn(30, TimeUnit.SECONDS)
    )

    val addr = prog.getAddressFactory().getAddress("0x00010088")
    val comm = prog.getListing().getComment(CommentType.PRE, addr)
    assert(
      Option(comm).isDefined,
      "The comment should exist at the address 0x00010088"
    )
    assert(
      comm.contains("Possible bug:"),
      s"The comment $comm at 0x00010088 should contain Possible BUG"
    )
  }
}
