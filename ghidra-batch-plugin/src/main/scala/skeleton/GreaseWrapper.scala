package skeleton

import ghidra.program.model.address.Address
import ghidra.program.model.listing.Program
import ghidra.app.util.opinion.ElfLoader
import ghidra.util.Msg
import scala.concurrent.duration._
import scala.util.Try
import scala.util.Failure
import scala.Serializable

case class GreaseConfiguration(
    val targetBinary: os.Path,
    entrypoint: Address,
    // None uses the default for grease
    timeout: Option[FiniteDuration],
    isRawBinary: Boolean
) {
  def commandLine(prog: Program): Seq[String] = {
    val modAddr = entrypoint.subtract(prog.getImageBase()) + ElfLoader
      .getElfOriginalImageBase(prog)
    val baseline = Seq(targetBinary.toString(), "--address", f"$modAddr%x")
    val rawline = if isRawBinary then Seq("--raw-binary") else Seq()
    val timeoutline =
      timeout.map(x => Seq("--timeout", x.toMillis.toString)).getOrElse(Seq())
    baseline ++ rawline ++ timeoutline
  }
}

case class GreaseException(val msg: String) extends Exception

case class GreaseResult() {}

class GreaseWrapper(val localRunner: os.Path, val prog: Program) {
  def runGrease(conf: GreaseConfiguration): Try[GreaseResult] = {
    val cmd = Seq(localRunner.toString()) ++ conf.commandLine(prog)
    Msg.info(this, s"Runnning GREASE cmd: ${cmd}")
    val proc = os.proc(cmd)
    // maybe enforce timeout here
    val result = proc.call()
    if result.exitCode != 0 then
      Msg.error(this, s"Grease failed to run with ${result.err}")
      return Failure(GreaseException("Grease process failed"))

    ???
  }

}
