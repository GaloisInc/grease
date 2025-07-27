package skeleton

import ghidra.program.model.address.{Address, AddressFactory}
import ghidra.program.model.listing.Program
import ghidra.app.util.opinion.ElfLoader
import ghidra.util.Msg
import scala.concurrent.duration._
import scala.util.Try
import scala.util.{Failure, Success}
import scala.Serializable
import scala.jdk.CollectionConverters._

object AddrConversions {
  def greaseOffsetToAddr(greaseOffset: Long, prog: Program): Address =
    prog.getAddressFactory.getAddress(
      prog.getAddressFactory().getDefaultAddressSpace().getSpaceID(),
      (greaseOffset - ElfLoader
        .getElfOriginalImageBase(prog))
        + prog.getImageBase().getOffset()
    )

}

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
    val baseline =
      Seq(targetBinary.toString(), "--json", "--address", f"0x$modAddr%x")
    val rawline = if isRawBinary then Seq("--raw-binary") else Seq()
    val timeoutline =
      timeout.map(x => Seq("--timeout", x.toMillis.toString)).getOrElse(Seq())
    baseline ++ rawline ++ timeoutline
  }
}

case class GreaseException(val msg: String) extends Exception

object GreaseResult {
  def parseBatch(btch: String, prog: Program): Option[PossibleBug] = {
    val js = ujson.read(btch)
    val hasBug = js("batchStatus")("tag").str == "BatchBug"
    if !hasBug then return None

    val contents = js("batchStatus")("contents")
    val desc = contents("bugDesc")
    val loc = Integer.parseInt(desc("bugLoc").str.drop(2), 16).toLong
    val addr = AddrConversions.greaseOffsetToAddr(loc, prog)

    Some(
      PossibleBug(
        addr,
        desc,
        contents("bugArgs").arr,
        contents("bugShapes").str
      )
    )
  }

  def parse(chnks: String, prog: Program): Try[GreaseResult] = {
    // each line should have a batch
    Success(
      GreaseResult(
        chnks.lines().iterator().asScala.flatMap(parseBatch(_, prog)).toList
      )
    )
  }
}

case class PossibleBug(
    appliedTo: Address,
    description: ujson.Value,
    args: ujson.Arr,
    shapes: String
)

// Coarse grained results for now, grab potential bugs only
case class GreaseResult(val possibleBugs: List[PossibleBug]) {}

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

    GreaseResult.parse(result.out.text(), prog)
  }

}
