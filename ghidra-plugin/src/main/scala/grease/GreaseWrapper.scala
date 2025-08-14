package grease

/*
 * Copyright        : (c) Galois, Inc. 2025
 */
import ghidra.program.model.address.{Address, AddressFactory}
import ghidra.program.model.listing.Program
import ghidra.app.util.opinion.ElfLoader
import ghidra.util.Msg
import scala.concurrent.duration._
import scala.util.Try
import scala.util.{Failure, Success}
import scala.Serializable
import scala.jdk.CollectionConverters._
import ghidra.framework.Application
import java.io.File

object AddrConversions {

  def greaseOffsetToAddr(greaseOffset: Long, prog: Program): Address =
    // Convert a greaseOffset in an ELF to a Ghidra address.
    // This function assumes the greaseOffset is in memory in the default address space (where code will be)
    // GREASE loads the binary at the elf base so the offset is relative to the ELF base.
    // Then we add the raw offset to image base in Ghidra.
    val addressOffset = (greaseOffset - ElfLoader
      .getElfOriginalImageBase(prog))

    prog.getImageBase().add(addressOffset)
}

object GreaseConfiguration {
  def renderAddress(x: Long): String = f"0x$x%x"
}

trait AddressingMode {
  def ghidraAddressToGreaseOffset(addr: Address): Long
  def greaseOffsettoGhidraAddress(greaseOff: Long): Address
  val loadBase: Option[Long]
}

class ELFAddressing(val prog: Program) extends AddressingMode {

  override def ghidraAddressToGreaseOffset(addr: Address): Long =
    addr.subtract(prog.getImageBase()) + ElfLoader
      .getElfOriginalImageBase(prog)

  override def greaseOffsettoGhidraAddress(greaseOff: Long): Address =
    AddrConversions.greaseOffsetToAddr(greaseOff, prog)

  override val loadBase: Option[Long] = None

}

class RawBase(val loadBaseOption: Option[Long], prog: Program)
    extends AddressingMode {

  def toRAMAddress(spaceOffset: Long): Address =
    prog
      .getAddressFactory()
      .getAddress(
        prog.getAddressFactory().getDefaultAddressSpace().getSpaceID(),
        spaceOffset
      )

  val loadBaseAddr = loadBaseOption
    .map(toRAMAddress(_))
    .getOrElse(prog.getImageBase())

  // TODO(#297): We assume that the when we are in raw mode
  // that the file is loaded such that offsets and addresses are
  // always equivalent.
  override def ghidraAddressToGreaseOffset(addr: Address): Long =
    addr.getOffset()

  override def greaseOffsettoGhidraAddress(greaseOff: Long): Address =
    toRAMAddress(greaseOff)

  override val loadBase: Option[Long] = Some(loadBaseAddr.getOffset())
}

case class GreaseConfiguration(
    val targetBinary: os.Path,
    entrypoint: Address,
    // None uses the default for grease
    timeout: Option[FiniteDuration],
    loadBase: Option[Long],
    isRawBinary: Boolean,
    overridesFile: Option[File]
) {

  val GHIDRA_THUMB_REG_NAME = "TMode"

  def isThumbMode(prog: Program, addr: Address): Boolean = {
    val ctx = prog.getProgramContext()
    val tmodeVal =
      ctx.getValue(ctx.getRegister(GHIDRA_THUMB_REG_NAME), addr, false)
    Msg.info(this, s"Tmode val: $tmodeVal")
    Option(tmodeVal)
      .map(_.longValue())
      .getOrElse(0L) != 0L
  }

  def addressResolver(prog: Program): AddressingMode =
    if isRawBinary then RawBase(loadBase, prog) else ELFAddressing(prog)

  def commandLine(prog: Program): Seq[String] = {
    var modAddr = addressResolver(prog).ghidraAddressToGreaseOffset(entrypoint)
    val isARM = prog.getLanguage().getProcessor().toString() == "ARM"
    // Unfortunately we need to special case here to handle setting up arm thumb mode:
    if isARM && isThumbMode(
        prog,
        entrypoint
      )
    then
      Msg.info(this, "Thumb mode addressing")
      modAddr = modAddr | 1
    else Msg.info(this, "Non thumb mode addressing")

    val baseLine =
      Seq(
        targetBinary.toString(),
        "--json",
        "--address",
        GreaseConfiguration.renderAddress(modAddr)
      )
    val rawLine = if isRawBinary then Seq("--raw-binary") else Seq()
    val timeoutLine =
      timeout.map(x => Seq("--timeout", x.toMillis.toString)).getOrElse(Seq())
    val baseAddr: Seq[String] = addressResolver(prog).loadBase
      .filter(_ => isRawBinary)
      .map(x => Seq("--load-base", GreaseConfiguration.renderAddress(x)))
      .getOrElse(Seq())
    val overrideLine =
      overridesFile.map(x => Seq("--overrides", x.getPath())).getOrElse(Seq())

    baseLine ++ rawLine ++ timeoutLine ++ baseAddr ++ overrideLine
  }
}

case class GreaseException(val msg: String) extends Exception

object GreaseResult {
  def parseBatch(
      ent: Address,
      batch: String,
      addrs: AddressingMode
  ): Option[PossibleBug] = {
    val js = ujson.read(batch)
    val hasBug = js("batchStatus")("tag").str == "BatchBug"
    if !hasBug then return None

    val contents = js("batchStatus")("contents")
    val desc = contents("bugDesc")
    try {
      val base = 16
      // drops 0x, I dont know of a java method that parses based on format
      val loc = Integer.parseInt(desc("bugLoc").str.drop(2), base).toLong
      val addr = addrs.greaseOffsettoGhidraAddress(loc)
      Some(
        PossibleBug(
          addr,
          desc,
          contents("bugArgs").arr,
          contents("bugShapes").str
        )
      )
    } catch {
      // GREASE can dump "Multiple Locations:" from checkMustFail currently we use the original addr
      case e: NumberFormatException => {
        Some(
          PossibleBug(
            ent,
            desc,
            contents("bugArgs").arr,
            contents("bugShapes").str
          )
        )
      }
    }

  }

  def parse(
      ent: Address,
      chunks: String,
      addrs: AddressingMode
  ): Try[GreaseResult] = {
    // each line should have a batch
    Success(
      GreaseResult(
        chunks
          .lines()
          .iterator()
          .asScala
          .flatMap(parseBatch(ent, _, addrs))
          .toList
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

object GreaseWrapper {
  def apply(prog: Program): GreaseWrapper = {
    new GreaseWrapper(
      os.Path(Application.getOSFile("grease").getAbsoluteFile()),
      prog
    )
  }
}

class GreaseWrapper(val localRunner: os.Path, val prog: Program) {
  def runGrease(conf: GreaseConfiguration): Try[GreaseResult] = {
    val cmd = Seq(localRunner.toString()) ++ conf.commandLine(prog)
    Msg.info(this, s"Runnning GREASE cmd: ${cmd}")
    val proc = os.proc(cmd)
    // maybe enforce timeout here
    val result = proc.call(check = false)
    if result.exitCode != 0 then
      Msg.error(this, s"GREASE failed to run with ${result.err}")
      return Failure(GreaseException("GREASE process failed"))
    val res = result.out.text()
    Msg.info(this, s"Received: $res")
    GreaseResult.parse(conf.entrypoint, res, conf.addressResolver(prog))
  }

}
