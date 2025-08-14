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
import ghidra.program.model.listing.CommentType

object AddrConversions {

  def greaseOffsetToAddr(greaseOffset: Long, prog: Program, loadOffset: Long): Address =
    // Convert a greaseOffset in an ELF to a Ghidra address.
    // This function assumes the greaseOffset is in memory in the default address space (where code will be)
    // GREASE loads the binary at the elf base so the offset is relative to the ELF base.
    // Then we add the raw offset to image base in Ghidra.
    val addressOffset = (greaseOffset - loadOffset - ElfLoader
      .getElfOriginalImageBase(prog))

    prog.getImageBase().add(addressOffset)
}

object GreaseConfiguration {
  def renderAddress(x: Long): String = f"0x$x%x"
}

trait AddressingMode {
  def ghidraAddressToGreaseOffset(addr: Address): Long
  def greaseOffsettoGhidraAddress(greaseOff: Long, loadOffset: Long): Address
  val loadBase: Option[Long]
}

class ELFAddressing(val prog: Program) extends AddressingMode {

  override def ghidraAddressToGreaseOffset(addr: Address): Long =
    addr.subtract(prog.getImageBase()) + ElfLoader
      .getElfOriginalImageBase(prog)

  override def greaseOffsettoGhidraAddress(greaseOff: Long, loadOffset: Long): Address =
    AddrConversions.greaseOffsetToAddr(greaseOff, prog, loadOffset)

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

  override def greaseOffsettoGhidraAddress(greaseOff: Long, loadOffset: Long): Address =
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
  def parseLoc(locStr: String, addrs: AddressingMode, loadOffset: Long) = {
    // drops 0x, I dont know of a java method that parses based on format
    val loc = Integer.parseInt(locStr.drop(2), 16).toLong
    addrs.greaseOffsettoGhidraAddress(loc, loadOffset)
  }

  def parseBug(
      ent: Address,
      addrs: AddressingMode,
      contents: ujson.Value,
      loadOffset: Long
  ): Option[PossibleBug] = {
    val desc = contents("bugDesc")

    try {
      val addr = GreaseResult.parseLoc(desc("bugLoc").str, addrs, loadOffset)
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

  def parseCouldNotInfer(
      ent: Address,
      addrs: AddressingMode,
      contents: ujson.Value,
      loadOffset: Long
  ): Option[FailedToRefine] = {
    def parsePred(x: ujson.Value): FailedPredicate =
      FailedPredicate(
        parseLoc(x("_failedPredicateLocation").str, addrs, loadOffset),
        x("_failedPredicateMessage").str,
        x("_failedPredicateConcShapes").str
      )

    Some(FailedToRefine(ent, contents.arr.toSeq.map(parsePred)))
  }

  def parseBatch(
      ent: Address,
      batch: String,
      addrs: AddressingMode
  ): Option[ParsedBatch] = {
    val js = ujson.read(batch)

    def getLoadOffset(): Long =
        js("batchLoadOffset").num.toLong

    js("batchStatus")("tag").str match
      case "BatchBug" => parseBug(ent, addrs, js("batchStatus")("contents"), getLoadOffset())
      case "BatchCouldNotInfer" =>
        parseCouldNotInfer(ent, addrs, js("batchStatus")("contents"), getLoadOffset())
      case _ => None
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
) extends ParsedBatch {
  def printToProg(prog: Program): Unit =
    ParsedBatch.addComment(
      s"\n Possible bug: ${description.render()}",
      prog,
      appliedTo
    )
    ParsedBatch.greaseBookmark(
      prog,
      appliedTo,
      "Possible bug",
      "GREASE detected a potential bug"
    )
}

case class FailedPredicate(loc: Address, message: String, concShapes: String) {}

case class FailedToRefine(inFunc: Address, preds: Seq[FailedPredicate])
    extends ParsedBatch {

  override def printToProg(prog: Program): Unit = {
    for (pred <- preds)
      Msg.info(this, s"Adding safety pred comment for $inFunc")
      ParsedBatch.addComment(
        s"Safety predicate failed: ${pred.message}",
        prog,
        pred.loc
      )
      ParsedBatch.addComment(
        s"Concretized invalidating shape ${pred.concShapes}",
        prog,
        pred.loc
      )

    ParsedBatch.greaseBookmark(
      prog,
      inFunc,
      "Failed to Refine",
      "could not refine to satisfy a safety condition"
    )
  }
}

object ParsedBatch {
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
        .getComment(CommentType.PRE, toAddr)
    )
    val nextCom =
      prevCom
        .getOrElse("") + comm + "\n"
    prog
      .getListing()
      .setComment(toAddr, CommentType.PRE, nextCom)
  }
}

sealed abstract class ParsedBatch {
  def printToProg(prog: Program): Unit
}

// Coarse grained results for now, grab potential bugs only
case class GreaseResult(val possibleBugs: List[ParsedBatch]) {}

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
