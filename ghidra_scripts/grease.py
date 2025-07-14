## ###
# Galois, Inc.
##
# GREASE Requirements Analysis
#
# This script invokes the `grease` program analysis tool.
# - You may optionally specify the path to your `grease` binary by setting the GHIDRA_GREASE_BIN environment variable. If not set, this script will prompt for the path to your `grease` binary.
# - Similarly, you may optionally specify a directory containing override files by setting the GHIDRA_GREASE_OVERRIDES environment variable. If not set, the script will prompt for a directory.
#
# Prerequisite: if you don't already have a `grease` binary:
# 1. Clone the `grease` source repository
# 2. Build the binary with `cabal build`
# 3. Locate the binary with `cabal exec -- which grease`
#
# Ghidra Plugin Installation:
# 1. Clone the `grease` repo
# 2. In Ghidra, use the CodeBrowser Script Manager -> Manage Script Directories to add the `grease/ghidra_scripts/` path (from the grease repo)
# 3. (Optional) In the Ghidra Script Manager, find `grease.py` and check the "In Tool" checkbox, to add "GREASE Analysis" to the Tools menu
#
# @author Galois, Inc.
# @category Analysis
# @menupath Tools.GREASE Analysis
# @copyright Galois Inc. 2024

# Many names are defined by Ghidra, but Ruff doesn't know that
# Also allow star imports for builtin stubs
# ruff: noqa: F821 F405 F403

# This one is just too opinionated
# ruff: noqa: E741

import json
import math
import os
import re
import subprocess
import tempfile
import typing

if typing.TYPE_CHECKING:
    from ghidra.ghidra_builtins import *  # type: ignore


from java.awt import Color

from ghidra.app.plugin.core.analysis.rust.RustUtilities import isRustProgram
from ghidra.app.util.opinion import ElfLoader


### CONSTANTS ###

# List of optional requirements a user may selectively enable.
# If this list is empty, the optional-requirement dialog will be skipped.
#
# Format: list of tuples, each tuple contains two elements:
# 1. a string that is matched by Grease.Requirement reqParser
# 2. a string description for the requirement to be displayed to the Ghidra user
OPTIONAL_REQUIREMENTS = []

LOOP_BOUND = 512
MAX_ITERS = 512
TIMEOUT_MS = 60000


### UTILITY FUNCTIONS ###


def addPlateComment(address, comment):
    """Like setPlateComment, but appends to any previous content rather than replacing."""
    previousContent = getPlateComment(address)
    if previousContent:
        comment = previousContent + "\n" + comment
    setPlateComment(address, comment)


def addPreComment(address, comment):
    """Like setPreComment, but appends to any previous content rather than replacing."""
    previousContent = getPreComment(address)
    if previousContent:
        comment = previousContent + "\n" + comment
    setPreComment(address, comment)


def addBookmark(address, category, note):
    """Like createBookmark, but uses the GREASE bookmark type.

    By using our own type we:
    - make it easy for the user to filter for/out GREASE bookmarks specifically
    - enable the user to add their own (e.g. note) bookmarks at the same
      location without overwriting GREASE bookmarks. (In Ghidra, adding a
      bookmark will clobber any previous bookmark of the same type at that
      location.)
    """
    GREASE_BOOKMARK_TYPE = "GREASE"
    bookmarkManager = currentProgram.getBookmarkManager()
    bookmarkManager.setBookmark(address, GREASE_BOOKMARK_TYPE, category, note)


def ghidraAddressToGreaseOffset(address):
    """Convert a Ghidra address into a value expected by GREASE's command line,
    compensating for Ghidra's ImageBase.

    Ghidra prefers to use a nonzero ImageBase, so it will choose to load ELF
    files at some nonzero offset when it can. Sometimes Ghidra can use existing
    nonzero ELF offsets, which GREASE will also use, so no adjustment is
    necessary. In other cases, Ghidra will choose its own nonzero image base
    that other tools don't know about, so we reverse this before passing
    addresses to GREASE. We compute the address GREASE expects by subtracting
    Ghidra's image base from the address to get the offset, then add back in the
    ELF loader image base.

    Takes a Ghidra Address as input and returns a long.
    """

    return address.subtract(
        currentProgram.getImageBase()
    ) + ElfLoader.getElfOriginalImageBase(currentProgram)


def greaseOffsetToGhidraAddress(greaseLoadOffset, offset):
    """Convert an address reported by GREASE into a value expected by Ghidra.
    This function compensates for the following:

    * Ghidra's ImageBase, which needs to be added to all addresses in order to
      valid within Ghidra.

    * GREASE's load offset (`greaseLoadOffset`), a constant which GREASE adds to
      all of the addresses that it reports in its output.

    Note that this function is *not* the inverse of
    `ghidraAddressToGreaseOffset`. This is because `ghidraAddressToGreaseOffset`
    produces values intended for GREASE's command line, which are not expected
    to have a load offset. This function, on the other hand, expects addresses
    from GREASE's output, which do have load offsets applied.
    """
    return toAddr(
        offset
        + currentProgram.getImageBase().getOffset()
        - ElfLoader.getElfOriginalImageBase(currentProgram)
        - greaseLoadOffset
    )


# used for parsing grease addresses formatted like 'segment1+0x1168'
# TODO: better serialization to avoid this kind of parsing
HEX_PATTERN = re.compile(r"\b0x([a-fA-F0-9]+)\b")


def parseGreaseLocation(locationString):
    """Turn GREASE locations like '0x1234' or 'segment1+0x1234' into integers"""
    try:
        # if it's just a location
        return int(locationString, 0)
    except ValueError:
        # if it's something like 'segment1+0x1168'
        match = HEX_PATTERN.search(locationString)
        if match:
            return int(match.group(1), 16)

    raise ValueError(
        "Could not convert grease location '{}' to int".format(locationString)
    )


def indent(spaces, lines):
    return "\n".join([" " * spaces + l for l in lines.strip().split("\n")])


### PARSING GREASE RESULTS ###


class GreaseResult(object):
    """A result that can be displayed in Ghidra.

    A GreaseResult is some specific piece of information that is associated with
    a specific point in the target program. Running Grease on a single function
    may produce multiple GreaseResult items (e.g. multiple requirement
    violations).

    The purpose of this class is to encapsulate the relevant data, and
    descendent classes should provide their own renderInGhidra functionality to
    display a result inside of Ghidra (e.g. as comments, bookmarks, color,
    as appropriate).
    """

    def __init__(self, tag, location, msg, details):
        """Capture a GREASE finding.

        Args:
          tag: small string representing the category of finding
          location: the Ghidra Address for this result
          msg: single line of text explaining the finding
          details: list of single-line strings with additional details
        """
        self.tag = tag
        self.location = location
        self.msg = msg
        self.details = details if details else []

    def printToConsole(self):
        """Print a summary of the contents of this result."""
        print(
            "Finding: 0x{:x} {} {}\n{}".format(
                self.location.getOffset(),
                self.tag,
                self.msg,
                indent(4, "\n".join(self.details)),
            )
        )

    def renderInGhidra(self):
        """Override this function to show results in Ghidra.

        Typically this will include adding a Plate Comment, Bookmark, and/or
        instruction Comment at the appropriate point, use of color, etc.
        """
        raise NotImplementedError


class GreaseCompleteFunction(GreaseResult):
    """This represents that GREASE fully analyzed a function.

    That is, GREASE could fully analyze the function, including inferring an
    appropriate precondition, etc. then symbolically executed the whole thing.
    """

    def __init__(self, location):
        super(GreaseCompleteFunction, self).__init__(
            "analyzed function", location, "Analysis Completed", []
        )

    def renderInGhidra(self):
        addPlateComment(self.location, "==> GREASE: Analysis Completed. <==")
        addBookmark(
            self.location,
            "Analysis Complete",
            "Function fully analyzed",
        )
        setBackgroundColor(self.location, Color(153, 231, 255))


class GreaseFunctionConcreteState(GreaseResult):
    """A bug was found to be reachable in this function given this concrete precondition state."""

    def __init__(self, location, interestingState, details):
        """Capture function entry and interesting arguments to display

        Args:
          location: Ghidra address for the relevant function entry
          interestingState: list of strings representing interesting state and values
            to display (e.g. "rdx == 0x1234")
          details: list of single-line strings with additional info
        """
        self.interestingState = interestingState
        super(GreaseFunctionConcreteState, self).__init__(
            "concrete state", location, "Concretized Precondition", details
        )

    def renderInGhidra(self):
        addPlateComment(
            self.location,
            "--> GREASE: Bug reachable with concrete initial state:\n{}".format(
                indent(4, "\n".join(self.interestingState))
            ),
        )
        if self.details:
            addPlateComment(
                self.location,
                indent(4, "\n".join(["Additional Information:"] + self.details)),
            )
        addBookmark(
            self.location,
            "GREASE initial state",
            "Bug-triggering initial state",
        )
        setBackgroundColor(self.location, Color(255, 202, 56))


class GreaseIncompleteFunction(GreaseResult):
    """This represents that GREASE was unable to fully analyze a function

    This could be because GREASE failed with a nonzero exit, or failed to infer
    a precondition, or hit the maximum recursion limit, or hit instruction for
    which we are missing semantics, etc.

    GREASE may still have some other results to report on instructions in this
    function, but we mark this function as incomplete because there may be e.g.
    other requirement violations present that GREASE doesn't find or report
    because it couldn't fully symbolically execute this function.
    """

    def __init__(self, location, msg, details):
        super(GreaseIncompleteFunction, self).__init__(
            "Analysis Incomplete", location, msg, details
        )

    def renderInGhidra(self):
        addPlateComment(
            self.location,
            "==> GREASE: Incomplete Analysis ({}) <==\n{}".format(
                self.msg, "\n".join(self.details)
            ),
        )
        addBookmark(
            self.location,
            self.tag,
            self.msg,
        )
        setBackgroundColor(self.location, Color.LIGHT_GRAY)


class GreaseInstructionResult(GreaseResult):
    """This represents some result from GREASE that is associated with a specific code location.

    These results get displayed inline as a comment above the appropriate
    instruction in bright yellow, as well as in the bookmarks list.
    """

    def renderInGhidra(self):
        addPreComment(
            self.location, "GREASE: {}\n{}".format(self.msg, "\n".join(self.details))
        )
        setBackgroundColor(self.location, Color.YELLOW)
        addBookmark(
            self.location,
            self.tag,
            self.msg,
        )


### Parsing Specific Result JSON


def renderInterestingConcreteReg(argInfo):
    """Render nonzero register concrete contents as a string, otherwise None.

    If the content is "boring" (zero) this function returns None
    E.g. {'arg': 'rdi', 'block': 0, 'offset': 0} => None

    More interesting examples:

    E.g. {'arg': 'rsi', 'value': {'offset': 128, 'block': 0}}
         returns: "rsi == 128 (0x80)"
    and  renders {'arg': 'rsp', 'value': {'offset': 1048568, 'block': 2}}
         returns: "rsp contains a pointer (offset 0xffff8)"
    """
    if (
        "arg" in argInfo
        and argInfo["value"]  # GREASE JSON values might be false or []
        # we're looking for ones with content like { "block": 0, "offset": 0 }
        and "block" in argInfo["value"]
    ):
        regName = argInfo["arg"]
        if (
            # block of zero means bitvector (with value in offset field)
            argInfo["value"]["block"] == 0
            # all offsets "start" at 0 so only print "interesting" ones
            and argInfo["value"]["offset"] != 0
        ):
            return "{} == {} (0x{:x})".format(
                regName,
                argInfo["value"]["offset"],
                argInfo["value"]["offset"],
            )
        elif argInfo["value"]["block"] != 0:
            # block of nonzero means a pointer. additionally, print the offset if nonzero
            if argInfo["value"]["offset"] != 0:
                return "{} contains a pointer (offset 0x{:x})".format(
                    regName, argInfo["value"]["offset"]
                )
            else:
                return "{} contains a pointer".format(regName)
    return None


def parseBatchBug(fnEntryPoint, greaseLoadOffset, batchBugJSON):
    bugDesc = batchBugJSON["bugDesc"]
    tag = bugDesc["bugType"]
    details = []
    try:
        location = greaseOffsetToGhidraAddress(
            greaseLoadOffset, parseGreaseLocation(bugDesc["bugLoc"])
        )
    except ValueError:
        # if we can't determine the location, use the entry point
        location = fnEntryPoint
        details.append("In this function, at {}:".format(bugDesc["bugLoc"]))

    results = []

    # Extract bug information
    if tag == "MustFail":
        # MustFail isn't very illuminating to users, so we default "SafetyCondition"
        tag = "SafetyCondition"

        # NOTE: we're explicitly encoding utf-8 here, as contents may include non-ascii characters.
        # TODO: how to handle this more generally?
        if "bugUb" in bugDesc and bugDesc["bugUb"]:  # bugUb is nullable
            # Use a more specific bug type as the tag, if available
            tag = bugDesc["bugUb"]["ubType"]["tag"]
            details.extend(
                [
                    "Finding type: {}".format(bugDesc["bugUb"]["ubType"]["tag"]),
                    bugDesc["bugUb"]["ubExplanation"],
                ]
            )
            details.extend(bugDesc["bugUb"]["ubDetails"].encode("utf-8").split("\n"))
        else:
            details.extend(bugDesc["bugDetails"].encode("utf-8").split("\n"))

        results.extend(
            [
                GreaseCompleteFunction(fnEntryPoint),
                GreaseInstructionResult(
                    tag,
                    location,
                    "Violation of safety condition",
                    details,
                ),
            ]
        )
    else:
        # NOTE: keep in sync with Grease.Bug BugType
        if tag == "UninitStackRead":
            msg = "uninitialized stack read"
        else:
            raise NotImplementedError("Unknown bug tag {}: {}".format(tag, bugDesc))

        if bugDesc["bugDetails"]:
            details.extend(bugDesc["bugDetails"].encode("utf-8").split("\n"))
        results.extend(
            [
                GreaseCompleteFunction(fnEntryPoint),
                GreaseInstructionResult(
                    tag, location, "Possible {}".format(msg), details
                ),
            ]
        )

    # Extract concrete state, if available
    concreteRegs = []
    if "bugArgs" in batchBugJSON:
        for argInfo in batchBugJSON["bugArgs"]:
            interesting_reg = renderInterestingConcreteReg(argInfo)
            if interesting_reg:
                concreteRegs.append(interesting_reg)

    if concreteRegs:
        results.append(GreaseFunctionConcreteState(fnEntryPoint, concreteRegs, details))
    return results


def parseBatchCouldNotInfer(fnEntryPoint, greaseLoadOffset, failedPredicateJSONs):
    """Render precondition inference failures from a BatchCouldNotInfer result.

    First, this produces a function-level report that we couldn't infer a
    precondition, so our analysis could not be completed.

    Each BatchCouldNotInfer result from GREASE contain a list of FailedPredicate
    objects.

    Each FailedPredicate object includes a location, message, and a list of args
    which may or may not be populated.

    For each FailedPredicate object:

    - if the args are populated, instead add information about the bug and
      "interesting" (nonzero) concrete state for reaching it, if present

    - if the concrete args aren't populated, then include the location and
      message for the issue in the overall function result

    - if the location is something we can map to a program location (i.e.
      instruction), then also render the message at the instruction location
    """
    details_without_concrete_state = []
    results_with_concrete_state = []
    location_results = []

    for p in failedPredicateJSONs:
        location = p["_failedPredicateLocation"]  # used for rendering

        # show message at a particular instruction, if we can
        try:
            location_addr = greaseOffsetToGhidraAddress(
                greaseLoadOffset, parseGreaseLocation(p["_failedPredicateLocation"])
            )
            location = location = "0x{:x}".format(location_addr.getOffset())
            location_results.append(
                GreaseInstructionResult(
                    "InferenceFailure", location_addr, p["_failedPredicateMessage"], []
                )
            )
        except ValueError:
            pass

        # the message to be rendered at the function level
        details = [
            "Failed to infer a safe precondition avoiding issue at {}:".format(location)
        ]
        details.extend(
            indent(2, p["_failedPredicateMessage"]).split("\n"),
        )

        # render alongside concrete args if present, otherwise just the top-level result
        interestingRegs = []
        if p["_failedPredicateArgs"]:
            for argInfo in p["_failedPredicateArgs"]:
                txt = renderInterestingConcreteReg(argInfo)
                if txt:
                    interestingRegs.append(txt)
        if interestingRegs:
            results_with_concrete_state.append(
                GreaseFunctionConcreteState(fnEntryPoint, interestingRegs, details)
            )
        else:
            details_without_concrete_state.extend(details)

    results = [
        GreaseIncompleteFunction(
            fn.getEntryPoint(),
            "Unable to infer suitable precondition",
            details_without_concrete_state,
        )
    ]
    results.extend(results_with_concrete_state)
    results.extend(location_results)
    return results


def parseBatchChecks(fnEntryPoint, greaseLoadOffset, mapReqStatusJSON):
    results = [GreaseCompleteFunction(fn.getEntryPoint())]
    for [req, reqResult] in mapReqStatusJSON:
        if reqResult["tag"] == "CheckAssertionFailure":
            for failedPredicate in reqResult["contents"]:
                msg = "Requirement {} violated: {}".format(
                    req, failedPredicate["_failedPredicateMessage"].partition("\n")[-1]
                )
                details = failedPredicate["_failedPredicateMessage"].split("\n")
                try:
                    location = greaseOffsetToGhidraAddress(
                        greaseLoadOffset,
                        parseGreaseLocation(
                            failedPredicate["_failedPredicateLocation"]
                        ),
                    )
                except ValueError:
                    # if we don't have a location, just report at start of function
                    location = fnEntryPoint
                    details = [
                        "In this function, at {}:".format(
                            failedPredicate["_failedPredicateLocation"]
                        )
                    ] + details
                results.append(GreaseInstructionResult(req, location, msg, details))
    return results


def parseBatchCantRefine(fnEntryPoint, greaseLoadOffset, cantRefineJSON):
    assert cantRefineJSON["tag"] in ["MissingFunc", "MissingSemantics", "MutableGlobal"]
    return [
        GreaseIncompleteFunction(
            fnEntryPoint,
            "{}: {}".format(cantRefineJSON["tag"], cantRefineJSON["contents"]),
            None,
        )
    ]


def parseGreaseResults(fnEntryPoint, greaseLoadOffset, batchStatusJSON):
    """Return a list of GreaseFunctionResult and/or GreaseInstructionResult"""
    tag = batchStatusJSON["tag"]
    if tag == "BatchBug":
        return parseBatchBug(
            fnEntryPoint, greaseLoadOffset, batchStatusJSON["contents"]
        )
    elif tag == "BatchCouldNotInfer":
        return parseBatchCouldNotInfer(
            fnEntryPoint, greaseLoadOffset, batchStatusJSON["contents"]
        )
    elif tag == "BatchItersExceeded":
        return [GreaseIncompleteFunction(fnEntryPoint, "Iteration count exceeded", [])]
    elif tag == "BatchResourceExhausted":
        return [
            GreaseIncompleteFunction(
                fnEntryPoint,
                "Resource exhausted",
                batchStatusJSON["contents"][1].split("\n"),
            )
        ]
    elif tag == "BatchChecks":
        return parseBatchChecks(
            fnEntryPoint, greaseLoadOffset, batchStatusJSON["contents"]
        )
    elif tag == "BatchCantRefine":
        return parseBatchCantRefine(
            fnEntryPoint, greaseLoadOffset, batchStatusJSON["contents"]
        )
    elif tag == "BatchTimeout":
        return [GreaseIncompleteFunction(fnEntryPoint, "Analysis timed out", [])]
    raise GreaseParseError("Failed to parse GREASE results: {}".format(batchStatusJSON))


### EXECUTING GREASE ###


def getGreaseFunctionStart(fn):
    """Turn a Ghidra Function to an entry point address for GREASE.

    Note we also use Ghidra to identify functions in ARM THUMB mode, and in
    these cases set the lowest address bit so GREASE starts in THUMB mode.
    """
    greaseFunctionStart = ghidraAddressToGreaseOffset(fn.getEntryPoint())

    # if this is an ARM function that starts in THUMB mode, set the low address bit
    entryAddr = fn.getEntryPoint()
    firstInstruction = getInstructionContaining(entryAddr)
    tmodeReg = firstInstruction.processorContext.getRegister("TMode")
    if tmodeReg and firstInstruction.processorContext.getValue(tmodeReg, False) == 1:
        greaseFunctionStart = greaseFunctionStart | 1

    return greaseFunctionStart


def getGhidraInitialPrecondition(fn):
    """Try to produce an initial precondition shape file based on Ghidra's type info.

    Returns the filename for the shape file, otherwise None.

    If Ghidra knows (thinks) a function has arguments with parameters that
    (through some level of indirection) point to "large" (>8 byte) regions of
    known size, create an initial precondition shape file for the relevant
    registers and return the filename for that shape file. The caller is
    responsible for subsequently deleting the temporary file created here.

    Otherwise, return None, indicating no initial precondition shapes file was
    created.
    """
    initial_preconditions = []  # [("reg_name", indirection_level, size)]

    # Look for interesting precondition data
    for p in range(fn.getParameterCount()):
        indirection_level = 0
        size = 0
        current_datatype = fn.getParameter(p).getDataType()
        while True:
            try:
                size = current_datatype.getLength()
                if isinstance(current_datatype, ghidra.program.model.data.Pointer):
                    indirection_level += 1
                current_datatype = current_datatype.getDataType()
            except AttributeError:
                break
        if indirection_level > 0 and size > 8:
            initial_preconditions.append(
                (
                    fn.getParameter(p).getRegister().getName().lower(),
                    indirection_level,
                    size,
                )
            )

    if not initial_preconditions:
        print("No interesting precondition data identified for '{}".format(fn))
        return None

    print(
        "Interesting precondition data identified for '{}': {}".format(
            fn,
            ", ".join(
                [
                    r + "=" + i * "*" + "(" + str(sz) + ")"
                    for (r, i, sz) in initial_preconditions
                ]
            ),
        )
    )

    # Prepare initial preconditions file content
    register_block = []
    memory_conditions = []
    next_block_id = 0

    for reg_name, indirection_level, size in initial_preconditions:
        # Registers must come first in shapes file
        register_block.append("{}: {:02}+00".format(reg_name, next_block_id))
        indirection_level -= 1

        # Create pointer-to-pointer blocks, if any.
        while indirection_level > 0:
            memory_conditions.append(
                "{:02}: {:02}+00".format(next_block_id, next_block_id + 1)
            )
            next_block_id += 1
            indirection_level -= 1

        # Create the block for the pointed-to symbolic data of known size.
        #
        # '##' means an uninitialized byte.
        memory_conditions.append(
            "{:02}: {}".format(next_block_id, " ".join(["##" for _ in range(size)]))
        )
        next_block_id += 1

    precondition_file_content = (
        "\n".join(register_block) + "\n\n" + "\n".join(memory_conditions)
    )

    # Create initial preconditions file on disk & return filename
    initial_precondition_file = tempfile.NamedTemporaryFile(delete=False)
    print(
        "Generating initial precondition file '{}' containing:\n{}".format(
            initial_precondition_file.name, precondition_file_content
        )
    )
    initial_precondition_file.write(precondition_file_content)
    initial_precondition_file.close()
    return initial_precondition_file.name


def executeGrease(
    greaseBin,
    requirements,
    greaseOverrides,
    filename,
    isRust,
    greaseFunctionStart,
    initialPreconditionFile,
):
    """Invoke GREASE and return (returncode, stdout, stderr)."""
    args = [
        greaseBin,
        filename,
        "--address",
        "0x{:x}".format(greaseFunctionStart),
        "--json",
        "--iters",
        str(MAX_ITERS),
        "--loop-bound",
        str(LOOP_BOUND),
        "--timeout",
        str(TIMEOUT_MS),
    ]
    for r in requirements:
        args.extend(["--req", r])
    for override in greaseOverrides:
        args.extend(["--overrides", override])
    if isRust:
        args.append("--rust")
    if initialPreconditionFile:
        args.extend(["--initial-precondition", initialPreconditionFile])
    print("Invoking GREASE: {}".format(args))
    p = subprocess.Popen(
        args,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    (stdout, stderr) = p.communicate()
    return (p.returncode, stdout, stderr)


def runGrease(greaseBin, requirements, greaseOverrides, filename, isRust, fn):
    """Invoke GREASE on the binary+function, return list of GreaseResults."""
    greaseFunctionStart = getGreaseFunctionStart(fn)
    initialPreconditionFile = getGhidraInitialPrecondition(fn)
    (returncode, stdout, stderr) = executeGrease(
        greaseBin,
        requirements,
        greaseOverrides,
        filename,
        isRust,
        greaseFunctionStart,
        initialPreconditionFile,
    )
    if initialPreconditionFile:
        os.remove(initialPreconditionFile)
    if returncode != 0:
        return [
            GreaseIncompleteFunction(
                fn.getEntryPoint(), "Nonzero exit", stderr.split("\n")
            )
        ]
    outputJSON = json.loads(stdout)
    loadOffset = outputJSON["batchLoadOffset"]
    batchJSON = outputJSON["batchStatus"]
    return parseGreaseResults(fn.getEntryPoint(), loadOffset, batchJSON)


### SCRIPT ENTRY POINT ###


def userSelectedFunctions():
    """Ask the user for a list of Functions to analyze"""

    # If there is a current function, ask if the user just wants that
    fn = getFunctionContaining(currentAddress)
    if fn and not fn.isThunk() and not fn.isExternal():
        c = askChoice("GREASE", "Function(s) to analyze", [fn, "Choose from list"], fn)
        if c == fn:
            return [fn]

    # Ask the user to choose from the list
    analyzableFunctions = []
    for fn in currentProgram.getFunctionManager().getFunctions(True):
        if fn.isThunk():
            print("==> GREASE excluding thunk '{}' from selection".format(fn))
        elif fn.isExternal():
            print("==> GREASE excluding external '{}' from selection".format(fn))
        else:
            print("==> GREASE enabling function '{}' for selection".format(fn))
            analyzableFunctions.append(fn)

    analyzableFunctions.sort(key=lambda fn: fn.getName())

    TARGETS_PER_SELECTION = 30  # pagination for programs with lots of functions
    numberOfPages = int(
        math.ceil(float(len(analyzableFunctions)) / TARGETS_PER_SELECTION)
    )
    targetFunctions = []
    for pageNumber, subsetOfAnalyzableFunctions in [
        (
            i / TARGETS_PER_SELECTION + 1,
            analyzableFunctions[i : i + TARGETS_PER_SELECTION],
        )
        for i in range(0, len(analyzableFunctions), TARGETS_PER_SELECTION)
    ]:
        targetFunctions.extend(
            askChoices(
                "GREASE",
                "Select functions to analyze (page {} of {})".format(
                    pageNumber, numberOfPages
                ),
                subsetOfAnalyzableFunctions,
                [
                    "{}{} [0x{:x}]".format(
                        fn.getName(),
                        (
                            ""
                            if fn.getParentNamespace().isGlobal()
                            else " in {}".format(fn.getParentNamespace())
                        ),
                        fn.getEntryPoint().getOffset(),
                    )
                    for fn in subsetOfAnalyzableFunctions
                ],
            )
        )
    print(
        "==> GREASE {} of {} Functions Selected: {}".format(
            len(targetFunctions), len(analyzableFunctions), targetFunctions
        )
    )
    return targetFunctions


try:
    greaseBin = os.getenv("GHIDRA_GREASE_BIN")
    if not greaseBin:
        greaseBin = askFile("Locate your GREASE binary", "Select").getCanonicalPath()

    greaseOverrides = []
    greaseOverridesDir = os.getenv("GHIDRA_GREASE_OVERRIDES")
    if not greaseOverridesDir:
        try:
            greaseOverridesDir = askDirectory(
                "Locate directory containing GREASE overrides to use",
                "Select (Optional)",
            ).getCanonicalPath()
        except ghidra.util.exception.CancelledException:
            print("==> GREASE aborted selection of override directory")
    if greaseOverridesDir:
        for root, _, files in os.walk(greaseOverridesDir):
            for filename in files:
                greaseOverrides.append(os.path.join(root, filename))
    print("==> GREASE Override Files: {}".format(greaseOverrides))

    requirements = []
    if OPTIONAL_REQUIREMENTS:
        requirements = askChoices(
            "Select Optional Requirements",
            "Select Optional Requirements to check. (NOTE: memory safety checks are always performed)",
            [req for (req, _) in OPTIONAL_REQUIREMENTS],
            ["{}: {}".format(req, desc) for (req, desc) in OPTIONAL_REQUIREMENTS],
        )
        print("==> GREASE Optional Requirements Selected: {}".format(requirements))

    isRust = isRustProgram(currentProgram)

    for fn in userSelectedFunctions():
        print("==> GREASE analyzing function '{}'...".format(fn))
        results = runGrease(
            greaseBin,
            requirements,
            greaseOverrides,
            currentProgram.getExecutablePath(),
            isRust,
            fn,
        )
        for result in results:
            result.printToConsole()
            result.renderInGhidra()
except ghidra.util.exception.CancelledException:
    popup("GREASE configuration cancelled")
