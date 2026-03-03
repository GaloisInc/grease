"""Generate tab separated callgraphs for tests in directed-test-data"""

import pyghidra

pyghidra.start()

import ghidra  # noqa: F401, E402
from ghidra.program.flatapi import FlatProgramAPI  # noqa: E402
from ghidra.program.model.listing import Function, Instruction  # noqa: E402
from ghidra.app.util.opinion import ElfLoader  # noqa: E402
from ghidra.program.model.address import Address  # noqa: E402
from ghidra.program.model.listing import Program  # noqa: E402

import argparse  # noqa: E402
import os  # noqa: E402

import typing  # noqa: E402
from functools import partial  # noqa: E402


# This function does not succeed for
# pie libraries where Macaw will load them at a
# 0x10000 offset
def ghidraAddressToScreachOffset(prog: Program, gaddr: Address) -> str:
    offset = gaddr.subtract(prog.getImageBase()) + ElfLoader.getElfOriginalImageBase(
        prog
    )
    return f"0x{offset:x}"


def main():
    prs = argparse.ArgumentParser(description=__doc__)
    prs.add_argument("target_bin", type=str)
    prs.add_argument("output", type=str)
    prs.add_argument("--entrypoint", type=str)
    args = prs.parse_args()
    try:
        with open(args.output, "w") as f:
            with pyghidra.open_program(args.target_bin) as flat_api:
                fapi = typing.cast(FlatProgramAPI, flat_api)
                funcs = fapi.getGlobalFunctions(args.entrypoint)
                if len(funcs) == 0:
                    raise ValueError(f"Entrypoint: {args.entrypoint} not found")

                addrConv = partial(
                    ghidraAddressToScreachOffset, fapi.getCurrentProgram()
                )
                stack = list(funcs)
                seen_functions = set()
                while len(stack) != 0:
                    curr = typing.cast(Function, stack.pop())
                    seen_functions.add(curr)
                    insns = (
                        fapi.getCurrentProgram()
                        .getListing()
                        .getInstructions(curr.getBody(), True)
                    )
                    for insn in insns:
                        for ref in typing.cast(Instruction, insn).getReferencesFrom():
                            if ref.getReferenceType().isCall():
                                ent = curr.getEntryPoint()
                                froma = ref.getFromAddress()
                                toa = ref.getToAddress()
                                print(
                                    f"{addrConv(ent)}\t{addrConv(froma)}\t{addrConv(toa)}",
                                    file=f,
                                )
                                called = fapi.getFunctionAt(ref.getToAddress())
                                if called is not None and called not in seen_functions:
                                    seen_functions.add(curr)
                                    stack.append(called)
    except Exception as e:
        os.remove(args.output)
        raise e


if __name__ == "__main__":
    main()
