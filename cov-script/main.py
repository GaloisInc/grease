import argparse
import pyghidra

pyghidra.start()

import ghidra
from ghidra.app.util.headless import HeadlessAnalyzer
from ghidra.program.flatapi import FlatProgramAPI
from ghidra.base.project import GhidraProject
from java.lang import String
import json
import typing
from dataclasses import dataclass
from pathlib import Path

EXEC = "Executing instruction at"


@dataclass
class AddrAdjustment:
    lb: int
    ub: int
    newbase: int

    def apply_to(self, addr: int) -> typing.Optional[int]:
        if addr < self.ub and addr >= self.lb:
            return self.newbase + (addr - self.lb)
        return None


def main():
    prs = argparse.ArgumentParser()
    prs.add_argument("file", type=argparse.FileType("r"))
    prs.add_argument("--addrmap", required=True, type=argparse.FileType("r"))
    prs.add_argument("--output", required=True, type=Path)
    prs.add_argument("target_bin", type=str)
    args = prs.parse_args()
    addrs = set()

    amap = json.load(args.addrmap)

    address_adjusts = []
    for rec in amap:
        address_adjusts.append(
            AddrAdjustment(rec["lower_bound"], rec["upper_bound"], rec["base"])
        )

    for line in args.file.readlines():
        if EXEC in line:
            trim_start = line[line.find(EXEC) + len(EXEC) + 1 :]
            taddr = int(trim_start.split(":")[0], base=16)
            for adj in address_adjusts:
                r = adj.apply_to(taddr)
                if r is not None:
                    print(line)
                    addrs.add(r)
                    print(hex(r))

    with open(args.output, "w") as f:
        print("EZCOV VERSION: 1", file=f)
        with pyghidra.open_program(args.target_bin) as flat_api:
            fapi = typing.cast(FlatProgramAPI, flat_api)
            for addr in addrs:
                gAddr = fapi.getAddressFactory().getAddress(hex(addr))
                insn = fapi.getInstructionContaining(gAddr)

                if insn is not None:
                    sz = insn.getParsedLength()
                    print(f"0x{addr:x},        {sz}, [  ]", file=f)
                else: 
                    print(f"Warning 0x{addr:x}")


if __name__ == "__main__":
    main()
