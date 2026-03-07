#!/usr/bin/env python3
"""Profile GREASE with multiple profiling methods and analyze results."""

import os
import re
import subprocess
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple


@dataclass
class HPStats:
    """Statistics extracted from heap profile."""
    samples: int
    peak_bytes: int
    peak_time: float
    cost_centers: Dict[str, int]  # name -> total bytes


def parse_hp_file(hp_path: Path) -> Optional[HPStats]:
    """Parse a .hp heap profile file and extract statistics.

    Format:
        JOB "grease"
        DATE "..."
        SAMPLE_UNIT "seconds"
        VALUE_UNIT "bytes"
        BEGIN_SAMPLE 0.00
        cost_center_1
        cost_center_2
        ...
        END_SAMPLE
        BEGIN_SAMPLE 0.01
        cost_center_1
        cost_center_2
        ...
        END_SAMPLE
        ...

    Actually the format is:
        BEGIN_SAMPLE timestamp
        cc_name value
        cc_name value
        ...
        END_SAMPLE

    Wait, let me reconsider. Looking at the plan again:
    - Column headers line starting with `BEGIN_SAMPLE`
    - Data lines: `<timestamp> <cost-center-1-value> <cost-center-2-value> ...`

    So it's more like:
        JOB "grease"
        DATE "..."
        SAMPLE_UNIT "seconds"
        VALUE_UNIT "bytes"
        BEGIN_SAMPLE
        cost_center_1
        cost_center_2
        ...
        END_SAMPLE
        <timestamp> <value1> <value2> ...
        <timestamp> <value1> <value2> ...

    Actually, I should look at a real .hp file format. The standard format is:
        JOB "name"
        DATE "..."
        SAMPLE_UNIT "seconds"
        VALUE_UNIT "bytes"
        BEGIN_SAMPLE 0.00
        constructor_name bytes
        constructor_name bytes
        END_SAMPLE
        BEGIN_SAMPLE 0.01
        constructor_name bytes
        constructor_name bytes
        END_SAMPLE

    Each sample is a separate BEGIN_SAMPLE/END_SAMPLE block.
    """
    if not hp_path.exists():
        return None

    try:
        with open(hp_path) as f:
            lines = [line.strip() for line in f]

        samples = []
        cost_center_totals: Dict[str, int] = {}

        i = 0
        while i < len(lines):
            line = lines[i]

            # Look for BEGIN_SAMPLE
            if line.startswith("BEGIN_SAMPLE"):
                # Extract timestamp
                match = re.match(r"BEGIN_SAMPLE\s+([\d.]+)", line)
                timestamp = float(match.group(1)) if match else 0.0

                sample_data = {}
                i += 1

                # Read cost center data until END_SAMPLE
                while i < len(lines) and not lines[i].startswith("END_SAMPLE"):
                    # Parse "(id)cost_center_name\tbytes" format
                    parts = lines[i].split('\t')
                    if len(parts) >= 2:
                        cc_full_name = parts[0]
                        # Extract the cost center name (remove ID prefix like "(8027)")
                        cc_name = re.sub(r'^\(\d+\)', '', cc_full_name)
                        try:
                            bytes_val = int(parts[1])
                            sample_data[cc_name] = sample_data.get(cc_name, 0) + bytes_val
                            cost_center_totals[cc_name] = cost_center_totals.get(cc_name, 0) + bytes_val
                        except ValueError:
                            pass
                    i += 1

                samples.append((timestamp, sample_data))

            i += 1

        if not samples:
            return None

        # Find peak heap usage
        peak_bytes = 0
        peak_time = 0.0
        for timestamp, sample_data in samples:
            total_bytes = sum(sample_data.values())
            if total_bytes > peak_bytes:
                peak_bytes = total_bytes
                peak_time = timestamp

        return HPStats(
            samples=len(samples),
            peak_bytes=peak_bytes,
            peak_time=peak_time,
            cost_centers=cost_center_totals
        )

    except Exception as e:
        print(f"Warning: Failed to parse {hp_path}: {e}", file=sys.stderr)
        return None


def log(log_file: Path, message: str):
    """Append message to log file."""
    with open(log_file, "a") as f:
        f.write(message + "\n")


def run_command(cmd: List[str], output_file: Path, log_file: Optional[Path] = None,
                stderr_file: Optional[Path] = None, check: bool = True) -> subprocess.CompletedProcess:
    """Run a command and capture output to files."""
    stderr_target = subprocess.STDOUT
    if stderr_file:
        stderr_target = open(stderr_file, "w")

    try:
        result = subprocess.run(
            cmd,
            stdout=open(output_file, "w"),
            stderr=stderr_target,
            check=check
        )
        return result
    finally:
        if stderr_file and stderr_target != subprocess.STDOUT:
            stderr_target.close()


def extract_rts_stats(stats_file: Path) -> Dict[str, str]:
    """Extract key metrics from RTS statistics output."""
    stats = {}

    if not stats_file.exists():
        return stats

    with open(stats_file) as f:
        content = f.read()

    # Extract metrics using regex
    patterns = {
        "bytes_alloc": r"([\d,]+)\s+bytes allocated in the heap",
        "bytes_copied": r"([\d,]+)\s+bytes copied during GC",
        "max_residency": r"([\d,]+)\s+bytes maximum residency",
        "total_time": r"Total\s+time\s+([\d.]+)s"
    }

    for key, pattern in patterns.items():
        match = re.search(pattern, content)
        if match:
            value = match.group(1).replace(",", "")
            stats[key] = value

    return stats


def extract_valgrind_insns(callgrind_file: Path) -> Optional[str]:
    """Extract instruction count from callgrind output."""
    if not callgrind_file.exists():
        return None

    with open(callgrind_file) as f:
        for line in f:
            if line.startswith("summary:"):
                parts = line.split()
                if len(parts) >= 2:
                    return parts[1]

    return None


def extract_perf_stats(perf_file: Path) -> Tuple[str, str, Optional[str]]:
    """Extract instructions, cycles, and IPC from perf output.

    Returns:
        (instructions, cycles, ipc)
    """
    if not perf_file.exists():
        return ("<not supported>", "<not supported>", None)

    with open(perf_file) as f:
        content = f.read()

    if "<not supported>" in content:
        return ("<not supported>", "<not supported>", None)

    instructions = None
    cycles = None

    # Parse perf output format: "  123,456  instructions"
    for line in content.split("\n"):
        if "instructions" in line and "<not" not in line:
            match = re.search(r"([\d,]+)\s+instructions", line)
            if match:
                instructions = match.group(1).replace(",", "")

        if "cycles" in line and "<not" not in line:
            match = re.search(r"([\d,]+)\s+cycles", line)
            if match:
                cycles = match.group(1).replace(",", "")

    # Calculate IPC
    ipc = None
    if instructions and cycles:
        try:
            insns_val = float(instructions)
            cycles_val = float(cycles)
            if insns_val > 0 and cycles_val > 0:
                ipc = f"{insns_val / cycles_val:.3f}"
        except ValueError:
            pass

    return (
        instructions or "<not supported>",
        cycles or "<not supported>",
        ipc
    )


def main():
    if len(sys.argv) < 2:
        print("Usage: profile-grease.py <grease-arguments>")
        print("Example: profile-grease.py --symbol test test.armv7l.elf")
        sys.exit(1)

    grease_args = sys.argv[1:]

    # Create output directory with timestamp
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_dir = Path(f"profile-results-{timestamp}")
    output_dir.mkdir(exist_ok=True)

    log_file = output_dir / "run.log"

    print(f"Profiling output: {output_dir}/")
    print(f"Progress log: {log_file}")
    print()

    log(log_file, "=== GREASE Profiling Suite ===")
    log(log_file, f"Arguments: {' '.join(grease_args)}")
    log(log_file, "")

    # Build grease-exe
    log(log_file, "[1/6] Building grease-exe...")
    build_log = output_dir / "build.log"
    subprocess.run(
        ["cabal", "build", "pkg:grease-exe"],
        stdout=open(build_log, "w"),
        stderr=subprocess.STDOUT,
        check=True
    )

    # Get binary path
    result = subprocess.run(
        ["cabal", "list-bin", "pkg:grease-exe"],
        capture_output=True,
        text=True,
        check=True
    )

    # Filter out configuration warnings
    grease_bin = None
    for line in result.stdout.split("\n"):
        if "Configuration is affected" in line or "cabal.project" in line:
            continue
        line = line.strip()
        if line and not line.startswith("Warning"):
            grease_bin = line
            break

    if not grease_bin:
        print("Error: Could not find grease-exe binary path", file=sys.stderr)
        sys.exit(1)

    log(log_file, f"Built: {grease_bin}")

    # Clean up stale profiling files
    for filename in ["grease.tix", "grease.prof", "grease.hp"]:
        try:
            Path(filename).unlink()
        except FileNotFoundError:
            pass

    # 1. Baseline run with +RTS -s (runtime statistics)
    log(log_file, "[2/6] Running with RTS statistics (+RTS -s)...")
    run_command(
        [grease_bin, "+RTS", "-s", "-RTS"] + grease_args,
        output_dir / "rts-output.log",
        stderr_file=output_dir / "rts-stats.log"
    )
    log(log_file, "Completed RTS run")

    # Extract RTS metrics
    rts_stats = extract_rts_stats(output_dir / "rts-stats.log")

    # 2. Valgrind callgrind (instruction counts)
    log(log_file, "[3/6] Running with valgrind (instruction counts)...")
    run_command(
        [
            "valgrind", "--tool=callgrind",
            f"--callgrind-out-file={output_dir / 'callgrind.out'}",
            f"--log-file={output_dir / 'valgrind.log'}",
            grease_bin
        ] + grease_args,
        output_dir / "valgrind-output.log"
    )
    log(log_file, "Completed valgrind run")

    valgrind_insns = extract_valgrind_insns(output_dir / "callgrind.out")

    # 3. Perf stat (instruction and cycle counts)
    log(log_file, "[4/6] Running with perf (instructions, cycles)...")
    run_command(
        [
            "perf", "stat", "-e", "instructions,cycles",
            "-o", str(output_dir / "perf-stats.log"),
            grease_bin
        ] + grease_args,
        output_dir / "perf-output.log",
        check=False  # perf might not be supported
    )
    log(log_file, "Completed perf run")

    perf_insns, perf_cycles, ipc = extract_perf_stats(output_dir / "perf-stats.log")

    # 4. Heap profiling (+RTS -hc -p)
    log(log_file, "[5/6] Running with heap profiling (+RTS -hc -p)...")

    # Clean up before heap profiling
    for filename in ["grease.tix", "grease.prof", "grease.hp"]:
        try:
            Path(filename).unlink()
        except FileNotFoundError:
            pass

    run_command(
        [grease_bin, "+RTS", "-hc", "-p", "-s", "-RTS"] + grease_args,
        output_dir / "heap-output.log",
        stderr_file=output_dir / "heap-stats.log"
    )

    if Path("grease.hp").exists():
        Path("grease.hp").rename(output_dir / "grease.hp")
        log(log_file, "Generated grease.hp")

    log(log_file, "Completed heap profiling run")

    # 5. Cost-center profiling (+RTS -p)
    log(log_file, "[6/6] Running with cost-center profiling (+RTS -p -s)...")

    # Clean up before cost-center profiling
    for filename in ["grease.tix", "grease.prof"]:
        try:
            Path(filename).unlink()
        except FileNotFoundError:
            pass

    run_command(
        [grease_bin, "+RTS", "-p", "-s", "-RTS"] + grease_args,
        output_dir / "prof-output.log",
        stderr_file=output_dir / "prof-stats.log"
    )

    if Path("grease.prof").exists():
        Path("grease.prof").rename(output_dir / "grease.prof")
        log(log_file, "Generated grease.prof")

    log(log_file, "Completed cost-center profiling run")
    log(log_file, "")
    log(log_file, "=== All profiling runs complete ===")

    # Parse heap profile
    hp_stats = parse_hp_file(output_dir / "grease.hp")

    # Output summary to stdout
    print("=== Profiling Results ===")
    print()
    print(f"Bytes allocated:  {rts_stats.get('bytes_alloc', 'N/A')}")
    print(f"Bytes copied (GC): {rts_stats.get('bytes_copied', 'N/A')}")
    print(f"Max residency:    {rts_stats.get('max_residency', 'N/A')} bytes")
    print(f"Total time:       {rts_stats.get('total_time', 'N/A')}s")
    print()
    print(f"Instructions (valgrind): {valgrind_insns or 'N/A'}")
    print(f"Instructions (perf):     {perf_insns}")
    print(f"Cycles (perf):           {perf_cycles}")
    if ipc:
        print(f"IPC:                     {ipc}")
    print()

    # Print heap profile statistics
    if hp_stats:
        print("Heap Profile:")
        print(f"  Samples:         {hp_stats.samples}")
        print(f"  Peak heap:       {hp_stats.peak_bytes:,} bytes")
        print(f"  Peak at:         {hp_stats.peak_time:.2f}s")

        # Sort cost centers by total bytes and show top 5
        if hp_stats.cost_centers:
            print("  Top cost centers:")
            sorted_ccs = sorted(
                hp_stats.cost_centers.items(),
                key=lambda x: x[1],
                reverse=True
            )

            total_bytes = sum(hp_stats.cost_centers.values())

            for cc_name, cc_bytes in sorted_ccs[:5]:
                percentage = (cc_bytes / total_bytes * 100) if total_bytes > 0 else 0
                print(f"    {cc_name}: {cc_bytes:,} bytes ({percentage:.1f}%)")

        print()

    print("Files:")
    print(f"  Cost centers:   {output_dir / 'grease.prof'}")
    print(f"  Heap profile:   {output_dir / 'grease.hp'}")
    print(f"  Callgrind:      {output_dir / 'callgrind.out'}")


if __name__ == "__main__":
    main()
