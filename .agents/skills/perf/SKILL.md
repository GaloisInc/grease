---
name: perf
description: Improve performance. Use when asked to profile or improve performance.
---

Purpose:

- Perform fine-grained timing and heap profiling
- Analyze hot spots and identify possible improvements

## Setup

Enable profiling in `cabal.project.local`:

```
profiling: True
profiling-detail: late
package *
  profiling: True
  ghc-options: -fprof-late
```

Then rebuild:

```bash
cabal build pkg:grease-exe
```

This takes a long time (30+ minutes) for a fresh build with profiling. Run in background.

## Commands

### Automated Profiling

Use the comprehensive profiling script for detailed analysis:

```bash
# Run all profiling passes (RTS, valgrind, perf, heap, cost-center)
.agents/skills/perf/profile-grease.py --symbol <symbol> <path-to-elf>

# Example
.agents/skills/perf/profile-grease.py --symbol test grease-exe/tests/llvm/malloc.llvm.cbl

# Output: profile-results-<timestamp>/ directory with:
#   - grease.prof (cost center report)
#   - grease.hp (heap profile with parsed statistics)
#   - callgrind.out (valgrind instruction counts)
#   - perf-stats.log (CPU performance counters)
#   - Summary printed to stdout
```

### Manual Commands

```bash
# Build (never use `cabal build all`)
cabal build pkg:grease-exe

# Run test suite with RTS statistics (baseline timing)
cabal test pkg:grease-exe --test-show-details=direct --test-options="+RTS -s"

# Profile a specific test case with cost-center profiling
rm -f grease.tix  # HPC coverage can conflict, remove stale .tix
cabal run exe:grease -- +RTS -p -RTS --symbol <symbol> <path-to-elf>
# Output: grease.prof (cost center profiling report)

# Profile with heap profiling
cabal run exe:grease -- +RTS -p -hc -RTS --symbol <symbol> <path-to-elf>
# Output: grease.hp (heap profile), convert with: hp2ps -c grease.hp
```

## Workflow

For each improvement identified, make a TODO list with the following steps:

- If possible, identify an existing test or create a new test that decisively demonstrates the particular slowdown
- Before making the improvement, run the test suite and record:

  - timings for each test
  - total test suite time
  - total test-suite allocations (RTS `-s` flag)

- Also, for a particular relevant test-case, record comprehensive metrics:

  - Use `.agents/skills/perf/profile-grease.py` to run all profiling passes
  - This captures: RTS stats, valgrind instruction counts, perf counters, heap profile, and cost centers
  - Save the profile-results directory for comparison

- If the change is in a submodule, make a new `perf` branch on that submodule before making changes
- Make the change
- Run the test suite again, recording per-test time, overall time, and heap stats (`-s`)
- Re-run the profiling script for the specific test case(s)
- Compare before/after metrics from the profile-results directories
- If the change resulted in the expected improvement, commit the change
- The commit message should include a brief overview of the improved stats

## Key files

- `grease-exe/tests/Main.hs` — test harness (Lua-based oughta framework)
- `grease/src/Grease/Refine.hs` — refinement loop (hot path)
- `grease/src/Grease/Setup.hs` — memory setup (runs every iteration)

## Notes

See `PERF.md` for detailed profiling results and identified issues.
