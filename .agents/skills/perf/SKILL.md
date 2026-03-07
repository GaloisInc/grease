---
name: perf
description: Improve performance. Use when asked to profile or improve performance.
---

Purpose:

- Perform fine-grained timing and heap profiling
- Analyze hot spots and identify possible improvements

For each improvement identified,

- If possible, identify an existing test or create a new test that decisively demonstrates the particular slowdown
- Before making the improvement, run the test suite and record timings for each test and overall test suite time
- If the change is in a submodule, make a new `perf` branch on that submodule before making changes
- Make the change
- Run the test suite again, recording per-test time and overall time
- If the change resulted in the expected improvement, commit the change
- The commit message should include a brief overview of the salient parts of the test-suite time analysis
