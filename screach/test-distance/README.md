# Distance tests

These tests test the shortest distance computation in isolation by checking that some path that starts at longer_path reaches the function
target through a longer shortest distance than shortest path.

A test is composed of a `<NAME>.c` file, a `<NAME>.exe` file, and a `<NAME>-cg.csv` file. The csv defines the callgraph in the following format:

```
Caller function address\tCaller instruction address\tCallee function address
```

These callgraphs were constructed by hand using objdump output.
