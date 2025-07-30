# GREASE Ghidra Batch Plugin

This [Ghidra](https://github.com/NationalSecurityAgency/ghidra) plugin adds an analysis called "GREASE Analysis" which runs GREASE on all functions in a binary The order the functions are analyzed in is given by a reverse topological sort of the callgraph.

Installing this plugin requires building native GREASE binary which requires cabal (can be installed from [ghcup](https://www.haskell.org/ghcup/)).

Follow the GREASE README to build GREASE (should only require updating submodules via `git submodule update --init` then running `cabal build grease-cli`)

After building GREASE one can use gradle to build the plugin zip file by:
1. Adding the Ghidra install directory to `gradle.properties` and uncommenting the line
2. Running `./gradlew buildExtension`
3. Optionally the [gradle](https://gradle.org/) file can install the extension directly with `./gradlew installExtension`

After building the extension a zip file will be in `./dist/ghidra_<VERSION>_<DATE>_ghidra-batch-plugin.zip` which can be installed using Ghidra Project menu:
1. Open File->Install Extensions
2. Press the + button and find the zip file in file selector 
3. Click the checkbox next to ghidra-plugin to enable the plugin
4. Restart Ghidra

Please see the GREASE topic in the Ghidra help menu for usage information.