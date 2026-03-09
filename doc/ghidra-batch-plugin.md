# Ghidra batch plugin

This [Ghidra](https://github.com/NationalSecurityAgency/ghidra) plugin adds an analysis called "GREASE Analysis" which runs GREASE on all functions in a binary. The order the functions are analyzed in is given by a reverse topological sort of the callgraph. The goal is for the functionality in this plugin to eventually subsume [the Ghidra script](./ghidra-plugin.md), but for now this analysis pass is better for use cases involving complete analysis of a large program where parallelism is required.

## Prerequisites

You will need to build GREASE from source. See [Installation](install.md) for instructions.

## Plugin installation

After building GREASE, you can use [gradle](https://gradle.org/) to build the plugin zip file:
1. Enter the ghidra plugin directory: `cd ghidra-plugin`
2. Add the Ghidra install directory to `gradle.properties` and uncomment the line
3. Run `./gradlew buildExtension`
4. Optionally, the gradle file can install the extension directly with `./gradlew installExtension`

After building the extension, a zip file will be in `./dist/ghidra_<VERSION>_<DATE>_ghidra-batch-plugin.zip` which can be installed using the Ghidra Project menu:
1. Open `File -> Install Extensions`
2. Press the `+` button and find the zip file in the file selector
3. Click the checkbox next to ghidra-plugin to enable the plugin
4. Restart Ghidra

## Plugin usage

Please see the GREASE topic in the Ghidra help menu for usage information.
