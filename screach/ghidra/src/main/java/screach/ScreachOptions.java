package screach;

import ghidra.framework.options.ToolOptions;
import ghidra.framework.plugintool.PluginTool;
import ghidra.util.HelpLocation;

/**
 * Registers and reads the Screach tool options (Edit &rarr; Tool Options &rarr; Screach). Options
 * are read lazily at run time so that edits take effect on the next run without restarting.
 */
class ScreachOptions {

    static final String OPTIONS_NAME = "Screach";

    private static final String USE_DOCKER = "Use Docker";
    private static final String BINARY_PATH = "Binary Path";
    private static final String DOCKER_IMAGE = "Docker Image";
    private static final String DOCKER_RUN_ARGS = "Docker Run Args";
    private static final String EXTRA_ARGS = "Extra CLI Args";
    private static final String SOLVER = "Solver";
    private static final String DUMP_COVERAGE = "Dump Coverage";

    // [tag:screach_default_binary_path]
    static final String DEFAULT_BINARY_PATH = "screach";

    // [tag:screach_default_docker_image]
    static final String DEFAULT_DOCKER_IMAGE = "ghcr.io/galoisinc/screach:nightly";

    // [tag:screach_default_docker_run_args]
    static final String DEFAULT_DOCKER_RUN_ARGS = "--rm";

    // [tag:screach_default_solver]
    static final String DEFAULT_SOLVER = "yices";

    // Accepted solver names. [ref:grease_solvers]
    static final String SOLVER_OPTIONS = "bitwuzla, cvc4, cvc5, yices, z3";

    private final ToolOptions options;

    ScreachOptions(PluginTool tool) {
        options = tool.getOptions(OPTIONS_NAME);
        // [ref:screach_help_options]
        HelpLocation help = new HelpLocation(ScreachPlugin.HELP_TOPIC, "options");

        options.registerOption(
                USE_DOCKER,
                Boolean.FALSE,
                help,
                "Run screach via a Docker image instead of a local binary.");
        options.registerOption(
                BINARY_PATH,
                DEFAULT_BINARY_PATH,
                help,
                "Path to the screach executable (used when 'Use Docker' is off).");
        options.registerOption(
                DOCKER_IMAGE,
                DEFAULT_DOCKER_IMAGE,
                help,
                "Docker image name/tag (used when 'Use Docker' is on).");
        options.registerOption(
                DOCKER_RUN_ARGS,
                DEFAULT_DOCKER_RUN_ARGS,
                help,
                "Extra arguments inserted after 'docker run' (e.g. --rm). The binary's "
                        + "directory is automatically mounted and set as the working directory.");
        options.registerOption(
                EXTRA_ARGS,
                "",
                help,
                "Extra arguments appended to every screach invocation (whitespace separated, "
                        + "simple double-quoting supported).");
        options.registerOption(
                SOLVER,
                DEFAULT_SOLVER,
                help,
                "Value passed to screach's --solver option (" + SOLVER_OPTIONS + ").");
        options.registerOption(
                DUMP_COVERAGE,
                Boolean.TRUE,
                help,
                "Pass --dump-coverage so a .greasecov file is produced for grease-cartographer.");
    }

    boolean isUseDocker() {
        return options.getBoolean(USE_DOCKER, false);
    }

    String getBinaryPath() {
        return options.getString(BINARY_PATH, DEFAULT_BINARY_PATH);
    }

    String getDockerImage() {
        return options.getString(DOCKER_IMAGE, DEFAULT_DOCKER_IMAGE);
    }

    String getDockerRunArgs() {
        return options.getString(DOCKER_RUN_ARGS, DEFAULT_DOCKER_RUN_ARGS);
    }

    String getExtraArgs() {
        return options.getString(EXTRA_ARGS, "");
    }

    String getSolver() {
        return options.getString(SOLVER, DEFAULT_SOLVER);
    }

    boolean isDumpCoverage() {
        return options.getBoolean(DUMP_COVERAGE, true);
    }
}
