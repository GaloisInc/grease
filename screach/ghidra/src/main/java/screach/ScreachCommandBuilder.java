package screach;

import java.util.ArrayList;
import java.util.List;

/** Builds the {@link ProcessBuilder} argument list for a Screach run. */
final class ScreachCommandBuilder {

    private ScreachCommandBuilder() {}

    /**
     * Build the command line.
     *
     * @param options the configured tool options
     * @param programPath absolute path to the binary being analyzed
     * @param entryAddr formatted entry address (e.g. {@code 0x1234})
     * @param targetAddr formatted target address
     * @param avoidAddrs formatted avoid addresses
     * @param coverageFile absolute path for {@code --dump-coverage}, or {@code null} to disable
     * @return the argument list for {@link ProcessBuilder}
     */
    static List<String> build(
            ScreachOptions options,
            String programPath,
            String entryAddr,
            String targetAddr,
            List<String> avoidAddrs,
            String coverageFile) {

        List<String> screachArgs = new ArrayList<>();
        screachArgs.add(programPath);
        screachArgs.add("--entry-addr");
        screachArgs.add(entryAddr);
        screachArgs.add("--target-addr");
        screachArgs.add(targetAddr);
        for (String avoid : avoidAddrs) {
            screachArgs.add("--avoid-addr");
            screachArgs.add(avoid);
        }
        String solver = options.getSolver();
        if (solver != null && !solver.isBlank()) {
            screachArgs.add("--solver");
            screachArgs.add(solver.trim());
        }
        screachArgs.addAll(tokenize(options.getExtraArgs()));
        if (coverageFile != null) {
            screachArgs.add("--dump-coverage");
            screachArgs.add(coverageFile);
        }

        List<String> command = new ArrayList<>();
        if (options.isUseDocker()) {
            command.add("docker");
            command.add("run");
            command.addAll(tokenize(options.getDockerRunArgs()));
            String pwd = parentDir(programPath);
            command.add("-v");
            command.add(pwd + ":" + pwd);
            command.add("-w");
            command.add(pwd);
            command.add(options.getDockerImage());
        } else {
            command.add(options.getBinaryPath());
        }
        command.addAll(screachArgs);
        return command;
    }

    /** Directory containing {@code programPath}; used as the docker mount/workdir. */
    static String parentDir(String programPath) {
        java.io.File f = new java.io.File(programPath);
        java.io.File parent = f.getAbsoluteFile().getParentFile();
        return parent != null ? parent.getAbsolutePath() : ".";
    }

    /**
     * Split a free-form argument string into tokens on whitespace, honoring simple double quotes
     * (e.g. {@code --foo "a b"}). Returns an empty list for blank input. Complex shell quoting is
     * intentionally not supported.
     */
    static List<String> tokenize(String args) {
        List<String> tokens = new ArrayList<>();
        if (args == null || args.isBlank()) {
            return tokens;
        }
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;
        boolean hasToken = false;
        for (int i = 0; i < args.length(); i++) {
            char c = args.charAt(i);
            if (c == '"') {
                inQuotes = !inQuotes;
                hasToken = true;
            } else if (Character.isWhitespace(c) && !inQuotes) {
                if (hasToken) {
                    tokens.add(current.toString());
                    current.setLength(0);
                    hasToken = false;
                }
            } else {
                current.append(c);
                hasToken = true;
            }
        }
        if (hasToken) {
            tokens.add(current.toString());
        }
        return tokens;
    }
}
