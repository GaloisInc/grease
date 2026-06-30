package screach;

import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Program;
import ghidra.util.task.Task;
import ghidra.util.task.TaskMonitor;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * Runs Screach as an external process off the Swing thread, streaming its stdout/stderr into the
 * {@link ScreachProvider} and remaining cancellable via the {@link TaskMonitor}.
 */
class ScreachRunnerTask extends Task {

    private final ScreachPlugin plugin;
    private final Program program;
    private final ScreachState state;

    ScreachRunnerTask(ScreachPlugin plugin, Program program, ScreachState state) {
        super("Run Screach", true /* canCancel */, false /* hasProgress */, false /* isModal */);
        this.plugin = plugin;
        this.program = program;
        this.state = state;
    }

    @Override
    public void run(TaskMonitor monitor) {
        ScreachProvider provider = plugin.getProvider();
        ScreachOptions options = plugin.getOptions();

        String coverageFile = null;
        if (options.isDumpCoverage()) {
            coverageFile = coverageFilePath();
        }

        List<String> command = buildCommand(options, program, state, coverageFile);

        provider.startRun(String.join(" ", command));
        monitor.setMessage("Running Screach...");

        ProcessBuilder pb = new ProcessBuilder(command);
        pb.directory(new File(ScreachCommandBuilder.parentDir(program.getExecutablePath())));
        pb.redirectErrorStream(false);

        Process process;
        try {
            process = pb.start();
        } catch (IOException e) {
            String exe = options.isUseDocker() ? "docker" : options.getBinaryPath();
            provider.finishRun(
                    ScreachResult.error(
                            "Failed to launch '"
                                    + exe
                                    + "': "
                                    + e.getMessage()
                                    + ". Check Edit → Tool Options → Screach."));
            return;
        }

        // StringBuffer (not StringBuilder): each is written by a reader thread and
        // read back here on the task thread once the readers are joined.
        StringBuffer stdout = new StringBuffer();
        StringBuffer stderr = new StringBuffer();
        Thread outThread = streamReader(process.getInputStream(), stdout, provider, false);
        Thread errThread = streamReader(process.getErrorStream(), stderr, provider, true);

        int exitCode;
        try {
            while (process.isAlive()) {
                if (monitor.isCancelled()) {
                    process.destroyForcibly();
                    joinQuietly(outThread);
                    joinQuietly(errThread);
                    provider.finishRun(ScreachResult.cancelled());
                    return;
                }
                Thread.sleep(100);
            }
            exitCode = process.exitValue();
            joinQuietly(outThread);
            joinQuietly(errThread);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            process.destroyForcibly();
            provider.finishRun(ScreachResult.cancelled());
            return;
        }

        String covFile = coverageFile;
        if (covFile != null && !new File(covFile).isFile()) {
            provider.appendStderr(
                    "[screach] Warning: expected coverage file was not written: " + covFile + "\n");
            covFile = null;
        }

        ScreachResult result =
                ScreachResult.fromRun(exitCode, stdout.toString(), stderr.toString(), covFile);
        provider.finishRun(result);
    }

    static List<String> buildCommand(
            ScreachOptions options, Program program, ScreachState state, String coverageFile) {
        String programPath = program.getExecutablePath();
        String entryAddr = AddressTranslator.formatForCli(program, state.getEntry());
        String targetAddr = AddressTranslator.formatForCli(program, state.getTarget());
        List<String> avoidAddrs = new ArrayList<>();
        for (Address addr : state.getAvoid()) {
            avoidAddrs.add(AddressTranslator.formatForCli(program, addr));
        }
        return ScreachCommandBuilder.build(
                options, programPath, entryAddr, targetAddr, avoidAddrs, coverageFile);
    }

    /** Build a temp coverage file next to the binary so Docker can write it too. */
    private String coverageFilePath() {
        String programPath = program.getExecutablePath();
        File dir = new File(ScreachCommandBuilder.parentDir(programPath));
        String base = new File(programPath).getName();
        return new File(dir, base + ".greasecov").getAbsolutePath();
    }

    private Thread streamReader(
            InputStream in, StringBuffer sink, ScreachProvider provider, boolean isStderr) {
        Thread t =
                new Thread(
                        () -> {
                            try (BufferedReader reader =
                                    new BufferedReader(
                                            new InputStreamReader(in, StandardCharsets.UTF_8))) {
                                String line;
                                while ((line = reader.readLine()) != null) {
                                    sink.append(line).append('\n');
                                    if (isStderr) {
                                        provider.appendStderr(line + "\n");
                                    } else {
                                        provider.appendStdout(line + "\n");
                                    }
                                }
                            } catch (IOException e) {
                                // Stream closed (process exited); nothing to do.
                            }
                        },
                        "screach-" + (isStderr ? "stderr" : "stdout"));
        t.setDaemon(true);
        t.start();
        return t;
    }

    /**
     * Wait for a stream-reader thread to finish. Called only after the process has exited (or been
     * destroyed), so the pipe is at (or heading to) EOF and the reader returns promptly. We join
     * without a timeout so that {@code stdout}/ {@code stderr} are fully drained before they are
     * read back and classified; a short timeout here risks reading a buffer that is missing the
     * trailing "reached target" line and misclassifying a successful run.
     */
    private static void joinQuietly(Thread t) {
        try {
            t.join();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
