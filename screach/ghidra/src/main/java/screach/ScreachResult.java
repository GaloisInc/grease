package screach;

/** Outcome of a Screach run, parsed from its exit code and diagnostics. */
class ScreachResult {

    enum Status {
        REACHED,
        NOT_REACHED,
        CANCELLED,
        ERROR
    }

    final Status status;
    final int exitCode;

    /** Absolute path to the coverage file, or {@code null} if not requested/written. */
    final String coverageFile;

    /** Human-readable one-line summary. */
    final String summary;

    private ScreachResult(Status status, int exitCode, String coverageFile, String summary) {
        this.status = status;
        this.exitCode = exitCode;
        this.coverageFile = coverageFile;
        this.summary = summary;
    }

    static ScreachResult cancelled() {
        return new ScreachResult(Status.CANCELLED, -1, null, "Run cancelled.");
    }

    static ScreachResult error(String summary) {
        return new ScreachResult(Status.ERROR, -1, null, summary);
    }

    /**
     * Classify a completed run. Screach prints its diagnostics (including the "reached target"
     * message) to stderr; a zero exit corroborates success.
     */
    static ScreachResult fromRun(int exitCode, String stdout, String stderr, String coverageFile) {
        boolean reached = containsReachedTarget(stdout) || containsReachedTarget(stderr);
        String cov = coverageFile;
        if (reached) {
            return new ScreachResult(Status.REACHED, exitCode, cov, "Reached target!");
        }
        if (exitCode != 0) {
            return new ScreachResult(
                    Status.ERROR, exitCode, cov, "Screach exited with code " + exitCode + ".");
        }
        return new ScreachResult(Status.NOT_REACHED, exitCode, cov, "Target not reached.");
    }

    private static boolean containsReachedTarget(String text) {
        return text != null && text.toLowerCase().contains("reached target");
    }
}
