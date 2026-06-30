package screach;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeNotNull;

import ecfs.EcfsElfExtension;

import generic.jar.ResourceFile;
import generic.test.AbstractGTest;

import ghidra.GhidraTestApplicationLayout;
import ghidra.base.project.GhidraProject;
import ghidra.framework.GModule;
import ghidra.framework.options.ToolOptions;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Program;
import ghidra.program.model.symbol.Symbol;
import ghidra.program.model.symbol.SymbolTable;
import ghidra.test.AbstractGhidraHeadlessIntegrationTest;
import ghidra.test.TestEnv;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import utility.application.ApplicationLayout;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Integration test that loads the checked-in ECFS snapshot through both the ghidra-ecfs extension
 * and the screach-ghidra plugin together.
 *
 * <p>It resolves {@code main} and {@code add_numbers} from Ghidra's symbol table, sets them on
 * {@link ScreachState} exactly as {@link RunScreachAction} does, builds the command via {@link
 * ScreachRunnerTask#buildCommand}, then spawns the real {@code screach} binary and asserts that the
 * target ({@code add_numbers}) is reported as reached.
 *
 * <p>The test fails when the screach binary cannot be found. Set {@code -Dscreach.test.binaryPath}
 * to an absolute path, or ensure {@code screach} is on {@code PATH}.
 */
public class ScreachEcfsIntegrationTest extends AbstractGhidraHeadlessIntegrationTest {

    private static final String FIXTURE_PROP = "screach.test.ecfsFixture";
    private static final String BINARY_PROP = "screach.test.binaryPath";

    private TestEnv env;
    private GhidraProject project;

    /**
     * Register both the ghidra-ecfs module directory and the screach-ghidra module directory so
     * that Ghidra discovers the {@link EcfsElfExtension} extension point during import.
     */
    @Override
    protected ApplicationLayout createApplicationLayout() throws java.io.IOException {
        return new GhidraTestApplicationLayout(new File(AbstractGTest.getTestDirectoryPath())) {
            @Override
            public Map<String, GModule> findGhidraModules() throws java.io.IOException {
                Map<String, GModule> modules = new HashMap<>(super.findGhidraModules());

                // screach-ghidra module (this project)
                File screachDir = new File("").getAbsoluteFile();
                modules.put(
                        screachDir.getName(),
                        new GModule(applicationRootDirs, new ResourceFile(screachDir)));

                // ghidra-ecfs module (composite-build sibling)
                File ecfsDir = screachDir.toPath().resolve("../../ghidra-ecfs").toFile();
                if (ecfsDir.isDirectory()) {
                    modules.put(
                            ecfsDir.getName(),
                            new GModule(applicationRootDirs, new ResourceFile(ecfsDir)));
                }

                return Collections.unmodifiableMap(modules);
            }
        };
    }

    @Before
    public void setUp() throws Exception {
        env = new TestEnv();
        project = env.getGhidraProject();
    }

    @After
    public void tearDown() {
        if (env != null) {
            env.dispose();
            env = null;
            project = null;
        }
    }

    /**
     * Loads the ECFS snapshot, locates {@code main} and {@code add_numbers} in Ghidra's symbol
     * table, sets them on {@link ScreachState} as the extension would, builds the command via
     * {@link ScreachRunnerTask#buildCommand}, runs screach, and asserts that {@code add_numbers} is
     * reached from {@code main}.
     */
    @Test
    public void screachReachesAddNumbersFromMain() throws Exception {
        File fixture = fixtureFile();
        assumeNotNull("ECFS fixture not found; set -D" + FIXTURE_PROP, fixture);

        String binaryPath = screachBinaryPath();
        assertTrue(
                "screach binary not found at '" + binaryPath + "'; set -D" + BINARY_PROP,
                isBinaryExecutable(binaryPath));

        Program program = project.importProgram(fixture);
        assertNotNull("the ECFS snapshot should import", program);

        Address mainAddr = resolveGlobalSymbol(program, "main");
        assertNotNull("expected 'main' symbol after ECFS import", mainAddr);

        Address addNumbersAddr = resolveAddNumbers(program);
        assertNotNull("expected 'add_numbers' symbol after ECFS import", addNumbersAddr);

        ScreachState state = new ScreachState();
        state.setEntry(mainAddr);
        state.setTarget(addNumbersAddr);

        ToolOptions toolOptions = new ToolOptions(ScreachOptions.OPTIONS_NAME);
        ScreachOptions options = new ScreachOptions(toolOptions);
        toolOptions.setString(ScreachOptions.BINARY_PATH, binaryPath);

        List<String> command = ScreachRunnerTask.buildCommand(options, program, state, null);

        ScreachResult result = runCommand(command, fixture.getParentFile());

        assertEquals(
                "screach should report add_numbers as reached from main. Summary: "
                        + result.summary,
                ScreachResult.Status.REACHED,
                result.status);
    }

    // ----- Helpers -------------------------------------------------------------------------------

    private File fixtureFile() {
        String path = System.getProperty(FIXTURE_PROP);
        if (path == null) {
            return null;
        }
        File f = new File(path);
        return f.isFile() ? f : null;
    }

    private String screachBinaryPath() {
        return System.getProperty(BINARY_PROP, "screach");
    }

    private static boolean isBinaryExecutable(String binaryPath) {
        File f = new File(binaryPath);
        if (f.isAbsolute()) {
            return f.isFile() && f.canExecute();
        }
        // PATH lookup: try running with --help; success means it's present.
        try {
            Process p = new ProcessBuilder(binaryPath, "--help").start();
            p.waitFor();
            return true;
        } catch (IOException | InterruptedException e) {
            return false;
        }
    }

    /**
     * Look up a symbol by name in the global namespace. Returns the symbol's address, or {@code
     * null} if the symbol is not found or has address zero.
     */
    private static Address resolveGlobalSymbol(Program program, String name) {
        SymbolTable symbolTable = program.getSymbolTable();
        for (Symbol symbol : symbolTable.getSymbols(name)) {
            if (symbol.getParentNamespace().isGlobal()) {
                Address addr = symbol.getAddress();
                if (addr.getOffset() != 0) {
                    return addr;
                }
            }
        }
        return null;
    }

    /**
     * Resolve {@code add_numbers} from the program symbol table. Searches all namespaces — the
     * stock ELF loader may place it in the global namespace, or {@link EcfsElfExtension} may place
     * it under an {@code ECFS::Symbols::<lib>} namespace.
     */
    private static Address resolveAddNumbers(Program program) {
        SymbolTable symbolTable = program.getSymbolTable();
        for (Symbol sym : symbolTable.getSymbols("add_numbers")) {
            Address addr = sym.getAddress();
            if (addr.getOffset() != 0) {
                return addr;
            }
        }
        return null;
    }

    /**
     * Spawn the process described by {@code command} and return the classified result, mirroring
     * what {@link ScreachRunnerTask} does (but synchronously and without Ghidra task machinery).
     */
    private static ScreachResult runCommand(List<String> command, File workDir)
            throws IOException, InterruptedException {

        ProcessBuilder pb = new ProcessBuilder(command);
        pb.directory(workDir);
        pb.redirectErrorStream(false);
        Process process = pb.start();

        StringBuffer stdout = new StringBuffer();
        StringBuffer stderr = new StringBuffer();
        Thread outThread = drainStream(process.getInputStream(), stdout);
        Thread errThread = drainStream(process.getErrorStream(), stderr);

        int exitCode = process.waitFor();
        outThread.join();
        errThread.join();

        return ScreachResult.fromRun(exitCode, stdout.toString(), stderr.toString(), null);
    }

    private static Thread drainStream(java.io.InputStream in, StringBuffer sink) {
        Thread t =
                new Thread(
                        () -> {
                            try (BufferedReader reader =
                                    new BufferedReader(
                                            new InputStreamReader(in, StandardCharsets.UTF_8))) {
                                String line;
                                while ((line = reader.readLine()) != null) {
                                    sink.append(line).append('\n');
                                }
                            } catch (IOException e) {
                                // stream closed
                            }
                        });
        t.setDaemon(true);
        t.start();
        return t;
    }
}
