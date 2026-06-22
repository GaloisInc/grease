package ecfs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import generic.jar.ResourceFile;
import generic.test.AbstractGTest;

import ghidra.GhidraTestApplicationLayout;
import ghidra.app.util.bin.ByteArrayProvider;
import ghidra.app.util.bin.ByteProvider;
import ghidra.app.util.bin.FileByteProvider;
import ghidra.app.util.bin.format.elf.ElfHeader;
import ghidra.app.util.bin.format.elf.ElfSectionHeader;
import ghidra.app.util.bin.format.elf.ElfSymbol;
import ghidra.app.util.bin.format.elf.ElfSymbolTable;
import ghidra.base.project.GhidraProject;
import ghidra.framework.GModule;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Bookmark;
import ghidra.program.model.listing.BookmarkType;
import ghidra.program.model.listing.CodeUnit;
import ghidra.program.model.listing.Program;
import ghidra.program.model.mem.MemoryBlock;
import ghidra.program.model.symbol.Namespace;
import ghidra.program.model.symbol.Symbol;
import ghidra.program.model.symbol.SymbolTable;
import ghidra.test.AbstractGhidraHeadlessIntegrationTest;
import ghidra.test.TestEnv;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import utility.application.ApplicationLayout;

import java.io.File;
import java.nio.file.AccessMode;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Integration tests that load the checked-in {@code ecfs/test.x64.elf} ECFS snapshot through
 * Ghidra's ELF importer and assert that {@link EcfsElfExtension} and its markup helpers apply the
 * expected fixups.
 */
public class EcfsElfExtensionTest extends AbstractGhidraHeadlessIntegrationTest {

    private TestEnv env;
    private GhidraProject project;

    /**
     * Build a test application layout that registers this module's directory, so the ECFS {@link
     * EcfsElfExtension} on the test classpath is discovered as an {@code ElfExtension} extension
     * point. Mirrors the layout used by the sibling GREASE Ghidra plugin tests.
     */
    @Override
    protected ApplicationLayout createApplicationLayout() throws java.io.IOException {
        return new GhidraTestApplicationLayout(new File(AbstractGTest.getTestDirectoryPath())) {
            @Override
            public Map<String, GModule> findGhidraModules() throws java.io.IOException {
                Map<String, GModule> modules = new HashMap<>(super.findGhidraModules());
                File projectDir = new File("").getAbsoluteFile();
                modules.put(
                        projectDir.getName(),
                        new GModule(applicationRootDirs, new ResourceFile(projectDir)));
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

    // ----- Unit tests of the static decision helpers ---------------------------------------------

    @Test
    public void recognizesEcfsMagicBytes() {
        assertArrayEquals(new byte[] {'E', 'C', 'F', 'S'}, EcfsElfExtension.ECFS_MAGIC);
        assertTrue(EcfsElfExtension.hasEcfsMagic(new byte[] {'E', 'C', 'F', 'S'}));
        assertFalse(EcfsElfExtension.hasEcfsMagic(new byte[] {'E', 'L', 'F', '!'}));
    }

    @Test
    public void sectionAllocationOverrides() {
        assertEquals(Boolean.TRUE, EcfsElfExtension.allocatedSectionOverride(".text"));
        assertEquals(Boolean.FALSE, EcfsElfExtension.allocatedSectionOverride("._TEXT"));
        assertNull(EcfsElfExtension.allocatedSectionOverride(".plt"));
    }

    @Test
    public void sectionExecutionOverrides() {
        assertEquals(Boolean.TRUE, EcfsElfExtension.executableSectionOverride(".text"));
        assertEquals(Boolean.FALSE, EcfsElfExtension.executableSectionOverride("._TEXT"));
        assertNull(EcfsElfExtension.executableSectionOverride(".got.plt"));
    }

    // ----- Magic-byte detection against the real fixture -----------------------------------------

    /** The ECFS magic bytes in the header padding mark the snapshot as something we can handle. */
    @Test
    public void recognizesEcfsFixture() throws Exception {
        EcfsElfExtension extension = new EcfsElfExtension();
        try (ByteProvider provider = new FileByteProvider(testFile(), null, AccessMode.READ)) {
            ElfHeader header = new ElfHeader(provider, msg -> {});
            header.parse();
            assertTrue(
                    "the vendored ee-ecfs fixture should be detected as ECFS",
                    extension.canHandle(header));
        }
    }

    /**
     * A plain ELF (one without the ECFS magic in its header padding) must not be claimed. We derive
     * such a file from the test snapshot by zeroing the magic bytes, leaving an otherwise-valid ELF
     * header.
     */
    @Test
    public void ignoresNonEcfsElf() throws Exception {
        byte[] bytes = java.nio.file.Files.readAllBytes(testFile().toPath());
        for (int i = 0; i < EcfsElfExtension.ECFS_MAGIC.length; i++) {
            bytes[EcfsElfExtension.EI_PAD_OFFSET + i] = 0;
        }
        try (ByteProvider provider = new ByteArrayProvider(bytes)) {
            ElfHeader header = new ElfHeader(provider, msg -> {});
            header.parse();
            assertFalse(
                    "an ELF without the ECFS magic must not be claimed",
                    new EcfsElfExtension().canHandle(header));
        }
    }

    // ----- Full-import behavior tests ------------------------------------------------------------

    /**
     * {@code .text} imports as an executable block, the synthetic {@code ._TEXT} marker is not
     * loaded as executable code, and the snapshot's shared-library text is imported too.
     */
    @Test
    public void promotesMainTextAndDemotesMarker() throws Exception {
        Program program = importTestSnapshot();

        String blockNames = blockNames(program);
        MemoryBlock textBlock = program.getMemory().getBlock(EcfsElfExtension.TEXT_SECTION);
        assertNotNull("expected main .text block after import. Blocks: " + blockNames, textBlock);
        assertTrue(
                "expected .text block to be executable. Blocks: " + blockNames,
                textBlock.isExecute());
        assertTrue(
                "expected .text block to have contents. Blocks: " + blockNames,
                textBlock.getSize() > 0);

        MemoryBlock markerBlock =
                program.getMemory().getBlock(EcfsElfExtension.TEXT_SEGMENT_MARKER);
        if (markerBlock != null) {
            // If present at all it is a non-loaded overlay, never executable code.
            assertFalse(
                    "synthetic ._TEXT block should not be executable. Blocks: " + blockNames,
                    markerBlock.isExecute());
        }

        assertNotNull(
                "expected shared-library text from the snapshot to be imported. Blocks: "
                        + blockNames,
                program.getMemory().getBlock("libmath.so.text"));
    }

    /**
     * Symbols from the ECFS-only per-library tables ({@code .dynsym.libc.so.6}, …) are organized
     * under {@code ECFS::Symbols::<table>} namespaces rather than dumped into the global namespace.
     */
    @Test
    public void importsExtraLibrarySymbolsIntoNamespaces() throws Exception {
        ImportedSymbolExpectation expectation = firstExtraImportedSymbol();
        Program program = importTestSnapshot();

        SymbolTable symbolTable = program.getSymbolTable();
        Namespace ecfsNamespace =
                symbolTable.getNamespace(
                        EcfsSymbolTableMarkup.ECFS_NAMESPACE, program.getGlobalNamespace());
        assertNotNull("expected ECFS namespace for imported snapshot symbols", ecfsNamespace);

        Namespace symbolsNamespace =
                symbolTable.getNamespace(EcfsSymbolTableMarkup.SYMBOLS_NAMESPACE, ecfsNamespace);
        assertNotNull("expected ECFS::Symbols namespace for extra symbol tables", symbolsNamespace);

        Namespace tableNamespace =
                symbolTable.getNamespace(expectation.namespaceName, symbolsNamespace);
        assertNotNull(
                "expected namespace for imported ECFS symbol table " + expectation.namespaceName,
                tableNamespace);

        Address symbolAddress = addressFor(program, expectation.symbolValue);
        Symbol symbol =
                symbolTable.getSymbol(expectation.symbolName, symbolAddress, tableNamespace);
        assertNotNull(
                "expected imported ECFS symbol " + expectation.symbolName + " at " + symbolAddress,
                symbol);
    }

    /** Thread registers from {@code .prstatus} are annotated with a comment and bookmark. */
    @Test
    public void annotatesPrStatusThreads() throws Exception {
        EcfsPrStatusMarkup.PrStatusEntry prStatusEntry = readPrStatusEntries().get(0);
        Program program = importTestSnapshot();

        Address programCounter = addressFor(program, prStatusEntry.programCounter);
        String comment = program.getListing().getComment(CodeUnit.PRE_COMMENT, programCounter);
        assertNotNull(
                "expected ECFS prstatus comment at thread program counter " + programCounter,
                comment);
        assertTrue(comment.contains("ECFS prstatus thread 1"));
        assertTrue(comment.contains(prStatusEntry.hex(prStatusEntry.programCounter)));

        Bookmark bookmark =
                program.getBookmarkManager()
                        .getBookmark(
                                programCounter,
                                BookmarkType.ANALYSIS,
                                EcfsPrStatusMarkup.BOOKMARK_CATEGORY);
        assertNotNull(
                "expected ECFS prstatus bookmark at thread program counter " + programCounter,
                bookmark);
    }

    // ----- Helpers -------------------------------------------------------------------------------

    private File testFile() {
        return new File(getClass().getResource("/ecfs/test.x64.elf").getFile());
    }

    private Program importTestSnapshot() throws Exception {
        Program program = project.importProgram(testFile());
        assertNotNull("the ECFS snapshot should import", program);
        return program;
    }

    private static String blockNames(Program program) {
        return Arrays.stream(program.getMemory().getBlocks())
                .map(MemoryBlock::getName)
                .collect(Collectors.joining(", "));
    }

    private static Address addressFor(Program program, long value) {
        return program.getAddressFactory().getDefaultAddressSpace().getAddress(value);
    }

    /** Pick one real symbol from an ECFS-only table so the test follows the fixture. */
    private ImportedSymbolExpectation firstExtraImportedSymbol() throws Exception {
        try (ByteProvider provider = new FileByteProvider(testFile(), null, AccessMode.READ)) {
            ElfHeader elf = new ElfHeader(provider, msg -> {});
            elf.parse();
            for (ElfSymbolTable symbolTable : elf.getSymbolTables()) {
                ElfSectionHeader tableSection = symbolTable.getTableSectionHeader();
                if (tableSection == null) {
                    continue;
                }

                String tableSectionName = tableSection.getNameAsString();
                if (!EcfsSymbolTableMarkup.isEcfsSymbolTable(tableSectionName)) {
                    continue;
                }

                String namespaceName = EcfsSymbolTableMarkup.namespaceNameFor(tableSectionName);
                for (ElfSymbol elfSymbol : symbolTable.getSymbols()) {
                    if (elfSymbol == null || elfSymbol.isSection() || elfSymbol.isFile()) {
                        continue;
                    }

                    String symbolName =
                            EcfsSymbolTableMarkup.normalizeName(elfSymbol.getNameAsString());
                    if (symbolName == null || elfSymbol.getValue() == 0) {
                        continue;
                    }

                    return new ImportedSymbolExpectation(
                            namespaceName, symbolName, elfSymbol.getValue());
                }
            }
        }

        throw new AssertionError("unable to find an ECFS-only extra symbol table in the fixture");
    }

    private List<EcfsPrStatusMarkup.PrStatusEntry> readPrStatusEntries() throws Exception {
        try (ByteProvider provider = new FileByteProvider(testFile(), null, AccessMode.READ)) {
            ElfHeader elf = new ElfHeader(provider, msg -> {});
            elf.parse();
            return EcfsPrStatusMarkup.readEntries(elf);
        }
    }

    private static final class ImportedSymbolExpectation {

        private final String namespaceName;
        private final String symbolName;
        private final long symbolValue;

        private ImportedSymbolExpectation(
                String namespaceName, String symbolName, long symbolValue) {
            this.namespaceName = namespaceName;
            this.symbolName = symbolName;
            this.symbolValue = symbolValue;
        }
    }
}
