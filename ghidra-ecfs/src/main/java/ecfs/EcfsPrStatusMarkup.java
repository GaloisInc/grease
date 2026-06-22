package ecfs;

import ghidra.app.util.bin.format.elf.ElfConstants;
import ghidra.app.util.bin.format.elf.ElfHeader;
import ghidra.app.util.bin.format.elf.ElfLoadHelper;
import ghidra.app.util.bin.format.elf.ElfSectionHeader;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.BookmarkManager;
import ghidra.program.model.listing.BookmarkType;
import ghidra.program.model.listing.CodeUnit;
import ghidra.program.model.listing.Listing;
import ghidra.program.model.listing.Program;
import ghidra.util.exception.CancelledException;
import ghidra.util.task.TaskMonitor;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Annotates the program counter of every thread recorded in the {@code .prstatus} section with a
 * pre-comment (dumping the thread's registers) and an {@code Analysis} bookmark, so an analyst can
 * immediately locate where each thread was executing when the snapshot was captured.
 *
 * <p>The per-architecture register layouts below mirror the core-dump decoders in {@code
 * elf-edit-core-dump} so the Ghidra extension reads the same {@code .prstatus} entries the rest of
 * GREASE understands.
 */
final class EcfsPrStatusMarkup {

    static final String BOOKMARK_CATEGORY = "ECFS prstatus";
    static final String PRSTATUS_SECTION = ".prstatus";

    // The offsets and register order below mirror the core-dump layouts in
    // [file:elf-edit-core-dump/src/Data/ElfEdit/CoreDump/X86_64.hs],
    // [file:elf-edit-core-dump/src/Data/ElfEdit/CoreDump/ARM.hs], and
    // [file:elf-edit-core-dump/src/Data/ElfEdit/CoreDump/PPC.hs].
    private static final PrStatusLayout X86_64_LAYOUT =
            new PrStatusLayout(
                    "x86_64",
                    8,
                    112,
                    "rip",
                    "rsp",
                    "rbp",
                    new String[] {
                        "r15",
                        "r14",
                        "r13",
                        "r12",
                        "rbp",
                        "rbx",
                        "r11",
                        "r10",
                        "r9",
                        "r8",
                        "rax",
                        "rcx",
                        "rdx",
                        "rsi",
                        "rdi",
                        "orig_rax",
                        "rip",
                        "cs",
                        "eflags",
                        "rsp",
                        "ss",
                        "fs_base",
                        "gs_base",
                        "ds",
                        "es",
                        "fs",
                        "gs"
                    });

    private static final PrStatusLayout ARM_LAYOUT =
            new PrStatusLayout(
                    "arm",
                    4,
                    72,
                    "r15",
                    "r13",
                    "r11",
                    new String[] {
                        "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11",
                        "r12", "r13", "r14", "r15", "cpsr", "orig_r0"
                    });

    private static final PrStatusLayout PPC32_LAYOUT =
            new PrStatusLayout("ppc32", 4, 64, "nip", "r1", null, ppcRegisterNames());

    private static final PrStatusLayout PPC64_LAYOUT =
            new PrStatusLayout("ppc64", 8, 96, "nip", "r1", null, ppcRegisterNames());

    private EcfsPrStatusMarkup() {}

    static void apply(ElfLoadHelper elfLoadHelper, TaskMonitor monitor) throws CancelledException {
        List<PrStatusEntry> prStatusEntries;
        try {
            prStatusEntries = readEntries(elfLoadHelper.getElfHeader());
        } catch (IOException e) {
            elfLoadHelper.log(e);
            return;
        }

        if (prStatusEntries.isEmpty()) {
            return;
        }

        Program program = elfLoadHelper.getProgram();
        Map<Address, List<PrStatusEntry>> entriesByAddress = new LinkedHashMap<>();
        for (PrStatusEntry prStatusEntry : prStatusEntries) {
            monitor.checkCancelled();

            if (prStatusEntry.programCounter == 0) {
                continue;
            }

            Address threadPcAddress;
            try {
                threadPcAddress = elfLoadHelper.getDefaultAddress(prStatusEntry.programCounter);
            } catch (RuntimeException e) {
                continue;
            }

            if (!program.getMemory().contains(threadPcAddress)) {
                continue;
            }

            entriesByAddress
                    .computeIfAbsent(threadPcAddress, unused -> new ArrayList<>())
                    .add(prStatusEntry);
        }

        if (entriesByAddress.isEmpty()) {
            return;
        }

        Listing listing = program.getListing();
        BookmarkManager bookmarkManager = program.getBookmarkManager();
        for (Map.Entry<Address, List<PrStatusEntry>> entry : entriesByAddress.entrySet()) {
            monitor.checkCancelled();

            Address address = entry.getKey();
            List<PrStatusEntry> addressEntries = entry.getValue();
            String commentBlock = buildComment(addressEntries);
            String existingComment = listing.getComment(CodeUnit.PRE_COMMENT, address);
            listing.setComment(
                    address, CodeUnit.PRE_COMMENT, mergeComment(existingComment, commentBlock));
            bookmarkManager.setBookmark(
                    address,
                    BookmarkType.ANALYSIS,
                    BOOKMARK_CATEGORY,
                    buildBookmarkComment(addressEntries));
        }

        elfLoadHelper.log(
                "ECFS: annotated " + prStatusEntries.size() + " prstatus thread snapshot(s)");
    }

    static List<PrStatusEntry> readEntries(ElfHeader elfHeader) throws IOException {
        ElfSectionHeader prStatusSection = elfHeader.getSection(PRSTATUS_SECTION);
        if (prStatusSection == null) {
            return Collections.emptyList();
        }

        PrStatusLayout layout = layoutFor(elfHeader);
        if (layout == null) {
            return Collections.emptyList();
        }

        byte[] sectionBytes = readSectionBytes(prStatusSection);
        if (sectionBytes.length == 0) {
            return Collections.emptyList();
        }

        int entrySize = Math.toIntExact(prStatusSection.getEntrySize());
        if (entrySize <= 0) {
            entrySize = layout.minimumEntrySize();
        }
        if (entrySize < layout.minimumEntrySize()) {
            return Collections.emptyList();
        }

        int entryCount = sectionBytes.length / entrySize;
        List<PrStatusEntry> entries = new ArrayList<>(entryCount);
        for (int entryIndex = 0; entryIndex < entryCount; entryIndex++) {
            int entryOffset = entryIndex * entrySize;
            if (entryOffset + layout.minimumEntrySize() > sectionBytes.length) {
                break;
            }
            entries.add(layout.decode(sectionBytes, entryOffset, entryIndex + 1, elfHeader));
        }
        return entries;
    }

    private static String buildBookmarkComment(List<PrStatusEntry> prStatusEntries) {
        if (prStatusEntries.size() == 1) {
            PrStatusEntry prStatusEntry = prStatusEntries.get(0);
            return "Thread "
                    + prStatusEntry.threadIndex
                    + " "
                    + prStatusEntry.programCounterRegister
                    + "="
                    + prStatusEntry.hex(prStatusEntry.programCounter)
                    + " "
                    + prStatusEntry.stackPointerRegister
                    + "="
                    + prStatusEntry.hex(prStatusEntry.stackPointer);
        }

        StringBuilder builder = new StringBuilder("Threads ");
        for (int i = 0; i < prStatusEntries.size(); i++) {
            if (i != 0) {
                builder.append(", ");
            }
            builder.append(prStatusEntries.get(i).threadIndex);
        }
        builder.append(" snapshot");
        return builder.toString();
    }

    private static String buildComment(List<PrStatusEntry> prStatusEntries) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < prStatusEntries.size(); i++) {
            if (i != 0) {
                builder.append("\n\n");
            }

            PrStatusEntry prStatusEntry = prStatusEntries.get(i);

            // One register per line keeps every line short enough that Ghidra's listing comment
            // field never clips it with a trailing "...". (A 64-bit value already renders as
            // "0x" + 16 hex digits, so even two columns would overrun the field width.) Names are
            // padded to a common column so the values line up.
            int nameWidth = registerNameColumnWidth(prStatusEntry);

            builder.append("ECFS prstatus thread ")
                    .append(prStatusEntry.threadIndex)
                    .append(" (")
                    .append(prStatusEntry.architectureName)
                    .append(")\n");
            appendRegisterLine(
                    builder,
                    "pc",
                    prStatusEntry.programCounterRegister,
                    prStatusEntry.hex(prStatusEntry.programCounter),
                    nameWidth);
            appendRegisterLine(
                    builder,
                    "sp",
                    prStatusEntry.stackPointerRegister,
                    prStatusEntry.hex(prStatusEntry.stackPointer),
                    nameWidth);

            if (prStatusEntry.framePointerRegister != null && prStatusEntry.framePointer != null) {
                appendRegisterLine(
                        builder,
                        "fp",
                        prStatusEntry.framePointerRegister,
                        prStatusEntry.hex(prStatusEntry.framePointer.longValue()),
                        nameWidth);
            }

            for (Map.Entry<String, Long> registerEntry : prStatusEntry.registerValues.entrySet()) {
                String registerName = registerEntry.getKey();
                if (registerName.equals(prStatusEntry.programCounterRegister)
                        || registerName.equals(prStatusEntry.stackPointerRegister)
                        || (prStatusEntry.framePointerRegister != null
                                && registerName.equals(prStatusEntry.framePointerRegister))) {
                    continue;
                }

                appendRegisterLine(
                        builder,
                        null,
                        registerName,
                        prStatusEntry.hex(registerEntry.getValue().longValue()),
                        nameWidth);
            }

            // Drop the trailing newline left by the last register line.
            if (builder.charAt(builder.length() - 1) == '\n') {
                builder.setLength(builder.length() - 1);
            }
        }
        return builder.toString();
    }

    /**
     * Append a single {@code name = value} register line, padding the name to {@code nameWidth} so
     * values align into a column. An optional {@code role} (e.g. {@code pc}/{@code sp}/{@code fp})
     * is shown in parentheses after the register name.
     */
    private static void appendRegisterLine(
            StringBuilder builder, String role, String registerName, String value, int nameWidth) {
        String label = role == null ? registerName : registerName + " (" + role + ")";
        builder.append(label);
        for (int pad = label.length(); pad < nameWidth; pad++) {
            builder.append(' ');
        }
        builder.append(" = ").append(value).append('\n');
    }

    /** Width of the register-name column: the longest {@code name (role)} label in the entry. */
    private static int registerNameColumnWidth(PrStatusEntry prStatusEntry) {
        int width = prStatusEntry.programCounterRegister.length() + " (pc)".length();
        width = Math.max(width, prStatusEntry.stackPointerRegister.length() + " (sp)".length());
        if (prStatusEntry.framePointerRegister != null) {
            width = Math.max(width, prStatusEntry.framePointerRegister.length() + " (fp)".length());
        }
        for (String registerName : prStatusEntry.registerValues.keySet()) {
            width = Math.max(width, registerName.length());
        }
        return width;
    }

    private static String mergeComment(String existingComment, String commentBlock) {
        if (existingComment == null || existingComment.isBlank()) {
            return commentBlock;
        }
        if (existingComment.contains(commentBlock)) {
            return existingComment;
        }
        return existingComment + "\n\n" + commentBlock;
    }

    private static PrStatusLayout layoutFor(ElfHeader elfHeader) {
        short machine = elfHeader.e_machine();
        if (machine == ElfConstants.EM_X86_64) {
            return X86_64_LAYOUT;
        }
        if (machine == ElfConstants.EM_ARM && elfHeader.is32Bit()) {
            return ARM_LAYOUT;
        }
        if (machine == ElfConstants.EM_PPC64) {
            return PPC64_LAYOUT;
        }
        if (machine == ElfConstants.EM_PPC) {
            return elfHeader.is64Bit() ? PPC64_LAYOUT : PPC32_LAYOUT;
        }
        return null;
    }

    private static byte[] readSectionBytes(ElfSectionHeader sectionHeader) throws IOException {
        int sectionSize = Math.toIntExact(sectionHeader.getSize());
        try (InputStream inputStream = sectionHeader.getRawInputStream()) {
            return inputStream.readNBytes(sectionSize);
        }
    }

    private static String[] ppcRegisterNames() {
        String[] registerNames = new String[43];
        for (int i = 0; i <= 31; i++) {
            registerNames[i] = "r" + i;
        }
        registerNames[32] = "nip";
        registerNames[33] = "msr";
        registerNames[34] = "orig_gpr3";
        registerNames[35] = "ctr";
        registerNames[36] = "lr";
        registerNames[37] = "xer";
        registerNames[38] = "cr";
        registerNames[39] = "mq";
        registerNames[40] = "trap";
        registerNames[41] = "dar";
        registerNames[42] = "dsisr";
        return registerNames;
    }

    static final class PrStatusEntry {

        final int threadIndex;
        final String architectureName;
        final int wordSize;
        final String programCounterRegister;
        final long programCounter;
        final String stackPointerRegister;
        final long stackPointer;
        final String framePointerRegister;
        final Long framePointer;
        final LinkedHashMap<String, Long> registerValues;

        PrStatusEntry(
                int threadIndex,
                String architectureName,
                int wordSize,
                String programCounterRegister,
                long programCounter,
                String stackPointerRegister,
                long stackPointer,
                String framePointerRegister,
                Long framePointer,
                LinkedHashMap<String, Long> registerValues) {
            this.threadIndex = threadIndex;
            this.architectureName = architectureName;
            this.wordSize = wordSize;
            this.programCounterRegister = programCounterRegister;
            this.programCounter = programCounter;
            this.stackPointerRegister = stackPointerRegister;
            this.stackPointer = stackPointer;
            this.framePointerRegister = framePointerRegister;
            this.framePointer = framePointer;
            this.registerValues = registerValues;
        }

        String hex(long value) {
            return String.format("0x%0" + (wordSize * 2) + "x", value);
        }
    }

    private static final class PrStatusLayout {

        private final String architectureName;
        private final int wordSize;
        private final int registerOffset;
        private final String programCounterRegister;
        private final String stackPointerRegister;
        private final String framePointerRegister;
        private final String[] registerNames;

        private PrStatusLayout(
                String architectureName,
                int wordSize,
                int registerOffset,
                String programCounterRegister,
                String stackPointerRegister,
                String framePointerRegister,
                String[] registerNames) {
            this.architectureName = architectureName;
            this.wordSize = wordSize;
            this.registerOffset = registerOffset;
            this.programCounterRegister = programCounterRegister;
            this.stackPointerRegister = stackPointerRegister;
            this.framePointerRegister = framePointerRegister;
            this.registerNames = registerNames;
        }

        private PrStatusEntry decode(
                byte[] bytes, int entryOffset, int threadIndex, ElfHeader elfHeader) {
            ByteBuffer buffer = ByteBuffer.wrap(bytes);
            buffer.order(
                    elfHeader.isLittleEndian() ? ByteOrder.LITTLE_ENDIAN : ByteOrder.BIG_ENDIAN);
            buffer.position(entryOffset + registerOffset);

            LinkedHashMap<String, Long> registerValues = new LinkedHashMap<>();
            for (String registerName : registerNames) {
                registerValues.put(registerName, readWord(buffer));
            }

            long programCounter = registerValues.get(programCounterRegister).longValue();
            long stackPointer = registerValues.get(stackPointerRegister).longValue();
            Long framePointer = null;
            if (framePointerRegister != null && registerValues.containsKey(framePointerRegister)) {
                framePointer = registerValues.get(framePointerRegister);
            }

            return new PrStatusEntry(
                    threadIndex,
                    architectureName,
                    wordSize,
                    programCounterRegister,
                    programCounter,
                    stackPointerRegister,
                    stackPointer,
                    framePointerRegister,
                    framePointer,
                    registerValues);
        }

        private int minimumEntrySize() {
            return registerOffset + (registerNames.length * wordSize);
        }

        private long readWord(ByteBuffer buffer) {
            if (wordSize == 8) {
                return buffer.getLong();
            }
            return Integer.toUnsignedLong(buffer.getInt());
        }
    }
}
