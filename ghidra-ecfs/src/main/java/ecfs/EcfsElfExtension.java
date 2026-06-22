package ecfs;

import ghidra.app.util.bin.format.elf.ElfHeader;
import ghidra.app.util.bin.format.elf.ElfLoadHelper;
import ghidra.app.util.bin.format.elf.ElfSectionHeader;
import ghidra.app.util.bin.format.elf.extend.ElfExtension;
import ghidra.util.Msg;
import ghidra.util.exception.CancelledException;
import ghidra.util.task.TaskMonitor;

import java.io.IOException;
import java.util.Arrays;

/**
 * Teaches Ghidra's ELF loader how to recognize and correctly load <a
 * href="https://github.com/elfmaster/ee-ecfs">extended core file snapshot (ECFS)</a> files.
 *
 * <p>An ECFS snapshot is a post-mortem image of a process: it looks like an ELF core dump, but its
 * producer rewrites the section headers so the file resembles a normal executable (with {@code
 * .text}, {@code .dynsym}, {@code .got.plt}, and friends) while also recording process state such
 * as thread registers ({@code .prstatus}) and per-library symbol tables. Ghidra's stock ELF loader
 * does not understand these conventions, so this extension applies the fixups an ECFS snapshot
 * needs:
 *
 * <ul>
 *   <li>The synthetic {@code ._TEXT} marker section spans the same memory as {@code .text}. Left
 *       alone, Ghidra would load it as a second, overlapping executable block. We suppress its
 *       allocation/execution so only {@code .text} is loaded as code (see {@link
 *       #isSectionAllocated(ElfSectionHeader)} and {@link #isSectionExecutable(ElfSectionHeader)}).
 *   <li>ECFS clears the alloc/exec flags on {@code .text}, so we restore them so Ghidra treats it
 *       as a real, disassemblable code block.
 *   <li>ECFS snapshots carry one extra symbol table per shared library (e.g. {@code
 *       .dynsym.libc.so.6} or {@code .symtab.libc.so.6}). Ghidra's stock loader applies these
 *       symbols into the global namespace, where they are indistinguishable from the executable's
 *       own symbols. We re-home each such table's symbols under an {@code ECFS::Symbols::<table>}
 *       namespace so an analyst can tell which library a symbol came from (see {@link
 *       EcfsSymbolTableMarkup}).
 *   <li>Thread registers recorded in {@code .prstatus} are annotated at each thread's program
 *       counter with a comment and an analysis bookmark, so an analyst can immediately see where
 *       each thread was executing when the snapshot was taken (see {@link EcfsPrStatusMarkup}).
 * </ul>
 *
 * <p>The class name ends in {@code Extension} so Ghidra's {@code ClassSearcher} discovers it as an
 * {@code ElfExtension} extension point. Because {@link #canHandle(ElfHeader)} is gated on the ECFS
 * magic bytes, ordinary ELF files are never affected.
 */
public class EcfsElfExtension extends ElfExtension {

    /** Offset of the {@code e_ident} padding within an ELF header. Matches {@code EI_PAD}. */
    static final int EI_PAD_OFFSET = 9;

    /** The four magic bytes an ECFS producer writes into the ELF header padding. */
    static final byte[] ECFS_MAGIC = new byte[] {'E', 'C', 'F', 'S'};

    /** The {@code ._TEXT} synthetic section ECFS writes to cover the original text mapping. */
    static final String TEXT_SEGMENT_MARKER = "._TEXT";

    /** The conventional {@code .text} code section. */
    static final String TEXT_SECTION = ".text";

    static boolean hasEcfsMagic(byte[] bytes) {
        return Arrays.equals(ECFS_MAGIC, bytes);
    }

    /**
     * Override the {@code SHF_ALLOC} determination for the sections ECFS mishandles: suppress the
     * synthetic {@code ._TEXT} marker and force {@code .text} to be allocated. Returns {@code null}
     * for any other section so the stock flag-based determination applies.
     */
    static Boolean allocatedSectionOverride(String sectionName) {
        if (TEXT_SEGMENT_MARKER.equals(sectionName)) {
            return Boolean.FALSE;
        }
        if (TEXT_SECTION.equals(sectionName)) {
            return Boolean.TRUE;
        }
        return null;
    }

    /**
     * Override the executable determination for the sections ECFS mishandles: keep {@code ._TEXT}
     * non-executable and restore the executable flag on {@code .text}. Returns {@code null} for any
     * other section so the stock flag-based determination applies.
     */
    static Boolean executableSectionOverride(String sectionName) {
        if (TEXT_SEGMENT_MARKER.equals(sectionName)) {
            return Boolean.FALSE;
        }
        if (TEXT_SECTION.equals(sectionName)) {
            return Boolean.TRUE;
        }
        return null;
    }

    /**
     * Decide whether {@code elf} is an ECFS snapshot by checking for the {@code ECFS} magic bytes
     * the producer writes into the ELF header padding. This mirrors the cheap magic-number test
     * {@code decodeEcfs} performs in the {@code elf-edit-ecfs} Haskell decoder before doing any
     * heavier parsing.
     */
    @Override
    public boolean canHandle(ElfHeader elf) {
        try {
            return hasEcfsMagic(elf.getReader().readByteArray(EI_PAD_OFFSET, ECFS_MAGIC.length));
        } catch (IOException e) {
            Msg.warn(this, "Failed to read ELF header while checking for ECFS magic", e);
            return false;
        }
    }

    @Override
    public boolean canHandle(ElfLoadHelper elfLoadHelper) {
        return canHandle(elfLoadHelper.getElfHeader());
    }

    @Override
    public String getDataTypeSuffix() {
        return "_ECFS";
    }

    @Override
    public Boolean isSectionAllocated(ElfSectionHeader section) {
        Boolean override = allocatedSectionOverride(section.getNameAsString());
        return override != null ? override : super.isSectionAllocated(section);
    }

    @Override
    public Boolean isSectionExecutable(ElfSectionHeader section) {
        Boolean override = executableSectionOverride(section.getNameAsString());
        return override != null ? override : super.isSectionExecutable(section);
    }

    @Override
    public void processElf(ElfLoadHelper elfLoadHelper, TaskMonitor monitor)
            throws CancelledException {
        super.processElf(elfLoadHelper, monitor);
        EcfsSymbolTableMarkup.apply(this, elfLoadHelper, monitor);
        EcfsPrStatusMarkup.apply(elfLoadHelper, monitor);
    }
}
