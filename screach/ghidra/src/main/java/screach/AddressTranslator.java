package screach;

import ghidra.app.util.opinion.ElfLoader;
import ghidra.framework.options.Options;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Program;

/**
 * Converts between Ghidra runtime {@link Address}es and the addresses Screach expects on its
 * command line / reports in its output.
 *
 * <p>Ghidra prefers a non-zero image base and will sometimes load an ELF at an offset that other
 * tools (including Screach) don't know about. We mirror the logic in the sibling {@code
 * ghidra_scripts/grease.py} script ({@code ghidraAddressToGreaseOffset} / {@code
 * greaseOffsetToGhidraAddress}): subtract Ghidra's image base to get the file-relative offset, then
 * add back the ELF loader's original image base.
 *
 * <p>Note that {@link #screachToGhidra} is <em>not</em> a pure inverse of {@link #ghidraToScreach}:
 * addresses Screach <em>reports</em> additionally carry GREASE's load offset (a constant GREASE
 * adds to every reported address), whereas addresses on Screach's <em>command line</em> do not. We
 * subtract that load offset when translating reported addresses back. See {@link
 * #greaseLoadOffset}.
 */
final class AddressTranslator {

    /**
     * The load offset GREASE applies to a position-independent executable (PIE). GREASE loads PIEs
     * relative to this constant so that a null-pointer dereference does not land on validly-mapped
     * memory; non-PIE binaries get no offset. [ref:pie_load_offset]
     */
    private static final long PIE_LOAD_OFFSET = 0x10000000L;

    private AddressTranslator() {}

    /** Convert a Ghidra runtime address into the value Screach expects on its command line. */
    static long ghidraToScreach(Program program, Address ghidraAddress) {
        long offset = ghidraAddress.subtract(program.getImageBase());
        return offset + ElfLoader.getElfOriginalImageBase(program);
    }

    /** Format an address for Screach's command line (e.g. {@code 0x1234}). */
    static String formatForCli(Program program, Address ghidraAddress) {
        return String.format("0x%x", ghidraToScreach(program, ghidraAddress));
    }

    /**
     * Convert an address reported by Screach back into a Ghidra address so we can navigate to it.
     *
     * <p>This is <em>not</em> a pure inverse of {@link #ghidraToScreach}: a reported address has
     * GREASE's load offset baked in (see {@link #greaseLoadOffset}), so we subtract it in addition
     * to undoing the image-base adjustment.
     *
     * @return the corresponding Ghidra address, or {@code null} if it is not part of the program's
     *     address space
     */
    static Address screachToGhidra(Program program, long screachOffset) {
        long ghidraOffset =
                screachOffset
                        - greaseLoadOffset(program)
                        + program.getImageBase().getOffset()
                        - ElfLoader.getElfOriginalImageBase(program);
        try {
            return program.getImageBase().getNewAddress(ghidraOffset);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    /**
     * The load offset GREASE applies to this program's reported addresses: {@link #PIE_LOAD_OFFSET}
     * for a position-independent executable, otherwise 0.
     *
     * <p>Screach's plain-text output does not report the offset, so we reconstruct it from the same
     * property GREASE keys off — whether the ELF is a shared object (PIE). Ghidra records the ELF
     * type as the {@code "ELF File Type"} program-information property; {@code ET_DYN} is stored as
     * {@code "shared object"}.
     */
    private static long greaseLoadOffset(Program program) {
        Options info = program.getOptions(Program.PROGRAM_INFO);
        String elfType = info.getString(ElfLoader.ELF_FILE_TYPE_PROPERTY, null);
        return "shared object".equals(elfType) ? PIE_LOAD_OFFSET : 0L;
    }
}
