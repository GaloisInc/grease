package ecfs;

import ghidra.app.util.bin.format.elf.ElfLoadHelper;
import ghidra.app.util.bin.format.elf.ElfSectionHeader;
import ghidra.app.util.bin.format.elf.ElfSymbol;
import ghidra.app.util.bin.format.elf.ElfSymbolTable;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Program;
import ghidra.program.model.symbol.Namespace;
import ghidra.program.model.symbol.SourceType;
import ghidra.program.model.symbol.SymbolTable;
import ghidra.program.model.symbol.SymbolUtilities;
import ghidra.util.exception.CancelledException;
import ghidra.util.exception.DuplicateNameException;
import ghidra.util.exception.InvalidInputException;
import ghidra.util.exception.NoValueException;
import ghidra.util.task.TaskMonitor;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Re-homes the symbols of every ECFS-only per-library symbol table (those named with the {@code
 * .dynsym.<lib>} or {@code .symtab.<lib>} convention) into an {@code ECFS::Symbols::<table>}
 * namespace.
 *
 * <p>By the time {@link EcfsElfExtension#processElf} runs, Ghidra's stock loader has already
 * applied these symbols into the global namespace, where they are indistinguishable from the
 * executable's own symbols. We create a corresponding namespaced label at each symbol's address;
 * both labels coexist, but the namespaced one records the originating library.
 */
final class EcfsSymbolTableMarkup {

    static final String ECFS_NAMESPACE = "ECFS";
    static final String SYMBOLS_NAMESPACE = "Symbols";

    /**
     * Prefixes shared by the ECFS-only per-library symbol tables (e.g. {@code .dynsym.libc.so.6},
     * {@code .symtab.libc.so.6}). The plain {@code .dynsym}/{@code .symtab} sections do not carry
     * the trailing dot and are handled correctly by the stock loader.
     */
    static final String DYNSYM_PREFIX = ".dynsym.";

    static final String SYMTAB_PREFIX = ".symtab.";

    private EcfsSymbolTableMarkup() {}

    static void apply(EcfsElfExtension extension, ElfLoadHelper elfLoadHelper, TaskMonitor monitor)
            throws CancelledException {
        // Keep ECFS-only symbol tables in a dedicated namespace instead of re-importing the
        // standard ELF .symtab/.dynsym tables that Ghidra already understands.
        Program program = elfLoadHelper.getProgram();
        SymbolTable symbolTable = program.getSymbolTable();
        Namespace ecfsNamespace =
                getOrCreateNamespace(symbolTable, program.getGlobalNamespace(), ECFS_NAMESPACE);
        Namespace symbolsNamespace =
                getOrCreateNamespace(symbolTable, ecfsNamespace, SYMBOLS_NAMESPACE);
        if (symbolsNamespace == null) {
            return;
        }

        int importedSymbolCount = 0;
        Set<String> importedTables = new LinkedHashSet<>();
        for (ElfSymbolTable elfSymbolTable : elfLoadHelper.getElfHeader().getSymbolTables()) {
            monitor.checkCancelled();

            ElfSectionHeader tableSection = elfSymbolTable.getTableSectionHeader();
            if (tableSection == null) {
                continue;
            }

            String tableSectionName = tableSection.getNameAsString();
            if (!isEcfsSymbolTable(tableSectionName)) {
                continue;
            }

            Namespace tableNamespace =
                    getOrCreateNamespace(
                            symbolTable, symbolsNamespace, namespaceNameFor(tableSectionName));
            if (tableNamespace == null) {
                continue;
            }

            int importedFromTable = 0;
            for (ElfSymbol elfSymbol : elfSymbolTable.getSymbols()) {
                monitor.checkCancelled();

                if (shouldSkipSymbol(elfSymbol)) {
                    continue;
                }

                String symbolName = normalizeName(elfSymbol.getNameAsString());
                if (symbolName == null) {
                    continue;
                }

                Address symbolAddress = resolveAddress(extension, elfLoadHelper, elfSymbol);
                if (symbolAddress == null || !program.getMemory().contains(symbolAddress)) {
                    continue;
                }

                if (symbolTable.getSymbol(symbolName, symbolAddress, tableNamespace) != null) {
                    continue;
                }

                try {
                    symbolTable.createLabel(
                            symbolAddress, symbolName, tableNamespace, SourceType.IMPORTED);
                    importedFromTable++;
                } catch (InvalidInputException e) {
                    elfLoadHelper.log(
                            "ECFS: failed to import symbol "
                                    + symbolName
                                    + " from "
                                    + tableSectionName
                                    + ": "
                                    + e.getMessage());
                }
            }

            if (importedFromTable > 0) {
                importedSymbolCount += importedFromTable;
                importedTables.add(tableSectionName);
            }
        }

        if (importedSymbolCount > 0) {
            elfLoadHelper.log(
                    "ECFS: imported "
                            + importedSymbolCount
                            + " symbol(s) from "
                            + importedTables.size()
                            + " extra symbol table(s)");
        }
    }

    static boolean isEcfsSymbolTable(String tableSectionName) {
        return tableSectionName != null
                && (tableSectionName.startsWith(DYNSYM_PREFIX)
                        || tableSectionName.startsWith(SYMTAB_PREFIX));
    }

    static String namespaceNameFor(String tableSectionName) {
        if (tableSectionName.startsWith(DYNSYM_PREFIX)) {
            return normalizeNamespaceName(tableSectionName.substring(DYNSYM_PREFIX.length()));
        }
        if (tableSectionName.startsWith(SYMTAB_PREFIX)) {
            return normalizeNamespaceName(tableSectionName.substring(SYMTAB_PREFIX.length()));
        }
        return normalizeNamespaceName(tableSectionName);
    }

    static String normalizeName(String rawName) {
        if (rawName == null) {
            return null;
        }

        String trimmed = rawName.trim();
        if (trimmed.isEmpty()) {
            return null;
        }

        try {
            SymbolUtilities.validateName(trimmed);
            return trimmed;
        } catch (InvalidInputException e) {
            StringBuilder sanitized = new StringBuilder(trimmed.length());
            for (int i = 0; i < trimmed.length(); i++) {
                char current = trimmed.charAt(i);
                sanitized.append(SymbolUtilities.isInvalidChar(current) ? '_' : current);
            }

            if (sanitized.length() == 0) {
                return null;
            }

            String sanitizedName = sanitized.toString();
            try {
                SymbolUtilities.validateName(sanitizedName);
                return sanitizedName;
            } catch (InvalidInputException ignored) {
                return null;
            }
        }
    }

    private static Namespace getOrCreateNamespace(
            SymbolTable symbolTable, Namespace parent, String rawNamespaceName) {
        String namespaceName = normalizeNamespaceName(rawNamespaceName);
        if (namespaceName == null) {
            return null;
        }

        Namespace existing = symbolTable.getNamespace(namespaceName, parent);
        if (existing != null) {
            return existing;
        }

        try {
            return symbolTable.getOrCreateNameSpace(parent, namespaceName, SourceType.IMPORTED);
        } catch (InvalidInputException e) {
            return null;
        } catch (DuplicateNameException e) {
            return null;
        }
    }

    private static String normalizeNamespaceName(String rawNamespaceName) {
        if (rawNamespaceName == null) {
            return null;
        }

        String trimmed = rawNamespaceName.trim();
        if (trimmed.isEmpty()) {
            return null;
        }

        if (trimmed.startsWith(".")) {
            trimmed = trimmed.substring(1);
        }

        return normalizeName(trimmed);
    }

    private static boolean shouldSkipSymbol(ElfSymbol elfSymbol) {
        if (elfSymbol == null) {
            return true;
        }
        if (elfSymbol.isSection() || elfSymbol.isFile()) {
            return true;
        }
        if (elfSymbol.getValue() == 0) {
            return true;
        }
        return elfSymbol.getNameAsString() == null || elfSymbol.getNameAsString().isBlank();
    }

    private static Address resolveAddress(
            EcfsElfExtension extension, ElfLoadHelper elfLoadHelper, ElfSymbol elfSymbol) {
        try {
            Address resolved = extension.calculateSymbolAddress(elfLoadHelper, elfSymbol);
            if (resolved != null) {
                return resolved;
            }
        } catch (NoValueException e) {
            // Fall back to the raw snapshot address for ECFS-only symbol tables.
        }

        try {
            return elfLoadHelper.getDefaultAddress(elfSymbol.getValue());
        } catch (RuntimeException e) {
            return null;
        }
    }
}
