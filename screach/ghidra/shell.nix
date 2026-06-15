# This Nix shell is provided for convenience; it is not tested in CI and it is
# not guaranteed to work.
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ghidra
    gradle
    jdk21
    # Java formatter + linter, used by [file:screach/ghidra/scripts/java-lint.sh].
    # nixpkgs pins the versions; keep them aligned with the pinned versions at
    # [ref:screach_java_lint_versions].
    google-java-format
    checkstyle
  ];

  shellHook = ''
    export GHIDRA_INSTALL_DIR="${pkgs.ghidra}/lib/ghidra"
    echo "Ghidra development environment"
    echo "GHIDRA_INSTALL_DIR set to: $GHIDRA_INSTALL_DIR"
    echo ""
    echo "Available commands:"
    echo "  gradle distributeExtension - Build the extension"
    echo "  ./scripts/java-lint.sh        - Check Java formatting + lint"
    echo "  ./scripts/java-lint.sh format - Apply Java formatting"
    echo "  ghidra - Launch Ghidra"
  '';
}
