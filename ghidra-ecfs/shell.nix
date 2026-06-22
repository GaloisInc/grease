# This Nix shell is provided for convenience; it is not tested in CI and it is
# not guaranteed to work.
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ghidra
    gradle
    jdk21
    # Java formatter + linter. These are run via Gradle (google-java-format and
    # the checkstyle plugin, see [file:ghidra-ecfs/build.gradle]); nixpkgs is only
    # a convenience here. Keep aligned with the pinned [ref:ghidra_java_lint_versions].
    google-java-format
    checkstyle
  ];

  shellHook = ''
    export GHIDRA_INSTALL_DIR="${pkgs.ghidra}/lib/ghidra"
    echo "ECFS Ghidra development environment"
    echo "GHIDRA_INSTALL_DIR set to: $GHIDRA_INSTALL_DIR"
    echo ""
    echo "Available commands:"
    echo "  gradle buildExtension   - Build the extension zip"
    echo "  gradle installExtension - Install into the selected Ghidra"
    echo "  gradle test             - Run unit tests"
    echo "  gradle lintJava         - Check Java formatting + lint"
    echo "  gradle formatJava       - Apply Java formatting"
    echo "  ghidra                  - Launch the Nix-packaged Ghidra"
  '';
}
