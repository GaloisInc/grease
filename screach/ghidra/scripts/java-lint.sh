#!/usr/bin/env bash
#
# Format and lint the Screach Ghidra plugin's Java sources.
#
# Tools (google-java-format, checkstyle) are expected on the PATH or as the jars
# named below. The Nix shell ([file:screach/ghidra/shell.nix]) provides both; CI
# ([file:.github/workflows/lint.yml]) installs the same pinned versions. Run with
# no arguments to check; run with `format` to rewrite files in place.
#
#   ./scripts/java-lint.sh           # check formatting + run checkstyle (CI mode)
#   ./scripts/java-lint.sh format    # apply formatting in place
#
# [tag:screach_java_lint_versions]
# These pinned versions are the single source of truth. The two other sites that
# install these tools ([file:screach/ghidra/shell.nix] and
# [file:.github/workflows/lint.yml]) carry a [ref:screach_java_lint_versions] and
# must use the same versions.
GJF_VERSION="1.25.2"
CHECKSTYLE_VERSION="10.21.1"

set -euo pipefail

cd "$(dirname "$0")/.."

# Collect the sources (NUL-delimited to be safe).
mapfile -d '' SOURCES < <(find src/main/java -name '*.java' -print0)
if [ "${#SOURCES[@]}" -eq 0 ]; then
  echo "No Java sources found." >&2
  exit 1
fi

# Resolve google-java-format: prefer a `google-java-format` launcher (Nix), then
# a GOOGLE_JAVA_FORMAT_JAR env var, then a jar in the current dir.
run_gjf() {
  if command -v google-java-format >/dev/null 2>&1; then
    google-java-format "$@"
  elif [ -n "${GOOGLE_JAVA_FORMAT_JAR:-}" ]; then
    java -jar "${GOOGLE_JAVA_FORMAT_JAR}" "$@"
  elif [ -f "google-java-format-${GJF_VERSION}-all-deps.jar" ]; then
    java -jar "google-java-format-${GJF_VERSION}-all-deps.jar" "$@"
  else
    echo "google-java-format not found (PATH, \$GOOGLE_JAVA_FORMAT_JAR, or jar)." >&2
    exit 127
  fi
}

run_checkstyle() {
  if command -v checkstyle >/dev/null 2>&1; then
    checkstyle "$@"
  elif [ -n "${CHECKSTYLE_JAR:-}" ]; then
    java -jar "${CHECKSTYLE_JAR}" "$@"
  elif [ -f "checkstyle-${CHECKSTYLE_VERSION}-all.jar" ]; then
    java -jar "checkstyle-${CHECKSTYLE_VERSION}-all.jar" "$@"
  else
    echo "checkstyle not found (PATH, \$CHECKSTYLE_JAR, or jar)." >&2
    exit 127
  fi
}

mode="${1:-check}"
case "${mode}" in
  format)
    echo "Formatting Java sources with google-java-format (AOSP)…"
    run_gjf --aosp --replace "${SOURCES[@]}"
    ;;
  check)
    echo "Checking Java formatting (google-java-format --aosp)…"
    run_gjf --aosp --dry-run --set-exit-if-changed "${SOURCES[@]}"
    echo "Running checkstyle…"
    # Config: [file:screach/ghidra/config/checkstyle/checkstyle.xml]
    run_checkstyle -c config/checkstyle/checkstyle.xml "${SOURCES[@]}"
    ;;
  *)
    echo "Usage: $0 [check|format]" >&2
    exit 2
    ;;
esac

echo "OK"
