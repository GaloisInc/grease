#!/usr/bin/env bash
set -xEeuo pipefail

EXT=""
if [[ "$RUNNER_OS" == 'Windows' ]]; then
  EXT=".exe"
fi
BIN=${PWD}/bin

is_exe() { [[ -x "$1/$2$EXT" ]] || command -v "$2" > /dev/null 2>&1; }

install_system_deps() {
  mkdir -p "$BIN"

  pushd $BIN
  curl -o bins.zip -fsSL "https://github.com/GaloisInc/what4-solvers/releases/download/$SOLVER_PKG_VERSION/$BIN_ZIP_FILE"
  unzip -o bins.zip
  rm bins.zip
  popd

  chmod +x $BIN/*
  cp $BIN/yices_smt2$EXT $BIN/yices-smt2$EXT
  export PATH=$BIN:$PATH
  echo "$BIN" >> "$GITHUB_PATH"
  is_exe "$BIN" z3 && is_exe "$BIN" cvc4 && is_exe "$BIN" cvc5 && is_exe "$BIN" yices
}

COMMAND="$1"
shift

"$COMMAND" "$@"
