#!/usr/bin/env bash
#
# Copyright (c) Galois, Inc. 2024

#####
## A script for generating the lists of overrides used in doc/overrides.md.
#####

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
CRUCIBLE_LLVM_DIR=${SCRIPT_DIR}/../../deps/crucible/crucible-llvm

function libc_overrides() {
  < "${CRUCIBLE_LLVM_DIR}/src/Lang/Crucible/LLVM/Intrinsics/Libc.hs" \
      grep -E 'llvmOvr.+@' \
    | grep -Eo '\|.+\|' \
    | tr -d '|' \
    | awk '{$1=$1};1' \
    | sed "s/.*/- \`&\`/"
}

function llvm_overrides() {
  # First, print all of the excluded libc overrides...
  < "${CRUCIBLE_LLVM_DIR}/src/Lang/Crucible/LLVM/Intrinsics/Libc.hs" \
      grep -E 'llvmOvr.+@' \
    | grep -Eo '\|.+\|' \
    | tr -d '|' \
    | awk '{$1=$1};1' \
    | sed "s/.*/- \`&\`/"
  # ...then print all of the LLVM overrides.
  < "${CRUCIBLE_LLVM_DIR}/src/Lang/Crucible/LLVM/Intrinsics/LLVM.hs" \
      grep -E 'llvmOvr.+@' \
    | grep -Eo '\|.+\|' \
    | tr -d '|' \
    | awk '{$1=$1};1' \
    | sed "s/.*/- \`&\`/"
}

case $1 in
  --libc-overrides)
    libc_overrides
    ;;
  --llvm-overrides)
    llvm_overrides
    ;;
  *)
    echo "Usage: ./get-overrides.sh (--libc-overrides|--llvm-overrides)"
    ;;
esac
