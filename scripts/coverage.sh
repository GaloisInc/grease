#!/usr/bin/env bash

# Run `hpc ` with paths appropriate for use with `cabal/coverage.cabal.project`.
# See `doc/dev/tests.md` for directions.

set -euo pipefail

grease_ver=$(grep '^version:' grease/grease.cabal | awk '{ print $2; }')
ghc_ver=$(ghc --numeric-version)

# This obviously isn't comprehensive, but it works well enough for now.
platform=x86_64-linux
if [[ ${OSTYPE} == darwin* ]]; then
  platform=aarch64-osx
fi

tix=$(find dist-newstyle -type f -name '*.tix')

# For some reason, we have to pass absolute paths to `--hpcdir`.
hpc "${@}" \
  ${tix} \
  --hpcdir=${PWD}/dist-newstyle/build/${platform}/ghc-${ghc_ver}/grease-${grease_ver}/build/extra-compilation-artifacts/hpc/dyn/mix \
  --hpcdir=${PWD}/dist-newstyle/build/${platform}/ghc-${ghc_ver}/grease-cli-${grease_ver}/build/extra-compilation-artifacts/hpc/dyn/mix \
  --hpcdir=${PWD}/dist-newstyle/build/${platform}/ghc-${ghc_ver}/grease-exe-${grease_ver}/build/extra-compilation-artifacts/hpc/dyn/mix \
  --hpcdir=${PWD}/dist-newstyle/build/${platform}/ghc-${ghc_ver}/grease-exe-${grease_ver}/x/grease/build/grease/grease-tmp/extra-compilation-artifacts/hpc/vanilla/mix \
  --hpcdir=${PWD}/dist-newstyle/build/${platform}/ghc-${ghc_ver}/grease-exe-${grease_ver}/t/grease-tests/build/grease-tests/grease-tests-tmp/extra-compilation-artifacts/hpc/vanilla/mix \
  --srcdir=grease \
  --srcdir=grease-cli \
  --srcdir=grease-exe
