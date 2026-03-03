#!/usr/bin/env bash

# A script for conveniently running a fixed version of gcc using the
# x86_64-linux-musl target triple. This uses Docker to make the script
# relatively portable.
#
# This assumes that the files being passed to the compiler reside in the same
# directory as this script or one of its subdirectories.

set -euo pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

docker run --rm \
  --entrypoint /bin/gcc \
  --user $(id -u):$(id -g) \
  --volume "$SCRIPT_DIR":/z \
  --workdir /z \
  muslcc/x86_64:x86_64-linux-musl \
  $@
