#!/bin/bash
set -e
cd "$(dirname "$0")"

docker build -t grease-ecfs-test . -q

docker run --rm --privileged \
    -v "$PWD:/out" \
    --entrypoint /bin/bash \
    grease-ecfs-test \
    -c "cd /work/test && ./build.sh && chmod 644 test.x64.elf && cp test.x64.elf /out/ && chmod 644 /out/test.x64.elf"

echo "Generated test.x64.elf"
