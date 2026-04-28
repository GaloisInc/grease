#!/bin/bash
set -e

gcc -shared -fPIC -g -o libmath.so libmath.c
gcc -g -o test test.c -L. -lmath -Wl,-rpath,'$ORIGIN'

LD_LIBRARY_PATH=. ./test &
PID=$!
sleep 1

if ! kill -0 $PID 2>/dev/null; then
    echo "Error: Test program exited"
    exit 1
fi

# Set coredump_filter so that ecfs captures all pages including executable text
echo "0x1ff" > /proc/$PID/coredump_filter

timeout 30 ecfs -S $PID -o test.x64.elf -t 2>&1 || {
    kill $PID 2>/dev/null || true
    echo "Error: ecfs_snapshot failed"
    exit 1
}

kill $PID 2>/dev/null || true
wait $PID 2>/dev/null || true

et_flip test.x64.elf 2>/dev/null || true
et_flip test.x64.elf 2>/dev/null || true

echo "Generated test.x64.elf"
