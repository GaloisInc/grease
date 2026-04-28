FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    build-essential git libelf-dev libdwarf-dev gdb \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /work

RUN git clone https://github.com/elfmaster/ee-ecfs /work/ecfs && \
    cd /work/ecfs && \
    (sed -i 's@ > /proc/sys/kernel/core_pattern@@' Makefile || true) && \
    make && make install || true

ENV PATH="/opt/ecfs/bin:${PATH}"

WORKDIR /work/test
COPY libmath.c test.c build.sh /work/test/
RUN chmod +x /work/test/build.sh
