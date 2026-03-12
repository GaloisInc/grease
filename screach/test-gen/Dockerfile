FROM ubuntu:22.04 AS crosstool


RUN apt-get update
RUN apt-get install -y wget xz-utils clang build-essential lld

RUN wget http://crosstool-ng.org/download/crosstool-ng/crosstool-ng-1.28.0.tar.xz
RUN tar -xvf crosstool-ng-1.28.0.tar.xz
WORKDIR /crosstool-ng-1.28.0

RUN apt-get install -y gcc g++ gperf bison flex texinfo help2man make libncurses5-dev \
    python3-dev autoconf automake libtool libtool-bin gawk wget bzip2 xz-utils unzip \
    patch libstdc++6 rsync git meson ninja-build
RUN wget -O /sbin/dumb-init https://github.com/Yelp/dumb-init/releases/download/v1.2.5/dumb-init_1.2.5_x86_64
RUN chmod a+x /sbin/dumb-init
RUN ./configure
RUN make
RUN make install
RUN ct-ng x86_64-unknown-linux-gnu
RUN  CT_EXPERIMENTAL=y CT_ALLOW_BUILD_AS_ROOT=y CT_ALLOW_BUILD_AS_ROOT_SURE=y ct-ng build


FROM ubuntu:22.04

WORKDIR /

COPY --from=crosstool /root/x-tools/ /root/x-tools/

RUN apt-get update
RUN apt-get install -y wget unzip openjdk-21-jdk

COPY --from=ghcr.io/astral-sh/uv:0.8.13 /uv /uvx /usr/local/bin/


ENV PATH="/root/x-tools/bin:${PATH}"


RUN wget -O /ghidra.zip https://github.com/NationalSecurityAgency/ghidra/releases/download/Ghidra_11.4.2_build/ghidra_11.4.2_PUBLIC_20250826.zip

RUN unzip -x ghidra.zip

COPY ./callgraph-script/ /callgraph-script


WORKDIR /callgraph-script

RUN uv sync --locked --no-dev

ENV PATH="/callgraph-script/.venv/bin:${PATH}"
ENV GHIDRA_INSTALL_DIR=/ghidra_11.4.2_PUBLIC