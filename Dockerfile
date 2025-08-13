# Copyright (c) Galois, Inc. 2024

FROM ubuntu:22.04 AS build

RUN apt update && \
    apt install -y \
      # ghcup requirements \
      build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses6 libtinfo6 \
      # Although ghcup's version of GHC shouldn't require libnuma, the aarch64 \
      # version does impose this requirement for unknown reasons. \
      # (See https://gitlab.haskell.org/ghc/ghc/-/issues/20876#note_399802) \
      libnuma1 \
      # GREASE dependencies \
      pkg-config zlib1g-dev \
      # Miscellaneous \
      git locales unzip wget && \
    rm -rf /var/lib/apt/lists/*

# Support Unicode via UTF-8
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

ARG TARGETPLATFORM
# If you update the values below, make sure to also update them in
# .github/workflows/ci.yaml.
ENV WHAT4_SOLVERS_VER=snapshot-20241119
ENV GHCUP_VER=0.1.40.0
ENV CABAL_VER=3.14.2.0
ENV GHC_VER=9.10.2

WORKDIR /usr/local/bin
RUN case ${TARGETPLATFORM} in \
      "linux/amd64") \
        WHAT4_SOLVERS_ARCH=X64 ;; \
      "linux/arm64" | "linux/arm64/v8") \
        WHAT4_SOLVERS_ARCH=ARM64 ;; \
      *) \
        printf "Unsupported architecture: %s\n" "${TARGETPLATFORM}" >&2 \
        exit 1 ;; \
    esac && \
    curl -o solvers.zip -sL "https://github.com/GaloisInc/what4-solvers/releases/download/${WHAT4_SOLVERS_VER}/ubuntu-22.04-${WHAT4_SOLVERS_ARCH}-bin.zip" && \
    unzip solvers.zip && \
    rm solvers.zip && \
    chmod +x *

ENV PATH=/root/ghcup-download/bin:/root/.ghcup/bin:$PATH
RUN case ${TARGETPLATFORM} in \
      "linux/amd64") \
        GHCUP_ARCH=x86_64 ;; \
      "linux/arm64" | "linux/arm64/v8") \
        GHCUP_ARCH=aarch64 ;; \
      *) \
        printf "Unsupported architecture: %s\n" "${TARGETPLATFORM}" >&2 \
        exit 1 ;; \
    esac && \
    mkdir -p /root/ghcup-download/bin && \
    curl -L "https://downloads.haskell.org/~ghcup/${GHCUP_VER}/${GHCUP_ARCH}-linux-ghcup-${GHCUP_VER}" -o /root/ghcup-download/bin/ghcup && \
    chmod +x /root/ghcup-download/bin/ghcup
RUN mkdir -p /root/.ghcup && \
    ghcup --version && \
    ghcup install cabal ${CABAL_VER} && \
    ghcup install ghc ${GHC_VER} && \
    ghcup set ghc ${GHC_VER}
RUN cabal v2-update

RUN mkdir -p /home/src
COPY . /home/src
WORKDIR /home/src
RUN cabal configure -w "ghc-${GHC_VER}" --enable-tests -j5 && \
    cabal build pkg:grease-cli
RUN cabal test pkg:grease-cli

RUN cp $(cabal list-bin -v0 exe:grease)        /usr/local/bin/grease && \
    cp $(cabal list-bin -v0 test:grease-tests) /usr/local/bin/grease-tests && \
    rm -rf dist-newstyle/

FROM ubuntu:22.04

USER root
RUN apt-get update && \
    apt-get install -y \
      libgmp10 zlib1g unzip locales

# Support Unicode via UTF-8
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

COPY --from=build \
  /usr/local/bin/grease \
  /usr/local/bin/grease-tests \
  /usr/local/bin/cvc4 \
  /usr/local/bin/cvc5 \
  /usr/local/bin/yices \
  /usr/local/bin/z3 \
  /usr/local/bin/
COPY --from=build /home/src/grease-cli/tests /tests

ENTRYPOINT ["/usr/local/bin/grease"]
