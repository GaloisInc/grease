FROM ${BUILD_BASE_PARENT} AS builder 

RUN apt-get update
RUN apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev zlib1g-dev

# This haskell version in BOOTSTRAP_HASKELL_GHC_VERSION should be one of the CI tested versions
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=9.10.2 BOOTSTRAP_HASKELL_CABAL_VERSION=latest BOOTSTRAP_HASKELL_INSTALL_STACK=0 BOOTSTRAP_HASKELL_INSTALL_HLS=0 BOOTSTRAP_HASKELL_ADJUST_BASHRC=P sh

ADD ./ /bracelet
WORKDIR /bracelet 
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"
RUN cabal build exe:screach
RUN cp "$(cabal list-bin -v0 exe:screach)" /usr/local/bin/screach

FROM ${BUILD_BASE_PARENT}

ARG TARGET_ARCH=X64
ARG WHAT4_SOLVERS_VER=snapshot-20251112

RUN apt-get update
RUN apt-get install -y build-essential curl zip

ENV WHAT4_SOLVERS_ARCH=${TARGET_ARCH}
ENV WHAT4_SOLVERS_VER=${WHAT4_SOLVERS_VER}

WORKDIR /usr/local/bin
RUN curl --proto '=https' --tlsv1.2  -o solvers.zip -sL "https://github.com/GaloisInc/what4-solvers/releases/download/${WHAT4_SOLVERS_VER}/ubuntu-24.04-${WHAT4_SOLVERS_ARCH}-bin.zip" && \
    unzip solvers.zip && \
    rm solvers.zip && \
    chmod +x *

COPY --from=builder /usr/local/bin/screach /usr/local/bin/
