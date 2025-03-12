# Copyright (c) Galois, Inc. 2024

FROM nixos/nix:2.10.3

RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf
RUN nix doctor

WORKDIR /workdir

ADD ./flake.nix /workdir/flake.nix
ADD ./flake.lock /workdir/flake.lock
RUN nix develop -c armv7l-unknown-linux-gnueabihf-cc -v

ADD ./Makefile /workdir/Makefile

ENTRYPOINT ["nix", "develop", "-c", "make"]