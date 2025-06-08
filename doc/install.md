# Installation

GREASE can be installed via a [Docker] image, or as a standalone binary.

[Docker]: https://www.docker.com/

## Using GREASE with Docker

The easiest way to get started with GREASE is with Docker.

Once you've downloaded or built a Docker image, see [Usage](usage.md) for how to
run it.

### Using a pre-built Docker image

GREASE publishes Docker images built from the `main` branch to the GitHub
container registry. You can pull the latest image with:
```
docker pull ghcr.io/galoisinc/grease:nightly
docker tag ghcr.io/galoisinc/grease:nightly grease:latest
```

### Building an image from source

First, fetch the source:
```sh
git clone https://github.com/GaloisInc/grease
cd grease
git submodule update --init
```
Then, build the image:
```sh
docker build . --tag grease:latest
```

## Building a binary from source

See the [developer's guide](dev/dev.md) for how to build `grease` from source.

When run as a standalone binary, GREASE requires a recent version of one of the
following SMT solvers: `cvc4`, `cvc5`, `yices`, or `z3`. GREASE is currently
tested against `yices` version 2.6.2 in CI. Appropriate versions of these SMT
solvers are already installed in the GREASE Docker image. `grease` will default
to using `yices` unless you manually specify a solver using the `--solver`
option.

<!-- Copyright (c) Galois, Inc. 2024. -->
