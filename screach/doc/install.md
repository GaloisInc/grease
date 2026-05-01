# Installation

Screach can be installed via a [Docker] image, or as a standalone binary.

[Docker]: https://www.docker.com/

## Using Screach with Docker

The easiest way to get started with Screach is with Docker.

Once you've downloaded or built a Docker image, see [Usage](usage.md) for how to
run it.

### Using a pre-built Docker image

Screach publishes Docker images built from the `main` branch to the GitHub
container registry. You can pull the latest image with:
```
docker pull ghcr.io/galoisinc/screach:nightly
docker tag ghcr.io/galoisinc/screach:nightly screach:latest
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
docker build --target screach . --tag screach:latest
```

## Building a binary from source

See the [developer's guide](dev.md) for how to build `screach` from source.
