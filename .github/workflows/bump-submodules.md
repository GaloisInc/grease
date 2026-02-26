---
description: Bump submodules.

on:
  schedule: daily
  workflow_dispatch:
  slash_command:
    name: bump-submodules
  reaction: "eyes"

permissions: read-all

network: defaults

safe-outputs:
  create-pull-request: {}

tools:
  github:
    toolsets: [all]
  web-fetch:
  bash: true

timeout-minutes: 60
---

# Bump submodules

Your job is to update dependencies for the GitHub repository ${{ github.repository }}.

## Steps

Bump submodules:
```sh
git submodule update --init
git submodule update --remote
```

Install the latest supported version of GHC (see `.github/workflows/ci.yml`).

Build:
```sh
cabal build pkg:grease-exe
```

If the build fails, update the code to address the breaking changes.

Run tests:
```sh
cd grease-exe
cabal run test:grease-tests --
```

## Output

When submodules were updated, create a pull request with the updates and any associated changes.

## Exit conditions

- Exit if the submodules are up to date

> NOTE: Never make direct pushes to the main branch. Always create a pull request.
