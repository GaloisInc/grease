# Ghidra script development

## Type stubs for autocomplete

Partial autocomplete can be enabled for the Ghidra script (`ghidra_scripts/grease.py`) by installing the upstream distributed
[Python stubs](https://pypi.org/project/ghidra-stubs/).

Using [virtualenvwrapper](https://virtualenvwrapper.readthedocs.io/en/latest/) one can setup the stubs by running:

```sh
mkvirtualenv ghidradev
workon ghidradev
pip install ghidra-stubs
```

Editors that support type stubs will be able to use this virtualenv to provide partial autocomplete support.

## Linting

See the [linting section of the dev docs](dev.md) for discussion of how to lint this script with ruff.

## Ghidra batch plugin development

### Formatting

The Scala files in the Ghidra batch plugin can be formatted with [scalafmt](https://scalameta.org/scalafmt/) by running `./gradlew spotlessApply` from the `ghidra-plugin` directory.
