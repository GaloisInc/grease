# Ghidra Script Development with Type Stubs

Partial autocomplete can be enabled for the ghidra script (ghidra_scripts/grease.py) by installing the upstream distributed
[python stubs](https://pypi.org/project/ghidra-stubs/).

Using [virtualenvwrapper](https://virtualenvwrapper.readthedocs.io/en/latest/) one can setup the stubs by running:

```sh
mkvirtualenv ghidradev
workon ghidradev
pip install ghidra-stubs
```

Editors that support type stubs will be able to use this virtualenv to provide partial autocomplete support.

## Linting

See the [linting section of the dev docs](dev.md) for discussion of how to lint this script with ruff.
