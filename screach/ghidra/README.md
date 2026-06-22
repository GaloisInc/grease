# Screach Ghidra plugin

A Ghidra extension that drives the
[Screach](https://github.com/GaloisInc/grease/tree/main/screach) reachability
analyzer from inside Ghidra and shows its results.

## Features

- **Set addresses by right-clicking** in the Listing, or on a function in the
  **Functions** window (which uses the function's entry point):
  <!-- [ref:screach_menu_entry] -->
  - *Screach → Set as Screach Entry (Source) Address*
  <!-- [ref:screach_menu_target] -->
  - *Screach → Set as Screach Target Address*
  <!-- [ref:screach_menu_avoid] -->
  - *Screach → Add Screach Avoid Address*

  In the Functions window, Entry and Target apply to a single selected function;
  Add Avoid applies to all selected functions.

  Entry and Target are replaced each time you set them, but avoid addresses
  accumulate; clear them all with *Screach → Clear Screach Avoid Addresses*.
  <!-- [ref:screach_menu_clear_avoid] -->

  Ghidra's image-base rebasing is undone automatically (mirroring
  `ghidra_scripts/grease.py`), so the addresses passed to Screach match the
  on-disk binary.
- **Run from a menu item or keyboard shortcut**: *Screach → Run Screach* <!-- [ref:screach_menu_run] -->, or
  **Ctrl+Alt+R** (rebindable). Screach runs as an external process off the UI
  thread and is cancellable.
- **Results window** with a colored banner (green = reached, amber = not reached,
  red = error) and streamed output. Toolbar buttons clear the log, navigate to
  the last reported address, and locate the coverage file.
- **Coverage heatmap**: with *Dump Coverage* enabled (default), Screach writes a
  `.greasecov` file next to the binary. Load it into
  [grease-cartographer](https://github.com/GaloisInc/grease-cartographer) via
  *Tools → Code Coverage → Load Code Coverage File(s)…* to see the path
  highlighted in the Listing.

## Configuration

*Edit → Tool Options → Screach*:

| Option | Description |
| --- | --- |
| Use Docker | Run `screach` via Docker instead of a local binary. |
| Binary Path | Path to the `screach` executable (when *Use Docker* is off). <!-- [ref:screach_default_binary_path] --> |
| Docker Image | Image name/tag (when *Use Docker* is on), default `ghcr.io/galoisinc/screach:nightly`. <!-- [ref:screach_default_docker_image] --> |
| Docker Run Args | Extra `docker run` args (default `--rm`). The binary's directory is mounted and used as the working directory automatically. <!-- [ref:screach_default_docker_run_args] --> |
| Extra CLI Args | Arguments appended to every invocation. |
<!-- [ref:grease_solvers] -->
<!-- [ref:screach_default_solver] -->
| Solver | Value for `--solver` (bitwuzla, cvc4, cvc5, yices, z3). |
| Dump Coverage | Pass `--dump-coverage` to produce a `.greasecov` file. |

## Building

Requires a Ghidra installation. Either enter the provided Nix shell (which sets
`GHIDRA_INSTALL_DIR` and provides `gradle`), running from this directory:

```sh
nix-shell --run "gradle distributeExtension"
```

or set `GHIDRA_INSTALL_DIR` yourself. This plugin has no Gradle wrapper of its
own, so if you don't have `gradle` on your `$PATH` you can reuse the checked-in
wrapper from `ghidra-plugin` (the same one CI uses) by running this from the
repository root:

```sh
export GHIDRA_INSTALL_DIR=/path/to/ghidra
./ghidra-plugin/gradlew -p screach/ghidra distributeExtension
```

(If you do have `gradle` available, `gradle distributeExtension` from this
directory works too.)

The extension zip is written to `dist/`, e.g.
`dist/ghidra_12.0.4_NIX_screach-ghidra.zip`. The Ghidra version in the filename
matches your `GHIDRA_INSTALL_DIR`; install into the *same* Ghidra version you
built against.

## Development

<!-- [file:doc/dev/lint.md] -->
See [doc/dev/lint.md](../../doc/dev/lint.md) for formatting and linting setup.

## Installing

1. Launch Ghidra (the project window, not a CodeBrowser).
2. *File → Install Extensions*.
3. Click **`+`** (Add extension), select the zip from `dist/`, and tick its
   checkbox.
4. Restart Ghidra when prompted.

## Enabling the plugin

The plugin does not auto-load into the CodeBrowser. After restarting:

1. Open a program in a CodeBrowser.
2. *File → Configure* → **Miscellaneous** → check **ScreachPlugin** → *OK*.

You will then have the top-level **Screach** menu (and **Ctrl+Alt+R**), the
right-click Listing items, the dockable **Screach** results window, and the
options under *Edit → Tool Options → Screach* (set the Binary Path or Docker
image first).

For the coverage heatmap, also install the separate
[grease-cartographer](https://github.com/GaloisInc/grease-cartographer)
extension the same way — that is what loads the `.greasecov` file this plugin
produces.
