# Refactor module regression suite

Headless, filesystem-level tests for `modules.java.refactor`. No plugin manager is loaded: Neovim runs with
`-u NONE` and only this config directory on the runtime path, so the module and its `utils.*` dependencies are
exercised exactly as in the editor, minus the terminal UI (`test_mode = true`).

```sh
./runall.sh          # every scenario; exit status = number of failures
./runall.sh s1 s3    # a subset
```

Requirements: `nvim`, `rg`, `fd`, `gsed` (macOS) or `sed`, `javac`.

| File | Role |
|---|---|
| `mkfixture.sh <dir>` | Builds a tiny Maven-like project (main + mirrored tests) at `<dir>`. |
| `driver.lua` | Performs the moves like a file manager (rename + mkdir), registers them, runs `process_registerd_changes()` in test mode. |
| `run.sh <fixture> <src> <dst>…` | One scenario: driver, then tree / package / import dump, then `javac` of main and test sources. |
| `runall.sh` | All scenarios with per-scenario assertions; also prints WARN/ERROR lines the module logged. |

Scenarios (`runall.sh`): partial move of one class out of a package (its test follows, nothing else moves),
a move that empties a package (whole test package follows and merges into an existing one), move + rename,
directory / package rename, two siblings leaving one package for different targets, same-package rename,
sub-package move, and a batch mixing a directory move with an unrelated file move.

Fixtures live under `$TMPDIR/java-refactor-tests/<scenario>` and are left in place for inspection.
The module log is `~/.local/state/nvim/java-refactor.log`.
