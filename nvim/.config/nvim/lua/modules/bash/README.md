# Bash test integration (bashunit)

Adds Java-parity test run / debug / report capabilities for Bash projects using
[`bashunit`](https://bashunit.typeddevs.com/) as the test framework.

## Keymaps

- `<leader>tt` — run test under cursor (`bashunit <file> --filter <fn>`)
- `<leader>td` — debug test under cursor (bash-debug-adapter)
- `<leader>tf` — run all tests in current file
- `<leader>ta` — run all tests in current file's directory
- `<leader>tm` / `<leader>tM` — run all project tests (aliased to ALL_TESTS)
- `<leader>tl` — re-run last
- `<leader>tD` — toggle last between debug / non-debug
- `<leader>to/tO/tL/tv/txx/txd` — show/hide/load output, tree view, Trouble, picker

After a run finishes, pass/fail signs appear at each `test_*` function line and
an aggregated summary sign at the top of the file.

## Required dependencies

| Tool                 | Purpose                                              | Install |
|----------------------|------------------------------------------------------|---------|
| `bashunit`           | Test framework. Emits JUnit XML via `--log-junit`.   | https://bashunit.typeddevs.com/installation — or vendor as `lib/bashunit` in the project |
| `bash-debug-adapter` | DAP adapter for `<leader>td`. Only needed for debug. | `:MasonInstall bash-debug-adapter` |
| Tree-sitter `bash`   | Used to discover `test_*` functions and to reverse bashunit's humanized test names back to function names. | `:TSInstall bash` |

`bashunit` binary resolution order:

1. `constants.bash.bashunit_bin` (manual override in `lua/utils/constants.lua`,
   default `/home/serhii/.local/bin/bashunit`) — if executable.
2. `<project_root>/lib/bashunit` or `<project_root>/bashunit`.
3. `bashunit` on `$PATH` (resolved to absolute via `exepath`).

If the configured path exists but is not executable, a warning is shown and the
auto-detect chain is used.

## Project layout assumptions

Project root is detected by walking up from the current file looking for any of:
`.bashunit`, `bashunit.conf`, `lib/bashunit`, a `tests/` directory, or `.git`.

Tests follow bashunit conventions:

- Files named `*_test.sh` or `test_*.sh` (usually under `tests/`).
- Test functions are bash functions whose name starts with `test_`.

Custom titles set via `bashunit::state::set_test_title` are not fully reverse-
mappable to function names; signs/diagnostics in that case fall back to the
humanized title as the test id.

## How it works

1. **Run** — `lua/modules/bash/bashunit-test/init.lua` builds a `bashunit`
   command tailored to the selected `task.test_type` and always appends
   `--log-junit <cache>/test-report/bash/<safe-project-path>/junit.xml`.
2. **Debug** — `dap_launch_test()` configures `bashdb` with the resolved
   absolute `bashunit` binary as the `program`, passes the same args, and
   registers a session-name-scoped `event_terminated` listener that reloads the
   JUnit report after the session ends (does not interfere with non-test
   `<leader>rd` bash debug sessions).
3. **Report** — `modules.bash.test-report` registers a `LangAdapter` with the
   shared `modules.common.test-report` core. `junit-xml.lua` parses bashunit's
   single-`<testsuite>` XML, reverses humanized test names via a per-file
   treesitter walk, aggregates data-provider invocations, and extracts a useful
   failure message from the multi-line `<failure>` body.
4. **Signs / Trouble / tree** — handled by the shared core
   (`modules.common.test-report`). Diagnostic source: `bashunit`. Trouble source:
   `bashunit_test_diagnostics`.

## Files

```
modules/bash/
├── bashunit-test/init.lua        — command builder + DAP entry point
└── test-report/
    ├── init.lua                  — registers sh/bash adapters with shared core
    ├── junit-xml.lua             — bashunit XML parser
    └── lang/bash.lua             — LangAdapter (positions, ids, diagnostics)
```

Wire-up lives in:
- `lua/plugins/overseer/tasks/lang/sh-runner.lua` — delegates test methods here.
- `lua/plugins/overseer/tasks/run_tests.lua` — maps `sh`/`bash` → `bashunit_report`.
- `lua/plugins/overseer/test-report-dispatcher.lua` — registers dispatch maps.
- `lua/overseer/component/test_report/bashunit_report.lua` — overseer component.
- `lua/plugins/editor/trouble-nvim.lua` — Trouble filter source.
- `lua/utils/constants.lua` — `M.bash` block (binary path + source names).

## Known limitations

- **Test-body breakpoints don't fire during `<leader>td`.** bashunit runs each
  test inside a `$(...)` command-substitution subshell (see
  `bashunit/src/runner.sh:871`). `bashdb`'s FIFO I/O channels are bound to the
  parent process, so breakpoints set inside test functions never trip. This is a
  long-standing bashdb limitation that also affects VSCode. Workarounds:
    - Set breakpoints in `set_up` / `set_up_before_script` /
      `tear_down_after_script` (they run in the parent shell).
    - Add `set -x` to the test for trace output.
    - Extract the logic under test into a standalone script and debug it with
      the regular `<leader>rd` runner.
- **Stop-on-launch.** `bashdb` with `request: "launch"` always halts at the
  first executable line of `program` (`bashunit` itself). There is no
  `stopOnEntry: false` switch — press Continue once after the session attaches.
