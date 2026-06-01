# Rust test integration

Adds Java-parity test run/debug/report capabilities to Rust:

- `<leader>tt` — run test at cursor
- `<leader>td` — debug test at cursor (codelldb)
- `<leader>tf` — run all tests in current file
- `<leader>ta` — run all tests in current package
- `<leader>tm` / `<leader>tM` — run tests in selected / all workspace packages
- `<leader>tl` — re-run last
- `<leader>tD` — toggle last between debug / non-debug
- `<leader>to/tO/tL/tv/txx/txd` — show/hide/load output, tree view, Trouble, picker

After a test run completes, pass/fail signs appear at each test fn line and an
aggregated container summary at the enclosing `mod` line (or line 1 for
file-scope integration tests).

## Required dependencies

| Tool             | Purpose                                          | Install                                              |
|------------------|--------------------------------------------------|------------------------------------------------------|
| `cargo-nextest`  | Test runner. Produces JUnit XML reports.         | `cargo install cargo-nextest --locked`               |
| `rust-analyzer`  | LSP. Used for `experimental/runnables` to select | Bundled via Mason / `rustup component add rust-analyzer` |
|                  | the precise test at cursor / in file.            |                                                      |
| `codelldb`       | DAP adapter. Used for `<leader>td` test debug.   | Bundled via Mason                                    |

`cargo test` (the stdlib runner) is **not used**. We always run `cargo nextest run`
because it emits structured JUnit XML, which our pipeline parses for signs,
diagnostics, and the test tree view. If `cargo nextest` is missing, the
integration aborts with a clear install message instead of silently falling back.

## How it works

1. **Test discovery & cmd construction** (`lua/modules/rust/cargo-test/init.lua`):
   - For `CURRENT_TEST` / `FILE_TESTS`: queries rust-analyzer
     `experimental/runnables` at cursor / file. The exact `cargo test ...` args
     it returns are translated to `cargo nextest run -E 'test(=<name>)' ...`.
   - For `ALL_DIR_TESTS`: derives current package from `cargo metadata`, runs
     `cargo nextest run -p <pkg>`.
   - For `ALL_*` / `SELECTED_MODULES_TESTS`: enumerates workspace members from
     `cargo metadata`, runs nextest per package or `--workspace`.

2. **JUnit XML emission** is injected non-intrusively via:
   ```bash
   cargo nextest run \
     --tool-config-file copilot-cli:~/.cache/nvim/test-report/nextest.toml ...
   ```
   The tool-config file (auto-created) sets `[profile.default.junit] path = "junit.xml"`.
   `--tool-config-file` *composes* with the user's `.config/nextest.toml` (user
   config wins), so existing repo configs are not clobbered.

3. **Report parsing** (`lua/modules/rust/test-report/nextest-xml.lua`):
   nextest writes `<workspace_root>/target/nextest/default/junit.xml`. The
   overseer `cargo_report` component triggers parsing on task completion and
   feeds results into the generic `lua/modules/common/test-report` core.

4. **Test ID format**: `<binary_id>::<module_path>#<fn_name>`
   - `binary_id` = `<crate>` for lib, `<crate>$<name>` for `src/bin/<name>.rs`,
     `<crate>::<name>` for `tests/<name>.rs`
   - File resolution uses a workspace index built from `cargo metadata` +
     treesitter, cached per session.

5. **Debug** (`<leader>td`): bypasses overseer entirely.
   - `cargo test --no-run --message-format=json` to compile + locate test binary.
   - `dap.run({ type = "codelldb", program = <test-bin>, args = { '-E', 'test(=...)' } })`
   - Note: debug runs **do not refresh marks** (the test binary doesn't write
     JUnit XML — only nextest does). Use `<leader>tt` non-debug to refresh.

## Limitations

- **Parametrized single-case selection** (`<leader>tp`): not implemented. For
  `rstest` / `test-case` / `parameterized` crates, each generated case is a
  separate `#[test]` fn — `<leader>tt` with cursor on the case usually picks
  the right one via rust-analyzer runnables.
- **Container summary sign**: placed on the `mod` block line if one exists,
  otherwise on line 1 of the file (typical for integration tests).
- **Restart persistence**: nextest overwrites `junit.xml` on every run, so
  `<leader>tL` after nvim restart only restores the most recent run's results.
  Cross-run accumulation works while nvim stays open.
