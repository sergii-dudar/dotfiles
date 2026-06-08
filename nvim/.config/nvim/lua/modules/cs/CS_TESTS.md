# C# test integration

Run / debug C# tests (xunit, nunit, mstest) through the shared overseer +
test-report pipeline, with signs, diagnostics, tree view and stdout/stderr
output — same UX as the Java/Rust integrations.

## How it works

- **Runner**: `dotnet test` with the built-in **TRX** logger
  (`--logger "trx;LogFileName=..." --results-directory <root>/TestResults/nvim`).
  TRX is uniform across xunit / nunit / mstest (all run on the VSTest platform),
  so **no per-project NuGet packages are required**.
- **Discovery**: treesitter (`modules/cs/cs-ts-util.lua`) finds the test method
  under the cursor / test classes in a file and builds a
  `--filter "FullyQualifiedName~..."` expression. A method counts as a test when
  it carries a known attribute: `Fact`/`Theory` (xunit), `Test`/`TestCase`/
  `TestCaseSource` (nunit), `TestMethod`/`DataTestMethod` (mstest).
- **Report**: `modules/cs/test-report/` parses the TRX (`trx-xml.lua`) and feeds
  `modules/common/test-report`. Parametrized cases (xunit `[Theory]`,
  nunit `[TestCase]`, mstest `[DataRow]`) are aggregated into one test with
  multiple invocations. Failure messages + `:line N` from the stacktrace become
  diagnostics.

## Requirements

- `dotnet` SDK on `PATH`.
- `netcoredbg` (installed via Mason by the LazyVim dotnet extra) — for debugging.
- `c_sharp` treesitter parser.

## Keymaps (shared `<leader>t…`)

| Key          | Action                                                        |
|--------------|---------------------------------------------------------------|
| `<leader>tt` | Run test under cursor                                         |
| `<leader>td` | Debug test under cursor (attaches netcoredbg to testhost)    |
| `<leader>tf` | Run all tests in the current file                            |
| `<leader>tF` | Debug all tests in the current file                         |
| `<leader>ta` | Run all tests (nearest `.sln`, else current project)        |
| `<leader>tp` | Run current parametrized method (all data rows — see below) |
| `<leader>tm` | Run tests in selected projects (multi-select)               |
| `<leader>tM` | Run tests in all projects                                    |
| `<leader>tl` | Re-run last test (keeps regular/debug mode)                 |
| `<leader>tD` | Toggle last test between regular and debug                  |
| `<leader>tv` | Test report tree view                                        |
| `<leader>to` | Toggle test output (stdout/stderr/stacktrace)               |
| `<leader>txx`| Test diagnostics in Trouble                                  |

## Test-type mapping

| Type                 | Behaviour                                              |
|----------------------|-------------------------------------------------------|
| CURRENT_TEST         | `--filter FullyQualifiedName~<Class>.<Method>`        |
| FILE_TESTS           | OR of `FullyQualifiedName~<Class>` for classes in file|
| ALL_TESTS / MODULES  | `dotnet test` at the `.sln` dir (else current project)|
| ALL_DIR_TESTS        | the `.csproj` owning the current file                 |
| SELECTED_MODULES     | pick test projects, run each (chained)                |

## Debugging

Debug tests run as an overseer `DEBUG_TESTS` task with `VSTEST_HOST_DEBUG=1`.
The testhost prints its PID and waits. The generic
`overseer/component/debug/dap_ctrl_component.lua` resolves the language's
`dap_output_attacher` (declared on `cs-runner.lua`), which matches the
`Process Id: N, Name: testhost` banner and attaches **netcoredbg**.
This mirrors Java's JDWP flow (Java declares its own `dap_output_attacher`), so
`<leader>tl` (re-run last in debug) and
`<leader>tD` (toggle regular/debug) work for C# the same as everywhere else.
The TRX report is still produced after the debug session, so signs/diagnostics
update on completion.

> Debug is supported for single test / file / single-project runs. It is disabled
> for multi-project (`<leader>tm`/`<leader>tM`) runs (multiple testhosts).

## Notes / limitations

- `<leader>tp` (parametrized single) runs the **whole** parametrized method
  (= every data row). Selecting an individual data row is framework-specific and
  unreliable across the three frameworks, so it is intentionally not attempted.
- Reports are written to `<run-root>/TestResults/nvim/` (cleared before each run).
