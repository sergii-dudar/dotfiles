# Go Test Integration

Java-parity test run/debug/report support for Go, built on top of the shared
`modules/common/test-report/` core.

## Required dependencies

| Tool | Purpose | Install |
|------|---------|---------|
| **go** | Toolchain providing built-in `go test -json` streamed events | https://go.dev/dl/ |
| **delve** (`dlv`) | DAP adapter — required for `<leader>td` (debug test) | `go install github.com/go-delve/delve/cmd/dlv@latest` |
| **nvim-dap-go** | Optional, used only if you want Go DAP types preconfigured | Already part of LazyVim Go extra |

If `dlv` is missing, debug-test keymaps will print an install hint.

## How it works

* All test runs go through `go test -json` and pipe NDJSON into:

  ```
  $XDG_CACHE_HOME/nvim/test-report/go/<safe-module-path>/go-test.json
  ```

  The path lives **outside** your repo so nothing needs to be `.gitignore`d.

* The JSON event stream is parsed by `modules/go/test-report/json-parser.lua`
  into `test_report.TestResult` records keyed `<package-import-path>#<TestFn>`.

* Subtests (`t.Run("name", ...)`) are aggregated under the parent test as
  multiple `invocations` — same approach used for Rust parametrized cases.

* Tree view / signs / Trouble diagnostics all come from the shared core
  (`modules/common/test-report`).

## Supported test types

The standard `<leader>t*` keymaps work with these `task.test_type` values:

| Test type | Behavior |
|-----------|----------|
| `ALL_TESTS` / `ALL_MODULES_TESTS` | `go test -json ./...` from module root |
| `ALL_DIR_TESTS` | `go test -json ./<current-dir>/...` |
| `FILE_TESTS` | `go test -json -run '^(T1\|T2\|…)$' [-bench '^(B1\|…)$' -benchtime=1x] ./<pkg>/` |
| `CURRENT_TEST` | Same as above, scoped to one test/bench/example/fuzz at cursor |
| `SELECTED_MODULES_TESTS` | Prompts (multi-select) between `go.work` use entries |
| `CURRENT_PARAMETRIZED_NUM_TEST` | Not implemented — Go subtests are aggregated automatically |
| `TOGGLE_LAST_DEBUG` | Re-runs the last single test or file scope |

### Function-kind handling

| Kind | Signature | Flag used |
|------|-----------|-----------|
| `TestXxx` | `func TestXxx(t *testing.T)` | `-run` |
| `BenchmarkXxx` | `func BenchmarkXxx(b *testing.B)` | `-bench` (+ `-benchtime=1x`) |
| `FuzzXxx` | `func FuzzXxx(f *testing.F)` | `-run` (executes the seed corpus only, no fuzzing) |
| `ExampleXxx` | `func ExampleXxx()` (no params; validated by `// Output:` comment) | `-run` |
| `TestMain` | `func TestMain(m *testing.M)` | **Skipped** — it's the package setup hook, not a runnable test |

## Debug

`<leader>td` (debug current test) uses **delve** via `nvim-dap`. Supported
for: `CURRENT_TEST`, `FILE_TESTS`, `TOGGLE_LAST_DEBUG`. Other test types will
warn — debugging entire packages is rarely meaningful.

The launched DAP config is:

```lua
{ type = "go", request = "launch", mode = "test",
  program = <pkg dir>, args = { "-test.run", "^TestName$" } }
```

## Test discovery

A test is detected when **both** hold:

1. Function name matches `^Test`, `^Benchmark`, or `^Example`.
2. First parameter type is `*testing.T`, `*testing.B`, or `*testing.F`.

This means `TestMain(m *testing.M)` is **not** treated as a runnable test
(it's the package setup hook). Helpers that take `*testing.T` but don't
start with `Test` are also excluded.

## Caveats and future work

* **Testify suites** (`suite.Run(t, &MySuite{})`) — not yet detected as
  individual `TestMethod` entries. Add to the wishlist if you start using
  them heavily.
* **Multi-module `go.work` aggregation** — `SELECTED_MODULES_TESTS` runs one
  `go test` per module sequentially and concatenates report files. The
  parser tolerates multiple JSON files per dir, but each module gets its
  own report dir.
* **Parametrized single-case run** (`CURRENT_PARAMETRIZED_NUM_TEST`) — not
  implemented. Subtests are aggregated under the parent test instead.

## Files

* `modules/go/go-test/init.lua` — command builder + DAP launcher
* `modules/go/test-report/json-parser.lua` — NDJSON → TestResult
* `modules/go/test-report/lang/go.lua` — language adapter (index, treesitter)
* `modules/go/test-report/init.lua` — shim that registers the adapter
* `lua/overseer/component/test_report/go_report.lua` — overseer component
* `lua/plugins/overseer/tasks/lang/go-runner.lua` — runner contract glue
