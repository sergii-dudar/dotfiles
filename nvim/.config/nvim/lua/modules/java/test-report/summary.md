# Test Report Module — Developer Summary

## Overview

Test results (JUnit XML for Java) are parsed into a flat `container_id#member` map, then
shown as gutter signs + EOL virtual text and error diagnostics in the test source buffers,
as a Trouble list (`junit_diagnostics`), in a sidebar **tree view**, and in a scratch split
showing stdout / stderr / stacktrace for one test.

The implementation is split into a **language-agnostic core** and **per-language adapters**:

- `lua/modules/common/test-report/` — core orchestration, sign/diagnostic placement, output
  split, tree view. Knows nothing about Java.
- `lua/modules/java/test-report/` — the Java adapter (JUnit XML parser + class→file
  resolution + treesitter positions). Other languages live next to it
  (`modules/<lang>/test-report/`) and follow the same contract.

**Entry point**: Overseer component `test_report.junit_report`
(`lua/overseer/component/test_report/junit_report.lua`) calls `core.process()` when a test
task completes. **Keymaps** (`lua/plugins/overseer/init.lua`, routed through
`plugins/overseer/test-report-dispatcher.lua` by filetype): `<leader>tv` tree view,
`<leader>to` / `<leader>tO` show / hide output, `<leader>tL` load the report from disk,
`<leader>txx` Trouble toggle, `<leader>txd` diagnostics picker.

---

## File Structure

```
lua/modules/common/test-report/
├── init.lua          — Core: process/clear/cancel, signs + diagnostics, output split, snapshot
├── report-view.lua   — Tree view (neotest-style layout), keymaps, running marks
├── registry.lua      — filetype -> LangAdapter registry
└── types.lua         — Shared ---@class annotations (LangAdapter, TestResult, Snapshot, ...)

lua/modules/java/test-report/
├── init.lua          — Shim: registers lang/java.lua with the registry, re-exports the core
├── junit-xml.lua     — JUnit XML -> TestResult (parameterized merge, error/line extraction)
├── lang/java.lua     — Java LangAdapter: class index, treesitter positions, id display
└── summary.md        — This file

lua/overseer/component/test_report/*_report.lua — one component per language (identical shape)
lua/lib/xml/                                   — XML parser used by all XML-based adapters
```

---

## Core Types (`common/test-report/types.lua`)

```lua
---@class test_report.TestResult
---@field status "passed"|"failed"|"skipped"
---@field errors? { message: string, line: number|nil }[]
---@field time? number
---@field invocations test_report.Invocation[]      -- one per XML testcase (parameterized runs)

---@class test_report.LangAdapter
---@field parse_results       fun(dirs: string[]): table<string, TestResult>
---@field id_to_file          fun(container_id: string, report_dir: string): string|nil
---@field find_test_positions fun(file: string, opts?: FindOpts): table<string, number>, number|nil
---@field extract_error_line  fun(container_id: string, stacktrace: string): number|nil
---@field get_test_report_dir fun(): string|string[]
---@field id_to_display       fun(id: string): { container: string, member: string, group?: string }
---@field group_separator     string    -- "." for java, "::" for rust
---@field diagnostic_source   string    -- vim.diagnostic `source`
---@field trouble_source?     string    -- e.g. "junit_diagnostics"
---@field display_name?       string    -- tree-view header label ("JUnit")
---@field clear_cache?        fun()

---@class test_report.Snapshot   -- what the tree view consumes
---@field results table<string, TestResult>          -- accumulated, keyed "container#member"
---@field positions table<string, table<string, number>>  -- file -> member key -> 0-based line
---@field container_files table<string, string>      -- container_id -> abs file path
---@field filetype string|nil
```

Result ids are `fully.qualified.ClassName#methodName`. Parameterized invocations
(`method(int)[1]`, `[2]`, …) are merged into one result with several `invocations`; the
result is `failed` if any invocation failed. Nested classes keep the `$`:
`com.foo.OuterTest$Inner#method` — a **container** is a class, not a file.

---

## Core (`common/test-report/init.lua`)

### State (module-level locals)

| Variable | Purpose |
|---|---|
| `process_generation` | Monotonic counter; an in-flight `process()` aborts if a newer one started |
| `last_results` | **Accumulated** results across runs (`{ [id] = TestResult }`) |
| `last_positions` | `{ [file_path] = { [member_key] = 0-based line } }` from treesitter |
| `last_container_files` | `{ [container_id] = abs file }` |
| `last_filetype` | Filetype of the last processed run (adapter lookup) |
| `signed_buffers` | Buffers holding our sign extmarks (for cleanup) |
| `output_bufnr` / `output_method` | Scratch output buffer + identity of the shown test (`file#member_key`) |
| `ns_diag` / `ns_signs` / `ns_output` | Namespaces `test_report_diag`, `test_report_signs`, `test_report_output` |

### `process(report_dir, filetype)` — placement grouped by FILE

1. Bump `process_generation`, cancel the spinner; run under `nio.run` + `nio.scheduler()`.
2. `adapter.parse_results(dirs)` → merge into `last_results` (key by key). Empty result set
   → WARN notification, spinner stopped, **tree view running marks cleared**, return.
3. `affected_containers` = containers present in *this* run. Each is resolved with
   `adapter.id_to_file` and recorded in `last_container_files`; the resolved paths form
   `affected_files`.
4. `by_file` = every accumulated result whose container maps to an affected file, grouped
   `file → container → member`. This is the load-bearing step: **one source file can host
   several containers** (JUnit `@Nested` classes, Rust `mod` blocks), and a partial rerun of
   one container must not erase its siblings' marks. Grouping by container (the previous
   design) cleared the whole buffer once per container, so only the last one survived.
5. Per file: `find_test_positions` once, clear signs + diagnostics once, re-place every
   member of every container, then one file-level sign on the outermost class line
   (failed if any member failed). Error lines from stacktraces are clamped to the buffer;
   outside → the method line.
6. Close the Overseer output, open Trouble if **any accumulated** result failed, stop the
   spinner with the current run's counts, `report-view.refresh_if_open(snapshot)`.

`config.load_buffers` / `config.load_only_buffers_with_error` decide whether buffers are
loaded silently (`noautocmd bufload`, no JDTLS attach) or fully.

### `cancel()` vs `clear()`

| | `cancel()` | `clear()` |
|---|---|---|
| Called by | Overseer `on_reset` / `on_dispose`, component on `CANCELED` | `load_existing()` (explicit user action) |
| Generation bump + spinner cancel | yes | yes |
| Accumulated state | **kept** | wiped (results, positions, files, adapter caches, output window) |
| Trouble | closed | untouched |
| Tree view | `clear_running()` — drops running marks | `reset_if_open()` — empties the view's copy |

`cancel()` is what makes incremental diagnostics survive reruns; keep component hooks on it.
The view sets its running marks only *after* the previous task is disposed (nio.run executes
synchronously up to the first yield), so the `clear_running()` in `cancel()` never races a
rerun started from the view.

### Output split

`show_test_output()` (source buffer, method above cursor) and `show_output_for(name, result,
{ file_path, member_key })` (tree view) render `Test / Status / Time`, then per invocation the
JUnit metadata block, stdout, stderr, stacktrace, into a scratch split
(`utils.buffer-util.open_scratch_split`). Calling `show_test_output()` again for the test
already shown toggles the split closed.

---

## Tree view (`common/test-report/report-view.lua`)

### Layout (neotest summary style)

```
 JUnit   42   40   2   0                              12.34s
 
 ├╮  com.example.service
 │├╮  UserServiceTest                                    2.10s
 ││├─  testCreate                                       500ms
 ││╰─  testDelete                                       1.20s
 │╰─  OrderServiceTest (2)                               1.11s      <- collapsed: test count
 ╰╮  com.example.api
  ╰╮  HealthCheckTest                                   100ms
   ╰─  testPing                                         100ms
```

- Header: adapter `display_name` (fallback: filetype), then icon+count for total / passed /
  failed / skipped (+ running while a rerun is in flight), total time right-aligned.
- Connectors `├ ╰ │` with a one-column indent per level; `╮` marks an expanded node, `─` a
  collapsed node or a leaf; collapsed nodes show their test count.
- Status icon in a fixed column right after the connector; names: group `TestReportGroup`
  (→ Directory), container `TestReportContainer` (→ Type), failed member
  `TestReportMemberFailed` (→ DiagnosticError). Durations right-aligned in
  `TestReportTime` (→ Comment); `≥1s` as `1.23s`, else `NNNms`, none for 0.
- All highlight groups are `TestReport*` **default links** (re-applied on `ColorScheme`), so a
  colorscheme/user can override them and nothing depends on scheme-specific groups.
- Icons are built with `vim.fn.nr2char(<codepoint>)`: passed `U+EAB2`, failed `U+EAB8`,
  skipped `U+EB32`, running `U+EB37`, total `U+EA79` — same as the gutter signs.

### Config

`report_view.setup({ width = 65, collapse_passed = false })`. `width` is the fixed split
width (re-asserted on `WinClosed`/`WinResized` so dap-ui panels can't grow it; this also
undoes a manual resize — change `width` instead). `collapse_passed = true` starts containers
without failures collapsed.

### State (singleton)

```lua
state = { bufnr, winid, prev_winid, tree, line_map, snapshot, running, adapter }
```

`open()` deep-copies `results` and `container_files`; `refresh_if_open()` merges the core's
snapshot in and rebuilds the tree **preserving expansion state** (keyed `grp:<full_path>` /
`cnt:<container_id>`). `refresh()` re-seats the cursor on the same node (by id) after a
re-render, so failed-first re-sorting doesn't move the cursor to another test.

### Keymaps (buffer-local)

| Key | Action |
|---|---|
| `<CR>` / `o` / `gd` | Go to source (member → its line, via the id's member key) |
| `O` | Show test output |
| `r` / `R` | Re-run / debug at cursor level (member → `CURRENT_TEST`, container → `FILE_TESTS`, group → `ALL_DIR_TESTS` with `package_name`) |
| `<Tab>` / middle mouse | Toggle fold |
| `<leader>G` | Full refresh from the core's live state |
| `]d` / `[d` | Next / previous failed member (visible lines only) |
| `<leader>?` | Keybinding help float |
| `q` | Close |

### Running marks

`action_rerun()` sets `state.running` (set of member ids) and re-renders with the running
icon bubbling up to container/group. Cleared by `refresh_if_open()` (results arrived),
`clear_running()` (no report / canceled / processing error) and `action_full_refresh()`.

---

## `junit-xml.lua` — XML parser

- `list_report_files(dir)` → `TEST-*.xml` (logs when empty; the core notifies).
- `parse_file(path)` → `_process_testsuite`: strips `()` / `[N]` from `name` to form
  `classname#method`, merges parameterized invocations, splits `<system-out>` into JUnit
  metadata (first tag) and real stdout (rest), extracts `<failure>` / `<error>` message +
  stacktrace + error line.
- `_extract_error_line(classname, stacktrace)` matches `<OuterSimpleName>.java:<N>` — the
  `$Inner` part is dropped because stack frames name the outer source file. Shared with the
  adapter (`lang/java.extract_error_line` delegates here).
- `message_from_stacktrace` is a safety net for a missing/empty `message` attribute. It used
  to cover a `lib/xml` bug (raw `>` inside attribute values, e.g. Jupiter's
  `message="expected: &lt;2> but was: &lt;1>"`); that is fixed in
  `lib/xml/parser.lua` (`_ATTRERR1/_ATTRERR2` — Lua patterns have no lazy `+?`).

---

## `lang/java.lua` — Java adapter

| Function | Description |
|---|---|
| `id_to_file(classname, report_dir)` | Strips `$Inner`, derives the module root from `<root>/<target|build>/junit-report`, looks the `com/foo/Bar.java` suffix up in the class index. Falls back to `java-common.java_class_to_proj_path` (first line of a possibly multi-match glob). |
| class index | One pruned `vim.fs.dir` walk per module root (skips `target build bin out node_modules .git .idea .gradle .mvn`; follows symlinked dirs once). ~8x faster than `glob("**/*.java")` and immune to source copies under `target/` shadowing real files. Duplicate suffixes resolve to the shallowest path. A **miss on a new suffix rebuilds the index once** (test class created after the first run); repeated misses are remembered until `clear_cache()`. |
| `find_test_positions(file, opts)` | `bufadd` + (silent) `bufload`, cached treesitter query over `@Test @ParameterizedTest @TestFactory @CartesianTest @RepeatedTest @TestTemplate`. Returns `{ method -> 0-based line }` and the **outermost** class line. |
| `id_to_display(id)` | `com.foo.Bar$Inner#m` → group `com.foo`, container `Bar$Inner`, member `m`. |
| `get_test_report_dir()` | `java-common.get_build_layout(module).report_dir` (maven `target/`, gradle `build/`). |

Known limitation: positions are keyed by bare method name, so two `@Nested` classes with a
method of the same name share one line (last treesitter capture wins).

---

## Overseer components (`lua/overseer/component/test_report/*_report.lua`)

| Hook | Action |
|---|---|
| `on_complete(status)` | `CANCELED` → `cancel()` and return (stale XML must not be shown as fresh); otherwise `vim.schedule(process(report_dir, filetype))` |
| `on_reset` / `on_dispose` | `cancel()` — never `clear()` |

`overseer-task-util.stop_all_prev_tasks()` disposes previous tasks before a new run; with
`clear()` here, accumulated state would be lost on every rerun.

---

## Call flow

```
keymap / tree-view rerun
  → overseer-util.run_test(context)
    → stop_all_prev_tasks() → dispose → on_dispose → core.cancel()  (Trouble closed, running marks kept for the new run)
    → task runs (junit console jar) → on_complete
      → core.process(report_dir, ft)      [nio.run]
        → adapter.parse_results → merge into last_results
        → resolve affected containers → files; group ALL accumulated results by file
        → per file: positions, clear once, place signs/diagnostics, file-level sign
        → overseer.close(); Trouble open if any accumulated failure
        → report-view.refresh_if_open(snapshot)
```

---

## Testing

- Busted specs (`make test` from the nvim config root; `vim` is a test double, so only pure
  logic is covered): `lua/tests/modules/java/test-report/junit_xml_spec.lua`,
  `lang/java_spec.lua` (index walk, rebuild-on-miss, nested error lines),
  `lua/tests/modules/java/junit/init_spec.lua`, `lua/tests/lib/xml_parser_spec.lua`.
- The placement loop and the view need a real Neovim: run
  `nvim --headless -u NONE -l script.lua` with rtp += this config, `nvim-nio`,
  `~/.local/share/nvim/site` (treesitter parsers), stub `Snacks.notifier`, `overseer`,
  `utils.java.java-common` and a no-op `:Trouble` command, write a fixture
  `src/test/java/...` + `target/junit-report/TEST-*.xml`, call `process()` and inspect the
  `test_report_diag` / `test_report_signs` namespaces.

---

## Gotchas

1. **Group by file, not container** in `process()` — see above; this is what keeps `@Nested`
   classes and partial reruns correct.
2. **`cancel()` vs `clear()`** — component hooks use `cancel()`; only explicit user actions
   call `clear()`.
3. **Snapshot ownership** — the view merges incrementally; `clear()` must call
   `reset_if_open()` or a clear+process shows the union of stale and fresh results.
4. **Positions are keyed by the id's member part**, not the display name (jest uses `L<row>`);
   goto/rerun/output in the view resolve through `member_pos_key()`.
5. **Nerd-font icons** are codepoints via `nr2char`; don't paste glyph literals.
6. **`_G.task`** (`plugins/overseer/init.lua`) holds the `test_type` enum used by reruns.
7. StyLua: `~/.local/share/nvim/mason/packages/stylua/stylua` with the repo `stylua.toml`.
