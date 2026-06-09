## Context information for JUnit Implementation

### Overview

The Java JUnit test integration is the **Java adapter** of a now language-agnostic
test-report framework built on overseer.nvim. A test run writes JUnit XML to
`<module>/target/junit-report`; the adapter parses it and renders results as gutter
signs, EOL virtual text, `vim.diagnostic` entries (source `junit`), a Trouble view,
a tree sidebar, and a detailed output panel with Java stack-trace highlighting.

The same pipeline now serves Rust, Go, Python, Bash, Lua, C#, JS/TS via their own
adapters — Java/JUnit is just one registered language. See
[`REGISTERING_NEW_MAIN_LANG_INFO.md`](./REGISTERING_NEW_MAIN_LANG_INFO.md) for the
per-language picture.

### Plugins Used

- **overseer.nvim** — task runner framework (run / test / debug templates + components)
- **nvim-nio** — async runtime (parsing / IO runs inside nio)
- **trouble.nvim** — failure diagnostics (`Trouble junit_diagnostics ...`)
- **snacks.nvim** — diagnostics picker (filtered by `source == "junit"`)

> The custom runner **replaces** `neotest` / `neotest-java` (kept only under
> `archive/` for reference).

### Key Files

**Generic test-report core (filetype-agnostic)**

- `lua/modules/common/test-report/` — `init.lua` (orchestrator: parse → signs /
  diagnostics / output panel / tree), `registry.lua` (filetype → `LangAdapter`),
  `report-view.lua` (tree sidebar), `types.lua` (the `LangAdapter` contract)

**Filetype dispatcher (the test-report keymaps route through here)**

- `lua/plugins/overseer/test-report-dispatcher.lua` — resolves the current filetype's
  adapter + Trouble/diagnostic source and forwards `show/hide output`, `load_existing`,
  `tree view`, `trouble`, `diagnostics picker`

**Java adapter**

- `lua/modules/java/test-report/init.lua` — thin shim: registers the Java adapter with
  the core (`registry.register("java", …)`) and re-exports the core API
- `lua/modules/java/test-report/junit-xml.lua` — JUnit XML parser (parses `TEST-*.xml`,
  merges parameterized invocations, extracts errors)
- `lua/modules/java/test-report/lang/java.lua` — Java `LangAdapter` (classname→file,
  treesitter test positions, error-line extraction)

**JUnit command builders**

- `lua/modules/java/junit/init.lua` — builds `junit-platform-console-standalone`
  commands (classpath from jdtls, parametrized signatures via `javap`)
  *(moved here from the old `lua/utils/java/junit/`)*

**Overseer glue**

- `lua/overseer/component/test_report/junit_report.lua` — overseer component that hands
  XML output to the Java adapter on task completion
- `lua/plugins/overseer/tasks/run_tests.lua` — RUN_TESTS / DEBUG_TESTS templates;
  attaches the right `test_report.*_report` component by filetype
- `lua/plugins/overseer/tasks/lang/java-runner.lua` — Java runner; the test command
  delegates to `modules.java.junit.init.build_run_test_cmd`
- `lua/plugins/overseer/init.lua` — overseer setup, `_G.task` enums, and all
  `<leader>r*` / `<leader>t*` keymaps
- `lua/plugins/overseer/overseer-util.lua`, `overseer-task-util.lua` — run / debug /
  stop orchestration + task lifecycle

**Java support utilities**

- `lua/utils/java/java-trace.lua` — Java stack-trace highlighting + navigation in the
  output buffer (`find_edit_win()` opens files in a normal editing window)
- `lua/utils/java/java-common.lua` — `get_buffer_project_path`, class/path helpers,
  test-file detection
- `lua/utils/java/java-ts-util.lua` — treesitter helpers (class name, method signature)
- `lua/utils/java/jdtls-util.lua`, `jdtls-classpath-util.lua` — jdtls helpers + classpath
- `lua/utils/lsp-util.lua` — LSP client lookup (`get_client_by_name` / `get_client_id_by_name`)
- `lua/lib/xml/` — vendored xml2lua-based XML parser

### Architecture Flow

```
Keybindings (<leader>t* / <leader>r*)            [plugins/overseer/init.lua]
  -> overseer-util.lua            (orchestration: run / debug / stop)
    -> overseer-task-util.lua     (task lifecycle: run / stop / restart)
      -> run_tests.lua            (RUN_TESTS / DEBUG_TESTS templates)
        -> junit_report.lua       (overseer component, picked by filetype)
          -> test-report-dispatcher.lua    (filetype -> language adapter)
            -> modules/common/test-report   (generic core: signs / diags / panel / tree)
                 |-- registry -> modules/java/test-report/lang/java.lua  (Java adapter)
                 |-- junit-xml.lua          (XML parsing)
                 |-- java-trace.lua         (output-buffer trace highlighting)
```

### Key Design Details

- Generic core + per-language `LangAdapter` registered by filetype (`registry.lua`);
  Java is one adapter — Rust / Go / Python / Bash / Lua / C# / JS each register their own.
- Treesitter-based test position detection (`@Test`, `@ParameterizedTest`,
  `@TestFactory`, `@CartesianTest`).
- Parameterized tests are merged under `classname#methodName`, individual invocations
  preserved.
- First `<system-out>` in JUnit XML is treated as JUnit5 metadata; the rest is real stdout.
- Report directory convention: `<module>/target/junit-report`.
- Results displayed via: extmark signs + EOL virtual text, `vim.diagnostic`
  (source `junit`), quickfix; failures open Trouble `junit_diagnostics`; tree view via
  `<leader>tv`.
- Summary notification: "🚀 N Tests Passed" or "🚫 Tests Finished with failed X/Y tests".
- Output buffer has Java stack-trace highlighting (project classes vs external);
  extmarks use `line_hl_group` (not the deprecated `nvim_buf_add_highlight`).
- Stop-all (`<leader>ts` / `<leader>rs`) stops running overseer tasks and terminates the
  DAP session independently.

### Snacks Picker Integration

- `Snacks.picker.diagnostics` supports a `severity` option (passed to `vim.diagnostic.get`).
- `filter.filter` is `fun(item, filter): boolean`; `item.item` is the raw
  `vim.Diagnostic`, so `item.item.source == "junit"` filters by source.
- Wired via `<leader>txd` → `test-report-dispatcher.picker_diagnostics()`.

### Keybindings — code runner (`<leader>r*`)

- `<leader>rr` — Run Current  (visual mode: Run Selected Lua)
- `<leader>rd` — Debug Current
- `<leader>rl` — Re-Run Last
- `<leader>rD` — Toggle Debug of Last Run Cmd
- `<leader>ro` — Task list
- `<leader>rt` — Task action
- `<leader>rs` — Stop All (overseer tasks + DAP)

### Keybindings — tests runner (`<leader>t*`)

- `<leader>tt` — Run Current Test
- `<leader>td` — Debug Current Test
- `<leader>tD` — Toggle Debug of Last Test Cmd
- `<leader>tf` — Run File Tests · `<leader>tF` — Debug File Tests
- `<leader>ta` — Run All Tests
- `<leader>tp` — Run Current Parametrized Single Test · `<leader>tP` — Debug it
- `<leader>tm` — Run All Tests in Selected Modules · `<leader>tM` — in All Modules
- `<leader>tl` — Re-Run Last
- `<leader>ts` — Stop All (overseer tasks + DAP)
- `<leader>to` — Toggle Test Output (stack-trace highlighted) · `<leader>tO` — Hide
- `<leader>tL` — Load Last Test Report
- `<leader>tv` — Test Report Tree View
- `<leader>txx` — Tests diagnostics (Trouble) · `<leader>txd` — Tests diagnostics (picker)

> `_G.task.test_type` enum: `ALL_TESTS`, `FILE_TESTS`, `CURRENT_TEST`,
> `CURRENT_PARAMETRIZED_NUM_TEST`, `ALL_DIR_TESTS`, `ALL_MODULES_TESTS`,
> `SELECTED_MODULES_TESTS`, `TOGGLE_LAST_DEBUG`.
