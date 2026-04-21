# Test Report Module — Developer Summary

## Overview

The test-report module parses JUnit XML reports produced by test runners, displays results as gutter signs and diagnostics in source buffers, provides a tree view UI, and supports test output viewing and rerunning from both source buffers and the tree view.

**Entry point**: Overseer component `test_report.junit_report` (defined in `lua/overseer/component/test_report/junit_report.lua`) triggers `init.lua:process()` on task completion.

**Keymap**: `<leader>tv` opens the tree view, `<leader>to`/`<leader>tO` show/hide test output, `<leader>tL` loads existing report from disk. Defined in `lua/plugins/overseer/init.lua`.

---

## File Structure

```
lua/modules/java/test-report/
├── init.lua                  — Core module: orchestrates parsing, signs, diagnostics, output viewer
├── junit-report-view.lua     — Tree view UI: renders results in a scratch buffer with hierarchy
├── junit-xml.lua             — XML parser: reads JUnit XML files into TestResult structures
├── lang/
│   └── java.lua              — Java language adapter: classname→file resolution, treesitter positions
└── summary.md                — This file

lua/overseer/component/test_report/
└── junit_report.lua          — Overseer component: hooks on_complete/on_reset/on_dispose
```

---

## Core Types

```lua
---@class test_report.TestResult
---@field status "passed"|"failed"|"skipped"
---@field errors? { message: string, line: number|nil }[]
---@field time? number
---@field invocations test_report.Invocation[]

---@class test_report.Invocation
---@field name string              -- Original test name including parameterized suffix
---@field status "passed"|"failed"|"skipped"
---@field metadata? string         -- JUnit unique-id/display-name from first system-out
---@field stdout? string           -- Actual test stdout (from subsequent system-out tags)
---@field stderr? string
---@field stacktrace? string
---@field time? number

---@class test_report.LangAdapter
---@field classname_to_file fun(classname: string, report_dir: string): string|nil
---@field find_test_positions fun(file_path: string): table<string, number>, number|nil
---@field extract_error_line fun(classname: string, stacktrace: string): number|nil
---@field get_test_report_dir fun(): string
```

Result IDs use the format `fully.qualified.ClassName#methodName`. Parameterized test invocations (e.g. `method(int)[1]`, `method(int)[2]`) are merged into a single `ClassName#method` result with multiple invocations. The overall result is "failed" if any invocation failed.

---

## init.lua — Core Module

### State Variables (module-level locals)

| Variable | Type | Purpose |
|---|---|---|
| `process_generation` | `integer` | Monotonically increasing counter; used to cancel stale async processing |
| `last_results` | `{ [classname#method]: TestResult }` | **Accumulated** test results across runs (incremental merge) |
| `last_positions` | `{ [file_path]: { [method_name]: 0-indexed_line } }` | Treesitter-resolved test method positions per file |
| `last_class_files` | `{ [classname]: file_path }` | Resolved source file for each fully-qualified class |
| `last_filetype` | `string\|nil` | Filetype used for adapter resolution |
| `signed_buffers` | `{ [bufnr]: true }` | Tracks which buffers have sign extmarks for cleanup |
| `output_bufnr` | `integer\|nil` | Scratch buffer for test output display |
| `output_method` | `string\|nil` | Method name currently shown in output buffer |
| `ns_diag` | namespace | `"overseer_test_report_diag"` — diagnostics namespace |
| `ns_signs` | namespace | `"overseer_test_report_signs"` — sign extmarks namespace |

### Key Design: Incremental Merge

`process()` does NOT call `M.clear()`. Instead:

1. **Cancels** in-flight processing (`process_generation++`, `spinner.cancel`)
2. **Merges** new XML results into `last_results` (key-by-key)
3. **Determines affected classes** from the current run's results
4. **Builds `by_class`** from `last_results` for affected classes only — this ensures that when a buffer is cleared and re-rendered, ALL accumulated results for that class are restored (not just the current run)
5. **Selectively clears** signs and diagnostics per-buffer (`vim.api.nvim_buf_clear_namespace(bufnr, ns_signs, ...)` + `vim.diagnostic.reset(ns_diag, bufnr)`) — buffers not in the current run keep their existing state
6. **Re-places** signs, virtual text, and diagnostics for affected buffers

This means: rerunning a single test method preserves diagnostics for all other methods in the same class and all other classes.

### Functions

| Function | Description |
|---|---|
| `M.process(report_dir, filetype)` | Main entry: parses XML, merges results, sets signs/diagnostics. Runs in `nio.run()` with spinner. Supports `string\|string[]` for multi-module. |
| `M.load_existing()` | Calls `M.clear()` then `M.process()` — full reset from disk. |
| `M.clear()` | **Full reset**: wipes all state, signs, diagnostics, closes output buffer. Used by explicit user action only. |
| `M.cancel()` | **Soft cancel**: increments generation, cancels spinner, closes Trouble. Does NOT wipe accumulated state. Used by overseer component lifecycle. |
| `M.show_test_output()` | Shows test output for method under cursor in source buffer. |
| `M.hide_test_output()` | Closes the output scratch buffer. |
| `M.show_output_for(method, result)` | Shows output for a specific method+result (used by tree view). |
| `M.get_report_snapshot()` | Returns `{ results, positions, class_files, filetype }` — used by tree view. References live state (not copies). |
| `M.open_tree_view()` | Opens/toggles the tree view via `junit-report-view.toggle()`. |

### Window Management on Test Runs

The flow for clean window layout:
1. **Test starts** → `cancel()` → closes Trouble diagnostics window (only overseer output visible during run)
2. **Test completes** → `process()` → closes overseer output → checks `last_results` for any accumulated failures → reopens Trouble only if failures remain

### Sign/Diagnostic Placement

For each affected class buffer:
- Gutter signs via extmarks (passed `` / failed `` / skipped ``)
- Virtual text with status icon and time after test method names
- Class-level sign at class declaration line (failure if any method failed)
- Diagnostics at error line (extracted from stacktrace) or at method declaration

### Trouble Integration

Uses a custom Trouble source `junit_diagnostics` (not quickfix). The `vim.fn.setqflist()` call is commented out to avoid triggering the `BufRead` autocmd in `config/autocmds.lua:65-74` that auto-opens `Trouble qflist`, which caused a double-window bug.

---

## junit-report-view.lua — Tree View

### Hierarchy

```
JUnit Test Report  ·  42 tests: 40/2/0  ·  12.34s
────────────────────────────────────────────────────

▼ com.example.service        ✘    3.21s
  ▼ UserServiceTest          ✘    2.10s
    ├── testCreate           ✔    0.50s
    ├── testDelete           ✘    1.20s
    └── testUpdate           ✔    0.40s
  ▼ OrderServiceTest         ✔    1.11s
    ├── testPlace            ✔    0.55s
    └── testCancel           ✔    0.56s
```

### Icons

Nerd font icons (cannot be reliably copy-pasted by AI — use Python with explicit codepoints):
- Passed: U+EAB2 (`\xee\xaa\xb2`)
- Failed: U+EAB8 (`\xee\xaa\xb8`)
- Skipped: U+EB32 (`\xee\xac\xb2`)

Icons are placed after the name, between name and time. Same icons are used in `init.lua` `sign_config` for gutter signs.

### State (singleton)

```lua
state = {
    bufnr,          -- scratch buffer number
    winid,          -- window id
    prev_winid,     -- window to return to for navigation/rerun
    tree,           -- PackageNode[] — the rendered hierarchy
    line_map,       -- LineInfo[] — maps each buffer line to its tree node
    snapshot,       -- deep-copied snapshot owned by the tree view
}
```

The tree view **deep-copies** `results` and `class_files` on open so it owns its accumulated state independently from `init.lua`'s live state.

### Tree Node Types

```lua
---@class report_view.PackageNode
---@field name string                    -- e.g. "com.example.service"
---@field status "passed"|"failed"|"skipped"  -- aggregate of all classes
---@field classes report_view.ClassNode[]
---@field expanded boolean

---@class report_view.ClassNode
---@field name string                    -- simple name e.g. "UserServiceTest"
---@field classname string               -- fully qualified
---@field file_path string|nil
---@field status "passed"|"failed"|"skipped"  -- aggregate of all methods
---@field time number
---@field methods report_view.MethodNode[]
---@field expanded boolean

---@class report_view.MethodNode
---@field name string
---@field status "passed"|"failed"|"skipped"
---@field time number|nil
---@field result test_report.TestResult  -- full result for output viewing
---@field id string                      -- "classname#method"
```

### Keymaps (buffer-local)

| Key | Action | Description |
|---|---|---|
| `<CR>` / `gd` | `action_goto` | Navigate to source, centers with `zz` |
| `o` | `action_output` | Show test output (methods only) |
| `r` | `action_rerun(false)` | Rerun test at cursor level |
| `R` | `action_rerun(true)` | Debug test at cursor level |
| `<Tab>` | `action_toggle_fold` | Toggle fold on package/class |
| `g` | `action_full_refresh` | Full refresh (rebuild with sorting) |
| `q` | `M.close()` | Close tree view |

### Rerun Strategy

Rerun navigates to the source file first, positions cursor, then triggers the existing test runner via `overseer-util.run_test()`. This avoids reinventing classpath/JVM-signature resolution:
- **Method**: opens file, positions cursor at method → `task.test_type.CURRENT_TEST`
- **Class**: opens file → `task.test_type.FILE_TESTS`
- **Package**: opens first class in package → `task.test_type.ALL_DIR_TESTS`

### Sorting

`build_tree()` sorts: packages failed-first, classes failed-first within packages, methods failed-first within classes. This only runs on initial open and on `g` (full refresh).

### Incremental vs Full Refresh

| Mode | Trigger | What happens |
|---|---|---|
| **Incremental** | `refresh_if_open(snapshot)` — called from `init.lua` after `process()` | Merges new results into `state.snapshot`, calls `update_tree_in_place()` (preserves sort order), then re-renders |
| **Full** | `g` keymap → `action_full_refresh()` | Replaces snapshot from `init.lua`'s live state, calls `build_tree()` (re-sorts), then re-renders |

`update_tree_in_place()`:
1. Updates existing method nodes with new results
2. Appends new methods to existing classes
3. Appends new classes to existing packages (or creates new packages)
4. Recomputes aggregate statuses for classes and packages
5. Does NOT re-sort — preserves the user's current view order

---

## junit-xml.lua — XML Parser

Parses JUnit XML reports (Maven Surefire format). Key behaviors:
- `parse_report_dir(dir)` → finds `TEST-*.xml` files, parses all, merges into flat results map
- `parse_file(filepath)` → parses single XML file
- Parameterized tests: multiple `<testcase>` with same method but different suffixes are merged into one result with multiple invocations
- `system-out`: first tag is JUnit metadata (unique-id, display-name), rest is actual stdout
- Error line extraction: matches `ClassName.java:123` pattern in stacktraces

---

## lang/java.lua — Java Adapter

| Function | Description |
|---|---|
| `classname_to_file(classname, report_dir)` | Resolves `com.example.MyTest` → source file path. First tries project root (derived from `report_dir` stripping `/target/junit-report`), falls back to CWD-relative via `java-common.java_class_to_proj_path`. Handles inner classes (`$`). |
| `find_test_positions(file_path)` | Uses treesitter to find `@Test`, `@ParameterizedTest`, `@TestFactory`, `@CartesianTest` annotated methods. Returns `{ method_name: 0-indexed_line }` and class declaration line. Loads buffer if not loaded. |
| `extract_error_line(classname, stacktrace)` | Extracts line number from stacktrace for the class. |
| `get_test_report_dir()` | Returns `<project_root>/target/junit-report` path. |

---

## Overseer Component (`junit_report.lua`)

Minimal overseer component that bridges task lifecycle to the test-report module:

| Hook | Calls | Purpose |
|---|---|---|
| `on_complete` | `test_report.process(report_dir, filetype)` | Parse results when test finishes |
| `on_reset` | `test_report.cancel()` | Cancel processing on task restart (preserves state) |
| `on_dispose` | `test_report.cancel()` | Cancel processing on task disposal (preserves state) |

**Critical**: uses `cancel()` not `clear()` — this is what enables incremental diagnostics preservation across partial reruns. The `stop_all_prev_tasks()` in `overseer-task-util.lua` disposes old tasks before starting new ones; if this called `clear()`, accumulated state would be lost.

---

## Call Flow: Test Execution → Results Display

```
User triggers test run (keymap or tree view rerun)
  → overseer-util.run_test(context)
    → overseer-task-util.run_task()
      → stop_all_prev_tasks() → task:dispose() → on_dispose → M.cancel()
      → new task created with junit_report component
      → task starts running
        → [Trouble closed by cancel(), only overseer output visible]
      → task completes
        → on_complete → M.process(report_dir, filetype)
          → nio.run (async with spinner)
            → junit_xml.parse_report_dir(dir) → results
            → merge results into last_results
            → for each affected class:
              → lang adapter: classname_to_file → file_path
              → lang adapter: find_test_positions → method lines
              → clear buffer signs/diagnostics (selective)
              → place signs, virtual text, diagnostics
            → close overseer output
            → if any failures in last_results: open Trouble junit_diagnostics
            → refresh tree view if open (incremental)
```

---

## Namespaces

| Namespace | ID variable | Purpose |
|---|---|---|
| `overseer_test_report_diag` | `ns_diag` | Diagnostics (vim.diagnostic) |
| `overseer_test_report_signs` | `ns_signs` | Sign extmarks in gutter + virtual text |
| `junit_report_view` | `ns` (in junit-report-view.lua) | Highlight extmarks in tree view buffer |
| `test_report_output` | `ns_output` | Highlight extmarks in output scratch buffer |

---

## Logging

All files log to `test-report.log` via `utils.logging-util`. Logger names: `test-report`, `junit-report-view`, `test-report-xml`, `test-report-java`, `test-report-component`. All at DEBUG level.

---

## Key Gotchas for Future Development

1. **Nerd font icons**: AI cannot reliably copy-paste nerd font characters. Use Python with explicit Unicode codepoints: `python3 -c "print('\ueab2')"` (passed=U+EAB2, failed=U+EAB8, skipped=U+EB32).

2. **`cancel()` vs `clear()`**: Component lifecycle hooks MUST use `cancel()`. Only explicit user actions should use `clear()`. This is the foundation of incremental diagnostics preservation.

3. **`by_class` source**: Built from `last_results` (merged accumulator) filtered to `affected_classes` (current run). NOT from `results` (current run only). This ensures sibling methods in the same class retain their diagnostics.

4. **Trouble double-window**: `vim.fn.setqflist()` triggers `BufRead` autocmd in `config/autocmds.lua:65-74` that auto-opens `Trouble qflist`. The `setqflist` call is intentionally commented out. Use only `Trouble junit_diagnostics`.

5. **Tree view snapshot ownership**: `M.open()` deep-copies results and class_files. The tree view manages its own accumulated state via `refresh_if_open()`. `action_full_refresh()` (g key) re-reads from `init.lua`'s live state.

6. **Multi-module support**: `process()` accepts `string|string[]` for `report_dir`. Each directory's XML files are parsed and merged. The Java adapter derives project root from each report_dir.

7. **StyLua**: Format with `~/.local/share/nvim/mason/packages/stylua/stylua`. Config in `nvim/.config/nvim/stylua.toml` (4 spaces, 120 width, double quotes).

8. **`_G.task`**: Global namespace defined in `plugins/overseer/init.lua` with `test_type` and `run_type` enums used for rerun actions.
