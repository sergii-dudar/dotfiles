-- Shared type annotations for the generic test-report module.
-- This file is documentation-only; no runtime code is exposed.

---@class test_report.Invocation
---@field name string
---@field status "passed"|"failed"|"skipped"
---@field metadata? string
---@field stdout? string
---@field stderr? string
---@field stacktrace? string
---@field time? number

---@class test_report.TestResult
---@field status "passed"|"failed"|"skipped"
---@field errors? { message: string, line: number|nil }[]
---@field time? number
---@field invocations test_report.Invocation[]

---@class test_report.FindOpts
---@field silent? boolean If true (default), load buffer without firing autocmds (fast, no LSP/highlight).

---@class test_report.IdDisplay
---@field container string  Display name of the container (Java class / Rust module).
---@field member string     Display name of the test (method/function).
---@field group? string     Group path above the container (Java package / Rust crate::module path). Nil → default group.

---@class test_report.LangAdapter
---@field parse_results       fun(dirs: string[]): table<string, test_report.TestResult>
---@field id_to_file          fun(container_id: string, report_dir: string): string|nil
---@field find_test_positions fun(file: string, opts?: test_report.FindOpts): table<string, number>, number|nil
---@field extract_error_line  fun(container_id: string, stacktrace: string): number|nil
---@field get_test_report_dir fun(): string|string[]
---@field id_to_display       fun(id: string): test_report.IdDisplay
---@field group_separator     string   Separator for group hierarchy ("." for java, "::" for rust).
---@field diagnostic_source   string   Value placed in vim.diagnostic entries `source` field.
---@field trouble_source?     string   Optional Trouble source key (e.g. "junit_diagnostics").
---@field clear_cache?        fun()

---@class test_report.Snapshot
---@field results table<string, test_report.TestResult>
---@field positions table<string, table<string, number>>
---@field container_files table<string, string>
---@field filetype string|nil

---@class test_report.Config
---@field load_buffers boolean Load every test container buffer with full autocmds.
---@field load_only_buffers_with_error boolean Load only buffers containing failed tests with full autocmds.

return {}
