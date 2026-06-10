-- Shared annotations for language runner modules.
-- Runtime code is intentionally empty; requiring this file makes the contract
-- available to LuaLS wherever runner metadata is consumed.

---@class task.lang.Context
---@field test_type? task.test_type|integer
---@field is_debug boolean|nil

---@class task.lang.test.TestCmd
---@field cmd string|string[]
---@field report_dir? string|string[]
---@field cwd? string

---@class task.lang.DapOutputAttacher
---@field name string                          Identifier for logging.
---@field match fun(line:string):any|nil       Scan one output line; return a target (port/pid) or nil.
---@field attach fun(target:any)               Attach the debugger using the matched target.

---@class task.lang.Runner
---@field get_envs? fun():table<string, string>
---@field build_run_cmd fun():string[]
---@field build_debug_cmd? fun():string[] - requires dap_attach_to_remote
---@field dap_attach_to_remote? fun()
---@field dap_launch? fun() - requires dap_launch_rerun for rerun support
---@field dap_launch_rerun? fun()
---@field build_compile_cmd? fun()
---@field make_compile? fun()
---@field build_run_test_cmd? fun(context:task.lang.Context):task.lang.test.TestCmd
---@field prepare_test_context? fun(context:task.lang.Context):boolean,string|nil - async-friendly pre-builder hook.
---@field dap_launch_test? fun(context:task.lang.Context) - direct DAP launch for test debug.
---@field get_test_report_dir? fun():string
---@field dap_output_attacher? task.lang.DapOutputAttacher - output-driven DAP attach for the overseer debug-task flow.

-- NOTE: LSP code-action match-names are an LSP concern, not part of the run/test/debug
-- runner contract. See `lang.LspCodeActions` in `utils.lang.lsp-common`.

return {}
