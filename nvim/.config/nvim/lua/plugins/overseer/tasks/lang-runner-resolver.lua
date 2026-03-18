local type_to_resolver = {}

require("plugins.overseer.tasks.lang.simple-runners").register(type_to_resolver)
type_to_resolver["java"] = require("plugins.overseer.tasks.lang.java-runner")
type_to_resolver["python"] = require("plugins.overseer.tasks.lang.python-runner")
type_to_resolver["go"] = require("plugins.overseer.tasks.lang.go-runner")
type_to_resolver["javascript"] = require("plugins.overseer.tasks.lang.js-runner")
type_to_resolver["sh"] = require("plugins.overseer.tasks.lang.sh-runner")
-- type_to_resolver["cs"] = require("plugins.overseer.tasks.lang.cs-runner")
type_to_resolver["c"] = require("plugins.overseer.tasks.lang.clang-runner")
type_to_resolver["cpp"] = require("plugins.overseer.tasks.lang.cpp-runner")
type_to_resolver["cpp"] = require("plugins.overseer.tasks.lang.cpp-runner")
type_to_resolver["rust"] = require("plugins.overseer.tasks.lang.rust-runner")

local M = {}

---@class task.lang.Context
---@field test_type? task.test_type|integer
---@field is_debug boolean|nil

---@class task.lang.test.TestCmd
---@field cmd string|string[]
---@field report_dir? string|string[]

---@class task.lang.Runner
---@field get_envs? fun():table<string, string>
---@field build_run_cmd fun():string[]
---@field build_debug_cmd? fun():string[] - [build_debug_cmd] requiring to have defined [dap_attach_to_remote]
---@field dap_attach_to_remote? fun()
---@field dap_launch? fun() - [dap_launch] requiring to have defined [dap_launch_rerun]
---@field dap_launch_rerun? fun()
---@field build_compile_cmd? fun()
---@field make_compile? fun()
---@field build_run_test_cmd?  fun(context:task.lang.Context):task.lang.test.TestCmd
---@field get_test_report_dir? fun():string

-- INFO: in case defined all pairs: [build_debug_cmd, dap_attach_to_remote], [dap_launch, dap_launch_rerun],
--  priority is next: [dap_launch, dap_launch_rerun] (just because native dap `launch`, more reliable and fast then `attach`), [build_debug_cmd, dap_attach_to_remote]

-- dap examples configs - ~/.local/share/nvim/lazy/mason-nvim-dap.nvim/lua/mason-nvim-dap/mappings/configurations.lua

---@return task.lang.Runner|nil
function M.resolve(filetype)
    local type_resolver = type_to_resolver[filetype]
    if type_resolver then
        return type_resolver
    else
        vim.notify(filetype .. " have no any registered resolvers.", vim.log.levels.WARN)
        return nil
    end
end

M.types_supported = {} -- all basic support it's shell cmd to run by overseer
M.types_supported_debug_cmd = {}
M.types_supported_compile_cmd = {}
M.types_supported_test_cmd = {}

for key, resolver in pairs(type_to_resolver) do
    table.insert(M.types_supported, key)
    if resolver.build_debug_cmd and resolver.dap_attach_to_remote then
        table.insert(M.types_supported_debug_cmd, key)
    end
    if resolver.build_compile_cmd then
        table.insert(M.types_supported_compile_cmd, key)
    end
    if resolver.build_run_test_cmd then
        table.insert(M.types_supported_test_cmd, key)
    end
end

--[[ print(vim.tbl_contains(M.types_supported, "jaav"))
print(vim.tbl_contains(M.types_supported, "java"))
print(vim.tbl_contains(M.types_supported_debug_cmd_flag, "jaav"))
print(vim.tbl_contains(M.types_supported_debug_cmd_flag, "java"))
print(M.types_supported_debug_cmd)
print(M.types_supported_dap_launch) ]]

return M
