local type_to_resolver = {}

require("plugins.overseer.tasks.lang.simple-runners").register(type_to_resolver)
type_to_resolver["java"] = require("plugins.overseer.tasks.lang.java-runner")
type_to_resolver["python"] = require("plugins.overseer.tasks.lang.python-runner")
type_to_resolver["go"] = require("plugins.overseer.tasks.lang.go-runner")
type_to_resolver["javascript"] = require("plugins.overseer.tasks.lang.js-runner")
type_to_resolver["sh"] = require("plugins.overseer.tasks.lang.sh-runner")

local M = {}

---@class task.lang.Runner
---@field build_cmd function
---@field build_debug_cmd function|nil - [build_debug_cmd] requiring to have defined [dap_attach_to_remote]
---@field dap_attach_to_remote function|nil
---@field dap_launch function|nil - [dap_launch] requiring to have defined [dap_launch_rerun]
---@field dap_launch_rerun function|nil

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

for key, resolver in pairs(type_to_resolver) do
    table.insert(M.types_supported, key)
    if resolver.build_debug_cmd and resolver.dap_attach_to_remote then
        table.insert(M.types_supported_debug_cmd, key)
    end
end

--[[ print(vim.tbl_contains(M.types_supported, "jaav"))
print(vim.tbl_contains(M.types_supported, "java"))
print(vim.tbl_contains(M.types_supported_debug_cmd_flag, "jaav"))
print(vim.tbl_contains(M.types_supported_debug_cmd_flag, "java"))
print(M.types_supported_debug_cmd)
print(M.types_supported_dap_launch) ]]

return M
