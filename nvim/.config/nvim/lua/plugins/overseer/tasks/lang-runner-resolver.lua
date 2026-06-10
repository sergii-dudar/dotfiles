local type_to_resolver = {}
local lang_registry = require("utils.lang.registry")

require("plugins.overseer.tasks.lang.types")
require("plugins.overseer.tasks.lang.simple-runners").register(type_to_resolver)

for _, entry in ipairs(lang_registry.all()) do
    local runner = entry.runner
    if runner then
        local resolver = require(runner.module)
        for _, ft in ipairs(runner.filetypes) do
            type_to_resolver[ft] = resolver
        end
    end
end

local M = {}

-- INFO: in case defined all pairs: [build_debug_cmd, dap_attach_to_remote], [dap_launch, dap_launch_rerun],
--  priority is next: [dap_launch, dap_launch_rerun] (just because native dap `launch`, more reliable and fast then `attach`), [build_debug_cmd, dap_attach_to_remote]

-- dap examples configs - ~/.local/share/nvim/lazy/mason-nvim-dap.nvim/lua/mason-nvim-dap/mappings/configurations.lua

---@param filetype? string
---@return task.lang.Runner|nil
function M.resolve(filetype)
    local ft = filetype or vim.bo.filetype
    local type_resolver = type_to_resolver[ft]
    if type_resolver then
        return type_resolver
    else
        vim.notify(tostring(ft) .. " has no registered resolver.", vim.log.levels.WARN)
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
