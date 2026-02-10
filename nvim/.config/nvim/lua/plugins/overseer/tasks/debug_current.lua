local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")

local M = {}

---@return table
function M.build_taks()
    return {
        name = "DEBUG_CURRENT",
        builder = function()
            local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
            local result_cmd
            if type_resolver then
                result_cmd = type_resolver.build_debug_cmd()
            else
                local file = vim.fn.expand("%:p")
                vim.notify(file .. " is not supported.")
                result_cmd = { "echo", file .. " is not supported." }
            end
            return {
                cmd = result_cmd,
                -- add some components that will pipe the output to quickfix,
                -- parse it using errorformat, and display any matching lines as diagnostics.
                components = {
                    { "on_output_quickfix", set_diagnostics = true },
                    "on_result_diagnostics",
                    "on_exit_set_status",
                   -- "on_complete_dispose",
                    -- "on_complete_notify",
                    "debug.dap_ui_output", -- mirror output to DAP REPL/console
                },
            }
        end,
        condition = {
            filetype = lang_runner_resolver.types_supported_debug_cmd,
        },
    }
end

return M
