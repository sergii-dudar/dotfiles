local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")

local M = {}

---@return table
function M.build_taks()
    return {
        name = "COMPILE_CURRENT",
        builder = function()
            local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
            local result_cmd
            if type_resolver then
                result_cmd = type_resolver.build_compile_cmd()
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
                    "on_exit_set_status",
                   -- "on_complete_dispose",
                    { "on_result_diagnostics_trouble", close = true },
                    -- "on_result_diagnostics",
                    -- { "on_output_quickfix", close = true, open = true },
                    -- "default",
                },
            }
        end,
        condition = {
            filetype = lang_runner_resolver.types_supported_compile_cmd,
        },
    }
end

return M
