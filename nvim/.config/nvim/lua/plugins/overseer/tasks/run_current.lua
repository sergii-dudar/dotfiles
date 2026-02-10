local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")

local M = {}

---@return table
function M.build_taks()
    return {
        name = "RUN_CURRENT",
        builder = function()
            local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
            local result_cmd
            if type_resolver then
                result_cmd = type_resolver.build_run_cmd()
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
                    -- { "on_complete_dispose", require_view = { "SUCCESS", "FAILURE" } },
                    -- "on_result_diagnostics_trouble",
                    -- "open_output",
                    -- "on_output_notify",
                    --"default",
                },
            }
        end,
        condition = {
            filetype = lang_runner_resolver.types_supported,
        },
    }
end

return M
