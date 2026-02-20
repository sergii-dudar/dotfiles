local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")

local M = {}

local resolve_type_cmd = function(params)
    local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
    local result_cmd
    if type_resolver then
        if params.test_type == task.test_type.CURRENT_TEST then
            result_cmd = type_resolver.build_run_test_cmd(params.is_test_debug)
        elseif params.test_type == task.test_type.FILE_TESTS then
            result_cmd = type_resolver.build_run_file_tests_cmd(params.is_test_debug)
        elseif params.test_type == task.test_type.ALL_TESTS then
            result_cmd = type_resolver.build_run_all_tests_cmd(params.is_test_debug)
        elseif params.test_type == task.test_type.CURRENT_PARAMETRIZED_NUM_TEST then
            result_cmd = type_resolver.build_run_parametrized_num_test_cmd(params.is_test_debug)
        else
            local file = vim.fn.expand("%:p")
            vim.notify(file .. " test_type " .. params.test_type .. " is not supported.")
            result_cmd = { "echo", file .. " test_type " .. params.test_type .. " is not supported tests." }
        end
    else
        local file = vim.fn.expand("%:p")
        vim.notify(file .. " is not supported.")
        result_cmd = { "echo", file .. " is not supported tests." }
    end
    return result_cmd
end

---@return table
function M.build_taks()
    return {
        name = "RUN_TESTS",
        builder = function(params)
            local result_cmd = resolve_type_cmd(params)
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
            filetype = lang_runner_resolver.types_supported_test_cmd,
        },
    }
end

---@return table
function M.build_debug_taks()
    return {
        name = "DEBUG_TESTS",
        builder = function(params)
            local result_cmd = resolve_type_cmd(params)
            return {
                cmd = result_cmd,
                -- add some components that will pipe the output to quickfix,
                -- parse it using errorformat, and display any matching lines as diagnostics.
                components = {
                    { "on_output_quickfix", set_diagnostics = true },
                    "on_result_diagnostics",
                    "on_exit_set_status",
                    "debug.dap_ctrl_component",
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
            filetype = lang_runner_resolver.types_supported_test_cmd,
        },
    }
end

return M
