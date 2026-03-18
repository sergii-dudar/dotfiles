local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")

local M = {}

local resolve_report_dir = function()
    local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
    if type_resolver and type_resolver.get_test_report_dir then
        return type_resolver.get_test_report_dir()
    end
    return nil
end

---@param params { context: task.lang.Context }
---@return task.lang.test.TestCmd
local resolve_type_test_cmd = function(params)
    local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
    if type_resolver then
        return type_resolver.build_run_test_cmd(params.context)
    end

    local file = vim.fn.expand("%:p")
    vim.notify(file .. " is not supported.")
    return { cmd = { "echo", file .. " is not supported tests." } }
end

---@return table
function M.build_taks()
    return {
        name = "RUN_TESTS",
        builder = function(params)
            local test_cmd = resolve_type_test_cmd(params)
            local result_cmd = test_cmd.cmd
            local report_dir = test_cmd.report_dir or resolve_report_dir()
            local components = { "on_exit_set_status" }
            if report_dir then
                table.insert(components, 1, {
                    "test_report.junit_report",
                    report_dir = report_dir,
                    filetype = vim.bo.filetype,
                })
            else
                table.insert(components, 1, { "on_output_quickfix", set_diagnostics = true })
                table.insert(components, 2, "on_result_diagnostics")
            end
            -- dd(result_cmd)
            return {
                cmd = result_cmd,
                components = components,
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
            local test_cmd = resolve_type_test_cmd(params)
            local result_cmd = test_cmd.cmd
            local report_dir = test_cmd.report_dir or resolve_report_dir()
            local components = { "on_exit_set_status", "debug.dap_ctrl_component" }
            if report_dir then
                table.insert(components, 1, {
                    "test_report.junit_report",
                    report_dir = report_dir,
                    filetype = vim.bo.filetype,
                })
            else
                table.insert(components, 1, { "on_output_quickfix", set_diagnostics = true })
                table.insert(components, 2, "on_result_diagnostics")
            end
            return {
                cmd = result_cmd,
                components = components,
            }
        end,
        condition = {
            filetype = lang_runner_resolver.types_supported_test_cmd,
        },
    }
end

return M
