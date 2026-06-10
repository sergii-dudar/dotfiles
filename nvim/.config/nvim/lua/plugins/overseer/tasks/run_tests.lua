local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")
local lang_registry = require("utils.lang.registry")

local M = {}

---@param filetype string|nil
---@return string|nil
local function report_component_for(filetype)
    local report = lang_registry.report_for_filetype(filetype)
    return report and report.component or nil
end

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
            local report_component = report_component_for(vim.bo.filetype)
            if report_dir and report_component then
                table.insert(components, 1, {
                    report_component,
                    report_dir = report_dir,
                    filetype = vim.bo.filetype,
                })
            else
                table.insert(components, 1, { "on_output_quickfix", set_diagnostics = true })
                table.insert(components, 2, "on_result_diagnostics")
            end
            local task = {
                cmd = result_cmd,
                components = components,
            }
            if test_cmd.cwd then
                task.cwd = test_cmd.cwd
            end
            return task
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
            local components = {
                "on_exit_set_status",
                { "debug.dap_ctrl_component", filetype = vim.bo.filetype },
            }
            local report_component = report_component_for(vim.bo.filetype)
            if report_dir and report_component then
                table.insert(components, 1, {
                    report_component,
                    report_dir = report_dir,
                    filetype = vim.bo.filetype,
                })
            else
                table.insert(components, 1, { "on_output_quickfix", set_diagnostics = true })
                table.insert(components, 2, "on_result_diagnostics")
            end
            local task = {
                cmd = result_cmd,
                components = components,
            }
            if test_cmd.cwd then
                task.cwd = test_cmd.cwd
            end
            return task
        end,
        condition = {
            filetype = lang_runner_resolver.types_supported_test_cmd,
        },
    }
end

return M
