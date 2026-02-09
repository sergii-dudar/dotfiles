local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")
local overseer_task_util = require("plugins.overseer.overseer-task-util")
local overseer = require("overseer")
-- local task_list = require("overseer.task_list")

local M = {}

function M.run_current()
    if vim.bo.filetype == "lua" then
        Snacks.debug.run()
        return
    end
    local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
    if not type_resolver then
        vim.notify("Runner is not configured for: " .. vim.bo.filetype, vim.log.levels.WARN)
        return
    end
    overseer_task_util.run_task({ task_name = "RUN_CURRENT", is_open_output = true })
    write_run_info("run")
end

function M.debug_current()
    local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
    if not type_resolver or (not type_resolver.build_debug_cmd and not type_resolver.dap_launch) then
        vim.notify("Debug is not configured for: " .. vim.bo.filetype, vim.log.levels.WARN)
        return
    end
    debug_current_internal(type_resolver)
end

---@param type_resolver task.lang.Runner
function debug_current_internal(type_resolver)
    overseer.close()

    if type_resolver.build_debug_cmd then
        dap_after_session_clear()

        -- 3. Re-launch via Overseer (non-blocking)
        overseer_task_util.run_task({
            task_name = "DEBUG_CURRENT",
            on_complete = dap_after_session_clear,
        })

        -- 4. After a short delay, re-attach DAP
        -- The delay gives the JVM a moment to start and open the port.
        -- jdtls_dap_util.attach_to_remote()
        type_resolver.dap_attach_to_remote()
    else
        type_resolver.dap_launch()
    end

    write_run_info("dap")
end

function M.restart_last()
    local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)

    if not type_resolver then
        vim.notify("Is not configured for: " .. vim.bo.filetype, vim.log.levels.WARN)
        return
    end

    restart_last_task(type_resolver)
end

local last_run_info = {}
function write_run_info(runtype)
    last_run_info.filetype = vim.bo.filetype
    last_run_info.runtype = runtype
end

---@param type_resolver task.lang.Runner
function restart_last_task(type_resolver)
    if last_run_info.runtype == "dap" then
        if
            not type_resolver.build_debug_cmd
            and not type_resolver.dap_launch
            and not type_resolver.dap_launch_rerun
        then
            vim.notify("Debug is not configured for: " .. vim.bo.filetype, vim.log.levels.WARN)
            return
        end

        if type_resolver.build_debug_cmd then
            require("dap").terminate()
            vim.defer_fn(function()
                overseer_task_util.run_last_task()
                type_resolver.dap_attach_to_remote()
            end, 400)
            return
        end

        if type_resolver.dap_launch_rerun then
            type_resolver.dap_launch_rerun()
            -- vim.notify("dap_launch_rerun" .. vim.bo.filetype, vim.log.levels.WARN)
            return
        end

        if type_resolver.dap_launch then
            type_resolver.dap_launch()
            -- vim.notify("dap_launch" .. vim.bo.filetype, vim.log.levels.WARN)
            return
        end
    else
        overseer_task_util.run_last_task()
    end
end

function dap_after_session_clear()
    if last_run_info.runtype == "dap" then
        require("dap").close()
        require("dapui").close({})
    end
end

return M
