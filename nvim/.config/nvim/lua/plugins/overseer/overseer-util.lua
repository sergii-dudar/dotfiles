local overseer = require("overseer")
local task_list = require("overseer.task_list")
local jdtls_dap_util = require("utils.java.jdtls-config-dap-util")

---@class task.Options
---@field task_name string
---@field is_open_output? boolean
---@field on_finish? function

local M = {}

function M.run_current()
    if vim.bo.filetype == "lua" then
        Snacks.debug.run()
        return
    end
    run_task({ task_name = "RUN_CURRENT", is_open_output = true })
    write_run_info("run")
end

function M.debug_current()
    overseer.close()
    dap_after_session_clear()

    -- 3. Re-launch via Overseer (non-blocking)
    run_task({
        task_name = "DEBUG_CURRENT",
        on_finish = dap_after_session_clear,
    })

    -- 4. After a short delay, re-attach DAP
    -- The delay gives the JVM a moment to start and open the port.
    jdtls_dap_util.attach_to_remote()
    write_run_info("dap")
end

function M.restart_last()
    restart_last_task()
end

local last_run_info = {}
function write_run_info(runtype)
    last_run_info.filetype = vim.bo.filetype
    last_run_info.runtype = runtype
end

---@param opts task.Options
function run_task(opts)
    stop_all_prev_tasks()
    overseer.run_task({ name = opts.task_name }, function(task)
        if task then
            task:start()
            if opts.is_open_output then
                overseer.open({ enter = false })
            end
            if opts.on_finish then
                task:subscribe("on_complete", function(t, status)
                    opts.on_finish()
                end)
                -- task:subscribe("on_exit", function(t, status)
                --     vim.notify("on_exit")
                -- end)
            end
        end
    end)
end

function stop_all_prev_tasks()
    local tasks = overseer.list_tasks({
        status = {
            overseer.STATUS.RUNNING,
            overseer.STATUS.SUCCESS,
            overseer.STATUS.FAILURE,
            overseer.STATUS.CANCELED,
        },
        sort = task_list.sort_finished_recently,
    })
    for _, task in ipairs(tasks) do
        task:dispose(true) -- true = force kill
    end
    -- dd(tasks)
end

function restart_last_task()
    local tasks = overseer.list_tasks({
        status = {
            overseer.STATUS.SUCCESS,
            overseer.STATUS.FAILURE,
            overseer.STATUS.CANCELED,
        },
        sort = task_list.sort_finished_recently,
    })
    if vim.tbl_isempty(tasks) then
        vim.notify("  No tasks found", vim.log.levels.WARN)
    else
        local most_recent = tasks[1]
        overseer.run_action(most_recent, "restart")
        dap_attach_if_specified()
    end
end

function dap_attach_if_specified()
    if last_run_info.runtype == "dap" then
        if last_run_info.filetype == "java" then
            jdtls_dap_util.attach_to_remote()
        end
    end
end

function dap_after_session_clear()
    if last_run_info.runtype == "dap" then
        require("dap").close()
        require("dapui").close({})
    end
end

--[[ 
function run_java_code_debug()
    if vim.bo.filetype == "java" then
        require("utils.java.jdtls-config-dap-util").run_current_main_class()
    end
    write_run_info("dap")
end
function rerun_java_last()
    if last_run_info.filetype == "java" and last_run_info.runtype == "dap" then
        require("utils.java.jdtls-config-dap-util").rerun_last()
    else
        restart_last()
    end
end
]]

return M