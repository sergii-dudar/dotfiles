local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")
local overseer = require("overseer")
local task_list = require("overseer.task_list")

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
    local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
    if not type_resolver or (not type_resolver.build_debug_cmd and not type_resolver.dap_launch) then
        vim.notify("Debug is not configured for: " .. vim.bo.filetype, vim.log.levels.WARN)
        return
    end

    overseer.close()

    if type_resolver.build_debug_cmd then
        dap_after_session_clear()

        -- 3. Re-launch via Overseer (non-blocking)
        run_task({
            task_name = "DEBUG_CURRENT",
            on_finish = dap_after_session_clear,
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
    restart_last_task()
end

local last_run_info = {}
function write_run_info(runtype)
    last_run_info.filetype = vim.bo.filetype
    last_run_info.runtype = runtype
end

---@param opts task.Options
function run_task(opts)
    local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
    if not type_resolver then
        vim.notify("Runner is not configured for: " .. vim.bo.filetype, vim.log.levels.WARN)
        return
    end

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
end

function run_last_task()
    local tasks = overseer.list_tasks({
        status = {
            overseer.STATUS.SUCCESS,
            overseer.STATUS.FAILURE,
            overseer.STATUS.CANCELED,
        },
        sort = task_list.sort_finished_recently,
    })
    if vim.tbl_isempty(tasks) then
        vim.notify("  No last tasks found", vim.log.levels.WARN)
        return
    end
    local last_task = tasks[1]
    overseer.run_action(last_task, "restart")
end

function restart_last_task()
    if last_run_info.runtype == "dap" then
        local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
        if
            not type_resolver
            or (
                not type_resolver.build_debug_cmd
                and not type_resolver.dap_launch
                and not type_resolver.dap_launch_rerun
            )
        then
            vim.notify("Debug is not configured for: " .. vim.bo.filetype, vim.log.levels.WARN)
            return
        end

        if type_resolver.build_debug_cmd then
            require("dap").terminate()
            vim.defer_fn(function()
                run_last_task()
                type_resolver.dap_attach_to_remote()
                -- vim.notify("build_debug_cmd " .. vim.bo.filetype, vim.log.levels.WARN)
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
        run_last_task()
    end
end

function dap_after_session_clear()
    if last_run_info.runtype == "dap" then
        require("dap").close()
        require("dapui").close({})
    end
end

return M