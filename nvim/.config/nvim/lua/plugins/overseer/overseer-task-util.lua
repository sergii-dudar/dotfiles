local overseer = require("overseer")
local task_list = require("overseer.task_list")

---@class task.Options
---@field task_name string
---@field is_open_output? boolean
---@field on_complete? function

local M = {}

---@param on_success function
function M.run_compile(on_success)
    vim.notify("running compile")
    M.run_task({
        task_name = "COMPILE_CURRENT",
        on_complete = function(task, status)
            vim.notify("finished." .. status, vim.log.levels.INFO)
            if status == overseer.STATUS.SUCCESS then
                vim.notify("Compilation successful.")
                on_success()
            else
                vim.notify("Compilation failed.", vim.log.levels.INFO)
                vim.cmd("Trouble diagnostics open")
                -- vim.cmd("copen") -- Open quickfix to show errors
            end
        end,
    })
end

---@param opts task.Options
function M.run_task(opts)
    M.stop_all_prev_tasks()
    overseer.run_task({ name = opts.task_name }, function(task)
        if task then
            task:start()
            if opts.is_open_output then
                overseer.open({ enter = false })
            end
            if opts.on_complete then
                task:subscribe("on_complete", function(t, status)
                    opts.on_complete(t, status)
                end)
                -- task:subscribe("on_exit", function(t, status)
                --     vim.notify("on_exit")
                -- end)
            end
        end
    end)
end

function M.stop_all_prev_tasks()
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

function M.run_last_task()
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

return M
