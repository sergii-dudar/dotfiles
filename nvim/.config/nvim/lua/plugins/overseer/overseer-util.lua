local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")
local overseer_task_util = require("plugins.overseer.overseer-task-util")
local overseer = require("overseer")
local nio_util = require("utils.nio-util")
-- local task_list = require("overseer.task_list")

local M = {}

-- Forward declarations so helpers are local (not implicit globals) and
-- can still reference each other in any order.
local debug_current_internal
local restart_last_task
local write_run_info
local dap_after_session_clear

local last_run_info = {}

---@param context task.lang.Context
function M.run_test(context)
    local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)
    if not type_resolver then
        vim.notify("Test runner is not configured for: " .. vim.bo.filetype, vim.log.levels.WARN)
        return
    end
    -- Opt-in: language runners can pre-populate context fields that require
    -- async prompts (e.g. multi-select). We're still in the caller's nio.run
    -- context here. By the time overseer's builder runs, the nio context may
    -- be gone (overseer's built-in templates can async-break it).
    if type_resolver.prepare_test_context then
        local ok, err = type_resolver.prepare_test_context(context)
        if not ok then
            if err then
                vim.notify(err, vim.log.levels.WARN)
            end
            return
        end
    end
    if context.is_debug then
        overseer.close()
        require("utils.dap-util").reset()
        dap_after_session_clear()
        -- Opt-in: language runners can provide a direct DAP launch for tests,
        -- bypassing overseer task lifecycle. Useful when there is no equivalent
        -- of JVM's JDWP "wait for debugger" mode (e.g. Rust test binaries).
        if type_resolver.dap_launch_test then
            type_resolver.dap_launch_test(context)
            write_run_info(task.run_type.TEST, true)
            return
        end
        overseer_task_util.run_task({
            task_name = "DEBUG_TESTS",
            is_open_output = false,
            context = context,
            env = type_resolver.get_envs and type_resolver.get_envs(),
        })
        write_run_info(task.run_type.TEST, true)
    else
        -- A non-java debug-test session is launched directly via dap.run (no
        -- overseer task), so stop_all_prev_tasks won't clean it up when we switch
        -- back to a regular run. Terminate any live session here so a
        -- debug -> regular toggle/rerun doesn't leave it dangling.
        local ok_dap, dap = pcall(require, "dap")
        if ok_dap and dap.session() then
            pcall(dap.terminate)
            pcall(function()
                require("dapui").close({})
            end)
        end
        require("utils.dap-util").reset()
        overseer_task_util.run_task({
            task_name = "RUN_TESTS",
            is_open_output = true,
            context = context,
            env = type_resolver.get_envs and type_resolver.get_envs(),
        })
        write_run_info(task.run_type.TEST, false)
    end
end

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
    require("utils.dap-util").reset()
    overseer_task_util.run_task({
        task_name = "RUN_CURRENT",
        is_open_output = true,
        env = type_resolver.get_envs and type_resolver.get_envs(),
    })
    write_run_info(task.run_type.RUN, false)
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

    if type_resolver.dap_launch then
        type_resolver.dap_launch()
    elseif type_resolver.build_debug_cmd then
        dap_after_session_clear()

        -- 3. Re-launch via Overseer (non-blocking)
        overseer_task_util.run_task({
            task_name = "DEBUG_CURRENT",
            on_complete = dap_after_session_clear,
            env = type_resolver.get_envs and type_resolver.get_envs(),
        })

        -- 4. After a short delay, re-attach DAP
        -- The delay gives the JVM a moment to start and open the port.
        -- jdtls_dap_util.attach_to_remote()
        -- type_resolver.dap_attach_to_remote() -- moved to dap_ctrl_component
    end

    write_run_info(task.run_type.RUN, true)
end

function M.restart_last()
    local type_resolver = lang_runner_resolver.resolve(vim.bo.filetype)

    if not type_resolver then
        vim.notify("Is not configured for: " .. vim.bo.filetype, vim.log.levels.WARN)
        return
    end

    restart_last_task(type_resolver)
end

---@param runtype task.run_type|integer
---@param is_debug boolean
function write_run_info(runtype, is_debug)
    last_run_info.filetype = vim.bo.filetype
    last_run_info.runtype = runtype
    last_run_info.is_debug = is_debug
end

---@param type_resolver task.lang.Runner
function restart_last_task(type_resolver)
    if last_run_info.runtype == task.run_type.RUN then
        if last_run_info.is_debug then
            if
                not type_resolver.build_debug_cmd
                and not type_resolver.dap_launch
                and not type_resolver.dap_launch_rerun
            then
                vim.notify("Debug is not configured for: " .. vim.bo.filetype, vim.log.levels.WARN)
                return
            end

            if type_resolver.dap_launch_rerun then
                type_resolver.dap_launch_rerun()
                return
            end

            if type_resolver.dap_launch then
                type_resolver.dap_launch()
                return
            end

            if type_resolver.build_debug_cmd then
                require("dap").terminate()
                overseer_task_util.run_last_task()
                return
            end
        else
            overseer_task_util.run_last_task({ is_open_output = true })
        end
    elseif last_run_info.runtype == task.run_type.TEST then
        if last_run_info.is_debug then
            if type_resolver.dap_launch_test then
                -- Non-java debug launches dap directly (no overseer task to
                -- restart). Re-dispatch through run_test, reusing the language's
                -- stored last selection via TOGGLE_LAST_DEBUG.
                nio_util.run(function()
                    M.run_test({ test_type = task.test_type.TOGGLE_LAST_DEBUG, is_debug = true })
                end)
            else
                -- Java: debug runs as an overseer DEBUG_TESTS task; restart it
                -- (handles every test type and re-attaches via dap_ctrl_component).
                require("dap").terminate()
                overseer_task_util.run_last_task()
            end
        else
            overseer_task_util.run_last_task({ is_open_output = true })
        end
    end
end

--- Toggle the last test run between regular and debug mode, reusing the
--- language's stored last selection. Centralised here (not in the keymap) so the
--- toggle direction is driven by the single source of truth (last_run_info),
--- which is maintained for every language — including the non-java direct-dap
--- path that does not touch the java-only `task.last_test` global.
function M.toggle_last_test_debug()
    if last_run_info.runtype ~= task.run_type.TEST then
        vim.notify("No previous test run to toggle debug for", vim.log.levels.WARN)
        return
    end
    local target = not last_run_info.is_debug
    -- Java's junit TOGGLE_LAST_DEBUG branch reads this global to decide whether to
    -- attach the JDWP agent; harmless no-op for other languages (they reuse their
    -- own state.last). Keep it in sync with the central last_run_info.
    task.last_test.is_debug = target
    nio_util.run(function()
        M.run_test({ test_type = task.test_type.TOGGLE_LAST_DEBUG, is_debug = target })
    end)
end

function dap_after_session_clear()
    if last_run_info.is_debug then
        pcall(function()
            require("dap").close()
            require("dapui").close({})
        end)
    end
end

function M.stop_all()
    -- Stop running overseer tasks
    local tasks = overseer.list_tasks({ status = { overseer.STATUS.RUNNING } })
    if not vim.tbl_isempty(tasks) then
        for _, t in ipairs(tasks) do
            t:stop()
        end
        return
    end

    -- Terminate DAP session if active
    local dap = require("dap")
    if dap.session() then
        dap.terminate()
        return
    end

    vim.notify("  No running tasks or active dap session found", vim.log.levels.WARN)
end

return M
