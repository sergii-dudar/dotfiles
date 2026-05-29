-- Execution engine for Java refactoring operations.
-- Handles terminal-based execution (with UI), test-mode direct execution,
-- and composite callback orchestration.

local M = {}

local util = require("utils.common-util")
local spinner = require("utils.ui.spinner")
local consts = require("modules.java.refactor.constants")

local log = consts.log

local current_term_win = nil

--- Execute a shell command string in a terminal split with progress UI.
--- On success, closes the terminal and calls the callback.
---@param cmd_args string The shell command to run
---@param on_success_callback? function Called after successful completion
function M.run_cmd(cmd_args, on_success_callback)
    log.info("Starting Java refactoring command execution")
    log.debug("Command:", cmd_args)
    spinner.start("🚀 " .. "Java Refactoring...")

    vim.cmd("botright split")

    util.close_window_if_exists(current_term_win)
    current_term_win = vim.api.nvim_get_current_win()
    local term_buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_win_set_buf(current_term_win, term_buf)

    local job_id = vim.fn.jobstart(cmd_args, {
        term = true,
        stdout_buffered = false,
        stderr_buffered = false,
        on_exit = function(_, code)
            if code == 0 then
                log.info("Java refactoring completed successfully")
            else
                log.error("Java refactoring failed with exit code:", code)
            end
            spinner.stop(code == 0, code == 0 and "Java refactoring finished" or "Java refactoring failed")
            if code == 0 then
                util.close_window_if_exists(current_term_win)
                if on_success_callback then
                    vim.schedule(function()
                        on_success_callback()
                    end)
                end
            end
        end,
    })

    if job_id <= 0 then
        log.error("Failed to start command via jobstart()")
        vim.notify("Failed to start cmd via jobstart()", vim.log.levels.ERROR)
    else
        log.debug("Command started with job_id:", job_id)
    end
end

--- Execute operations in test mode (directly, without UI).
--- Runs shell commands individually for better error reporting.
---@param shell_cmds string[]
---@param lua_operations table[]
---@return boolean success
function M.execute_test_mode(shell_cmds, lua_operations)
    log.info("Test mode: executing operations directly")

    -- Execute shell commands individually
    if #shell_cmds > 0 then
        local failed_cmds = 0
        for i, cmd in ipairs(shell_cmds) do
            log.debug("Executing command", i, "of", #shell_cmds, ":", cmd)
            local exit_code = os.execute(cmd)
            if not (exit_code == 0 or exit_code == true) then
                log.error("Shell command failed (continuing):", cmd)
                failed_cmds = failed_cmds + 1
            end
        end
        if failed_cmds > 0 then
            log.warn(failed_cmds, "of", #shell_cmds, "shell commands had non-zero exit")
        end
        log.info("Shell commands completed:", #shell_cmds - failed_cmds, "succeeded,", failed_cmds, "failed")
    end

    -- Execute Lua operations
    if #lua_operations > 0 then
        log.info("Executing", #lua_operations, "Lua operations")
        for _, op in ipairs(lua_operations) do
            log.info("Executing:", op.description)
            local success = op.fn()
            if not success then
                log.error("Lua operation failed:", op.description)
                return false
            end
        end
        log.info("All Lua operations completed successfully")
    end

    return true
end

--- Separate operations into shell commands and Lua operations.
---@param global_operations RefactorOperation[]
---@return string[] shell_cmds
---@return table[] lua_operations
function M.separate_operations(global_operations)
    local shell_cmds = {}
    local lua_operations = {}
    for _, op in ipairs(global_operations) do
        if op.type == "shell" then
            table.insert(shell_cmds, op.command)
        elseif op.type == "lua" then
            table.insert(lua_operations, op)
        end
    end
    log.info("Shell commands:", #shell_cmds, "| Lua operations:", #lua_operations)
    return shell_cmds, lua_operations
end

return M
