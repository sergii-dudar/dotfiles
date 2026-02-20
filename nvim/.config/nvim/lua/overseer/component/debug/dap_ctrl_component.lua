function write_to_dapui_console(lines)
    local ok_dap, dap = pcall(require, "dap")
    local ok_dapui, dapui = pcall(require, "dapui")
    if ok_dap and ok_dapui and dap.session() then
        -- Get the console buffer from dapui
        local console_buf = dapui.elements.console.buffer()

        if console_buf and vim.api.nvim_buf_is_valid(console_buf) then
            -- Temporarily make buffer modifiable
            local modifiable = vim.bo[console_buf].modifiable
            vim.bo[console_buf].modifiable = true

            -- Get current line count
            local line_count = vim.api.nvim_buf_line_count(console_buf)

            -- Append lines to the console buffer
            vim.api.nvim_buf_set_lines(console_buf, line_count, line_count, false, lines)

            -- Restore modifiable setting
            vim.bo[console_buf].modifiable = modifiable
        end
    end
end

function clean_dapui_console()
    local ok_dap, dap = pcall(require, "dap")
    local ok_dapui, dapui = pcall(require, "dapui")

    if ok_dap and ok_dapui then
        local console_buf = dapui.elements.console.buffer()
        if console_buf and vim.api.nvim_buf_is_valid(console_buf) then
            local modifiable = vim.bo[console_buf].modifiable
            vim.bo[console_buf].modifiable = true
            -- Clear all lines
            vim.api.nvim_buf_set_lines(console_buf, 0, -1, false, {})
            vim.bo[console_buf].modifiable = modifiable
        end
    end
end

function write_to_dapui_repl(lines)
    local ok, dap = pcall(require, "dap")
    if ok and dap.session() then
        for _, line in ipairs(lines) do
            dap.repl.append(line)
        end
    end
end

---@type overseer.ComponentFileDefinition
return {
    desc = "Pipe task output to DAP UI console",
    constructor = function(params)
        local first_line_checked = false

        return {
            on_start = function(self, task)
                -- Clear console buffer when task starts
                vim.schedule(clean_dapui_console)
                first_line_checked = false
            end,
            on_output_lines = function(self, task, lines)
                -- Check only the first line for debug port pattern
                if not first_line_checked and lines and #lines > 0 then
                    first_line_checked = true
                    local first_line = lines[1]
                    -- Match pattern: "Listening for transport dt_socket at address: <port>"
                    local port = first_line:match("Listening for transport dt_socket at address: (%d+)")
                    -- TODO: It's java specific, make language-agnostic by moving to overseer.tasks.lang.[lang]-runner.lua
                    if port then
                        vim.notify("Connecting to java dap port: " .. port)
                        require("utils.java.jdtls-config-dap-util").attach_to_remote(port)
                    end
                end

                vim.schedule(function()
                    write_to_dapui_console(lines)
                    -- write_to_dapui_repl(lines)
                end)
            end,

            --[[ on_init = function(self, task)
                -- Called when the task is created
                -- This is a good place to initialize resources, if needed
            end,
            ---@return nil|boolean
            on_pre_start = function(self, task)
                -- Return false to prevent task from starting
            end,
            on_start = function(self, task)
                -- Called when the task is started
            end,
            on_reset = function(self, task)
                -- Called when the task is reset to run again
            end,
            ---@return table
            on_pre_result = function(self, task)
                -- Called when the task is finalizing.
                -- Return a map-like table value here to merge it into the task result.
                return { foo = { "bar", "baz" } }
            end,
            ---@param result table A result table.
            on_preprocess_result = function(self, task, result)
                -- Called right before on_result. Intended for logic that needs to preprocess the result table and update it in-place.
            end,
            ---@param result table A result table.
            on_result = function(self, task, result)
                -- Called when a component has results to set. Usually this is after the command has completed, but certain types of tasks may wish to set a result while still running.
            end,
            ---@param status overseer.Status Can be CANCELED, FAILURE, or SUCCESS
            ---@param result table A result table.
            on_complete = function(self, task, status, result)
                -- Called when the task has reached a completed state.
            end,
            ---@param status overseer.Status
            on_status = function(self, task, status)
                -- Called when the task status changes
            end,
            ---@param data string[] Output of process. See :help channel-lines
            on_output = function(self, task, data)
                -- Called when there is output from the task
            end,
            ---@param lines string[] Completed lines of output, with ansi codes removed.
            on_output_lines = function(self, task, lines)
                -- Called when there is output from the task
                -- Usually easier to deal with than using on_output directly.
            end,
            ---@param code number The process exit code
            on_exit = function(self, task, code)
                -- Called when the task command has completed
            end,
            on_dispose = function(self, task)
                -- Called when the task is disposed
                -- Will be called IFF on_init was called, and will be called exactly once.
                -- This is a good place to free resources (e.g. timers, files, etc)
            end, ]]
        }
    end,
}