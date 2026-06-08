-- Pipes a debug task's output to the DAP UI console AND drives output-based DAP
-- attach for languages whose debug flow is an overseer task rather than a native
-- dap launch (Java JDWP, C# VSTest testhost).
--
-- This component is language-agnostic: the per-language "when to attach" logic
-- lives in each runner (plugins/overseer/tasks/lang/<lang>-runner.lua) as a
-- `dap_output_attacher = { name, match, attach }`. The task is built with a
-- `filetype` param; we resolve that runner and use ONLY its attacher (so no
-- cross-language matchers run). If a language has no attacher, we just stream
-- output to the console.

local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")
local log = require("utils.logging-util").new({
    name = "dap-ctrl-component",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local function write_to_dapui_console(lines)
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
            vim.bo[console_buf].filetype = "log" -- highlight logs (log-highlight.nvim)
        end
    end
end

local function clean_dapui_console()
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

local function write_to_dapui_repl(lines)
    local ok, dap = pcall(require, "dap")
    if ok and dap.session() then
        for _, line in ipairs(lines) do
            dap.repl.append(line)
        end
    end
end

---@type overseer.ComponentFileDefinition
return {
    desc = "Pipe debug task output to the DAP UI console and drive output-based DAP attach",
    params = {
        filetype = {
            type = "string",
            optional = true,
            desc = "Filetype used to resolve the language's dap_output_attacher",
        },
    },
    constructor = function(params)
        local attached = false
        -- nil = not resolved yet, false = none for this filetype, table = attacher
        local attacher

        ---@return task.lang.DapOutputAttacher|false
        local function resolve_attacher()
            if attacher ~= nil then
                return attacher
            end
            local ft = params and params.filetype
            local runner = ft and lang_runner_resolver.resolve(ft) or nil
            attacher = (runner and runner.dap_output_attacher) or false
            log.debug(
                "resolve_attacher: filetype=" .. tostring(ft) .. " attacher=" .. tostring(attacher and attacher.name)
            )
            return attacher
        end

        --- Scan output lines for the language's attach banner; attach once.
        ---@param lines string[]
        local function try_attach(lines)
            if attached or not lines then
                return
            end
            local att = resolve_attacher()
            if not att then
                return
            end
            for _, line in ipairs(lines) do
                local target = line and att.match(line)
                if target then
                    attached = true
                    log.debug("attaching via " .. att.name .. " target=" .. tostring(target))
                    att.attach(target)
                    break
                end
            end
        end

        return {
            on_start = function(self, task)
                -- Clear console buffer when task starts
                vim.schedule(clean_dapui_console)
                attached = false
            end,
            on_output_lines = function(self, task, lines)
                -- Per-language output-driven DAP attach (Java JDWP / C# testhost).
                -- Scans ALL provided lines (not just the first) since the banner may
                -- be preceded by other output.
                try_attach(lines)

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
