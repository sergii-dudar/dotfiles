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
        return {
            on_output_lines = function(self, task, lines)
                vim.schedule(function()
                    -- write_to_dapui_console(lines)
                    write_to_dapui_repl(lines)
                end)
            end,
        }
    end,
}
