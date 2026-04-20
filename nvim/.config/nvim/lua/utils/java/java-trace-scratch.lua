local M = {}

--- Replace literal escape sequences (\n, \t, \r) with actual newlines,
--- effectively converting an inline stack trace into a multi-line one.
function normalize_trace_buffer(buf)
    local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local text = table.concat(lines, "\n")
    -- split on literal \n (optionally followed by literal \t or \r)
    local normalized = text:gsub("\\n\\t", "\n\t"):gsub("\\n\\r", "\n"):gsub("\\n", "\n"):gsub("\\t", "\t")
    local new_lines = vim.split(normalized, "\n", { plain = true })
    for i, line in ipairs(new_lines) do
        new_lines[i] = vim.trim(line)
    end
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, new_lines)
end

function M.openStackTraceScratch()
    Snacks.scratch({
        name = "Stack Trace Scratch",
        ft = "log",
        win = {
            keys = {
                ["parse_trace"] = {
                    -- "<leader>p",
                    "<cr>",
                    function(self)
                        -- normalize_trace_buffer(self.buf)
                        local java_trace = require("utils.java.java-trace")
                        local common = require("utils.common-util")
                        local stack_trace = common.get_current_buffer_text()
                        self:close()
                        java_trace.show_stack_trace_qflist(stack_trace)
                    end,
                    desc = "Trace to QF",
                    mode = "n",
                },
                ["normalize_trace"] = {
                    "<leader>n",
                    function(self)
                        normalize_trace_buffer(self.buf)
                        -- vim.bo[self.buf].filetype = "log"
                    end,
                    desc = "Normalize Trace",
                    mode = "n",
                },
            },
        },
    })
end

return M
