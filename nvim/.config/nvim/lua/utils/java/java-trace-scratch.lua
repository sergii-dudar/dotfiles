-- Java stack trace scratch buffer: open a Snacks scratch pad for pasting and navigating traces.
--
-- • openStackTraceScratch — open a scratch buffer with trace normalization and navigation keymaps

local M = {}

--- Escape sequences decoded when an inline (JSON-style) stack trace is unfolded.
local TRACE_ESCAPES = { n = "\n", r = "\r", t = "\t", ['"'] = '"', ["\\"] = "\\", ["/"] = "/" }

--- Marks a line as an inline trace: a literal \n followed by a stack frame ("at ...").
local INLINE_FRAME_PATTERN = "\\n[\\tr ]*at%s"

--- Upper bound of unescape passes per line (covers traces escaped more than once).
local MAX_UNESCAPE_PASSES = 3

--- Unfold one line holding an inline trace by decoding its escape sequences left to right.
--- Lines without an escaped frame are returned untouched, so backslashes in regular
--- text (Windows paths, regexes) survive.
local function unescape_inline_trace_line(line)
    for _ = 1, MAX_UNESCAPE_PASSES do
        if not line:find(INLINE_FRAME_PATTERN) then
            break
        end
        line = line:gsub("\\(.)", TRACE_ESCAPES)
    end
    return line
end

--- Convert an inline stack trace (literal \n, \t, \r\n, \", \\) into a multi-line one.
local function normalize_trace_text(text)
    local lines = vim.split(text, "\n", { plain = true })
    for i, line in ipairs(lines) do
        lines[i] = unescape_inline_trace_line(line)
    end
    local normalized = table.concat(lines, "\n"):gsub("\r\n?", "\n")
    return normalized
end

--- Rewrite the buffer with its inline stack traces unfolded into trimmed, multi-line ones.
--- See normalize_trace_text() for which escape sequences are decoded and when.
local function normalize_trace_buffer(buf)
    local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local text = table.concat(lines, "\n")
    -- unfold literal \n / \t / \r\n of lines holding an inline trace
    local normalized = normalize_trace_text(text)
    local new_lines = vim.split(normalized, "\n", { plain = true })
    for i, line in ipairs(new_lines) do
        new_lines[i] = vim.trim(line)
    end
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, new_lines)
end

--- Replace the buffer content with the system clipboard and normalize it.
local function replace_buffer_with_clipboard(buf)
    local clipboard_text = vim.fn.getreg("+")
    local lines = vim.split(clipboard_text, "\n", { plain = true })

    if clipboard_text == "" then
        lines = {}
    end

    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    normalize_trace_buffer(buf)
    vim.notify("💾 Scratch content replaced from clipboard")
end

--- Return true when the text holds at least one parsable Java stack frame.
local function has_trace_frames(text)
    local java_common = require("utils.java.java-common")
    return #java_common.parse_java_mvn_run_class_text(text) > 0
end

--- Normalize the trace, close the scratch window and show the frames in the quickfix list.
--- Without any parsable frame the scratch stays open and the quickfix list is left untouched.
local function send_trace_to_qflist(win, stack_trace)
    local normalized = normalize_trace_text(stack_trace)
    if not has_trace_frames(normalized) then
        vim.notify("⚠️ No Java stack trace frames found", vim.log.levels.WARN)
        return
    end
    local java_trace = require("utils.java.java-trace")
    win:close()
    java_trace.show_stack_trace_qflist(normalized)
end

--- Open a scratch buffer for stack trace navigation.
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
                        local common = require("utils.common-util")
                        local stack_trace = common.get_buffer_text(self.buf)
                        send_trace_to_qflist(self, stack_trace)
                    end,
                    desc = "Buf trace to QF",
                    mode = "n",
                },
                ["parse_selected_trace"] = {
                    -- "<leader>v",
                    "<cr>",
                    function(self)
                        local common = require("utils.common-util")
                        local stack_trace = common.get_visual_selection()
                        send_trace_to_qflist(self, stack_trace)
                    end,
                    desc = "Selected trace to QF",
                    mode = "x",
                },
                ["replace_with_clipboard"] = {
                    "<leader>r",
                    function(self)
                        replace_buffer_with_clipboard(self.buf)
                    end,
                    desc = "Replace with Clipboard",
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
