-- JSON normalization: clean up stringified/escaped JSON in the current buffer.
--
-- • normalize_buffer — remove escaping, unwrap quoted objects/arrays in current buffer
-- • normalize_selection — remove escaping, unwrap quoted objects/arrays in visual selection

local M = {}

--- Normalize stringified JSON content:
--- - Remove newlines (\n)
--- - Convert "{ to { and }" to }
--- - Convert "[ to [ and ]" to ]
--- - Convert escaped quotes \" to "
--- @param content string
--- @return string
local function normalize_content(content)
    -- Remove literal \n and \r
    content = content:gsub("\\n", ""):gsub("\\r", "")

    -- Remove escaped quotes: \" -> "
    content = content:gsub('\\"', '"')

    -- Remove string-wrapped braces/brackets: "{ -> {, }" -> }, "[ -> [, ]" -> ]
    content = content:gsub('"{', "{"):gsub('}"', "}")
    content = content:gsub('"%[', "["):gsub('%]"', "]")

    -- Format with jq if available
    local jq_result = vim.fn.system("echo " .. vim.fn.shellescape(content) .. " | jq .", "")
    if vim.v.shell_error == 0 and jq_result and jq_result ~= "" then
        content = jq_result
    end

    return content
end

--- Normalize stringified JSON in the current buffer.
function M.normalize_buffer()
    local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
    local content = normalize_content(table.concat(lines, ""))
    local new_lines = vim.split(content, "\n", { trimempty = true })
    vim.api.nvim_buf_set_lines(0, 0, -1, false, new_lines)
end

--- Normalize stringified JSON in the current visual selection.
function M.normalize_selection()
    local start_pos = vim.fn.getpos("'<")
    local end_pos = vim.fn.getpos("'>")
    local start_row = start_pos[2] - 1
    local start_col = start_pos[3] - 1
    local end_row = end_pos[2] - 1
    local end_col = end_pos[3]

    -- Handle end of line ($) selection
    if end_col >= 2147483647 then
        end_col = #vim.api.nvim_buf_get_lines(0, end_row, end_row + 1, false)[1]
    end

    local lines = vim.api.nvim_buf_get_text(0, start_row, start_col, end_row, end_col, {})
    local content = normalize_content(table.concat(lines, ""))
    local new_lines = vim.split(content, "\n", { trimempty = true })
    vim.api.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, new_lines)
end

return M
