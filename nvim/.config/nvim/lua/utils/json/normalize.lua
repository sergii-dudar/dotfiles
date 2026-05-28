-- JSON normalization: clean up stringified/escaped JSON in the current buffer.
--
-- • normalize_buffer — remove escaping, unwrap quoted objects/arrays in current buffer

local M = {}

--- Normalize stringified JSON in the current buffer:
--- - Remove newlines (\n)
--- - Convert "{ to { and }" to }
--- - Convert "[ to [ and ]" to ]
--- - Convert escaped quotes \" to "
function M.normalize_buffer()
    local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
    local content = table.concat(lines, "")

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

    local new_lines = vim.split(content, "\n", { trimempty = true })
    vim.api.nvim_buf_set_lines(0, 0, -1, false, new_lines)
end

return M
