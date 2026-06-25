-- JSON normalization: unwrap stringified JSON objects/arrays and format them.
--
-- • normalize_buffer — unwrap and format JSON in current buffer
-- • normalize_selection — unwrap and format JSON in visual selection

local M = {}

--- Decode JSON content if possible.
---@param content string
---@return any|nil
local function decode_json(content)
    local ok, decoded = pcall(vim.json.decode, content)
    if ok then
        return decoded
    end
    return nil
end

--- Check whether text looks like a JSON object or array.
---@param content string
---@return boolean
local function looks_like_json_container(content)
    local trimmed = vim.trim(content)
    local first = trimmed:sub(1, 1)
    local last = trimmed:sub(-1)
    return (first == "{" and last == "}") or (first == "[" and last == "]")
end

--- Unwrap a top-level JSON string when it contains JSON object or array text.
---@param content string
---@return string
local function unwrap_stringified_json(content)
    local decoded = decode_json(content)
    if type(decoded) ~= "string" then
        return content
    end

    local candidate = vim.trim(decoded)
    for _ = 1, 5 do
        if not looks_like_json_container(candidate) then
            return content
        end

        local nested = decode_json(candidate)
        if type(nested) ~= "string" then
            return nested ~= nil and candidate or content
        end

        local next_candidate = vim.trim(nested)
        if next_candidate == candidate then
            return content
        end
        candidate = next_candidate
    end

    return content
end

--- Format JSON content with jq when available.
---@param content string
---@return string
local function format_json(content)
    if vim.fn.executable("jq") ~= 1 then
        return content
    end

    local jq_result = vim.fn.system({ "jq", "." }, content)
    if vim.v.shell_error == 0 and jq_result and jq_result ~= "" then
        return jq_result
    end
    return content
end

--- Normalize stringified JSON content.
---@param content string
---@return string
local function normalize_content(content)
    content = vim.trim(content)
    return format_json(unwrap_stringified_json(content))
end

--- Normalize stringified JSON in the current buffer.
function M.normalize_buffer()
    local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
    local content = normalize_content(table.concat(lines, "\n"))
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
    local content = normalize_content(table.concat(lines, "\n"))
    local new_lines = vim.split(content, "\n", { trimempty = true })
    vim.api.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, new_lines)
end

return M
