local M = {}

local function parse_check_output(output, bufnr)
    if not output or output == "" then
        return {}
    end

    local lines = vim.split(output, "\n", { plain = true })

    -- Extract error message (first line), error line content ("> N | ..."), and column ("^")
    local message = (lines[1] or ""):gsub("%s*%(%d+:%d+%)%s*$", "")
    local error_content, caret_col
    for _, line in ipairs(lines) do
        -- "> 3 |     "type": ""example","
        local content = line:match("^>%s*%d+%s*|%s?(.*)")
        if content then
            error_content = content
        end
        -- "    |               ^"
        local prefix = line:match("^%s*|(%s*%^)")
        if prefix then
            caret_col = #prefix - 1
        end
    end

    if not error_content then
        return {}
    end

    -- Find the matching line in the buffer using error content + surrounding context
    local buf_lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

    -- Collect context lines (non-error numbered lines) for disambiguation
    local before_ctx, after_ctx
    local seen_error = false
    for _, line in ipairs(lines) do
        if line:match("^>%s*%d+%s*|") then
            seen_error = true
        elseif line:match("^%s*%d+%s*|") then
            local ctx = line:match("^%s*%d+%s*|%s?(.*)")
            if ctx then
                if not seen_error then
                    before_ctx = ctx
                elseif not after_ctx then
                    after_ctx = ctx
                end
            end
        end
    end

    local match_lnum
    for i, buf_line in ipairs(buf_lines) do
        if buf_line == error_content then
            local before_ok = not before_ctx or (i > 1 and buf_lines[i - 1] == before_ctx)
            local after_ok = not after_ctx or (i < #buf_lines and buf_lines[i + 1] == after_ctx)
            if before_ok and after_ok then
                match_lnum = i - 1
                break
            end
        end
    end

    -- Fallback: match without context
    if not match_lnum then
        for i, buf_line in ipairs(buf_lines) do
            if buf_line == error_content then
                match_lnum = i - 1
                break
            end
        end
    end

    if not match_lnum then
        return {}
    end

    local col = caret_col or 0
    return {
        {
            lnum = match_lnum,
            col = col,
            end_lnum = match_lnum,
            end_col = #buf_lines[match_lnum + 1],
            severity = vim.diagnostic.severity.ERROR,
            message = message,
            source = "kulala-fmt",
        },
    }
end

M.linter = {
    cmd = "kulala-fmt",
    stdin = true,
    args = { "check", "--stdin" },
    stream = "stdout",
    ignore_exitcode = true,
    parser = parse_check_output,
}

return M
