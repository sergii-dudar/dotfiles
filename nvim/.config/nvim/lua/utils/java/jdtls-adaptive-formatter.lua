-- JDTLS formatter adapter: retain on-column wrapping unless a declaration's
-- first parameter starts on the line after the opening parenthesis.

local M = {}

local METHOD_PARAMETERS_ALIGNMENT = "org.eclipse.jdt.core.formatter.alignment_for_parameters_in_method_declaration"
local CONSTRUCTOR_PARAMETERS_ALIGNMENT =
    "org.eclipse.jdt.core.formatter.alignment_for_parameters_in_constructor_declaration"
local DEFAULT_INDENT_ALIGNMENT = "16"
local FORMAT_TIMEOUT_MS = 3000

local PARAMETERS_QUERY = [[
    [
        (method_declaration parameters: (formal_parameters) @parameters)
        (constructor_declaration parameters: (formal_parameters) @parameters)
    ]
]]

local query_cache

--- Resolve the current-buffer sentinel used by formatter callbacks.
---@param bufnr integer|nil
---@return integer
local function resolve_bufnr(bufnr)
    if not bufnr or bufnr == 0 then
        return vim.api.nvim_get_current_buf()
    end
    return bufnr
end

--- Return the cached Java query for method and constructor parameter lists.
---@return vim.treesitter.Query|nil
local function get_query()
    if query_cache then
        return query_cache
    end
    local ok, query = pcall(vim.treesitter.query.parse, "java", PARAMETERS_QUERY)
    if ok then
        query_cache = query
    end
    return query_cache
end

--- Return the attached JDTLS client when it supports document and range formatting.
---@param bufnr integer
---@return vim.lsp.Client|nil
local function get_client(bufnr)
    local clients = vim.lsp.get_clients({
        bufnr = bufnr,
        name = "jdtls",
        method = "textDocument/formatting",
    })
    for _, client in ipairs(clients) do
        if client:supports_method("textDocument/rangeFormatting", bufnr) then
            return client
        end
    end
    return nil
end

--- Compare two zero-based positions.
---@param left lsp.Position
---@param right lsp.Position
---@return integer
local function compare_positions(left, right)
    if left.line ~= right.line then
        return left.line < right.line and -1 or 1
    end
    if left.character == right.character then
        return 0
    end
    return left.character < right.character and -1 or 1
end

--- Return the active characterwise or linewise Visual selection as a byte range.
---@param bufnr integer
---@return lsp.Range|nil
local function get_visual_range(bufnr)
    local mode = vim.api.nvim_get_mode().mode
    if mode ~= "v" and mode ~= "V" then
        return nil
    end

    local start_position = vim.fn.getpos("v")
    local end_position = vim.fn.getpos(".")
    local start = { line = start_position[2] - 1, character = start_position[3] - 1 }
    local finish = { line = end_position[2] - 1, character = end_position[3] - 1 }
    if compare_positions(finish, start) < 0 then
        start, finish = finish, start
    end

    if mode == "V" then
        start.character = 0
        local line = vim.api.nvim_buf_get_lines(bufnr, finish.line, finish.line + 1, true)[1] or ""
        finish.character = #line
    elseif vim.o.selection ~= "exclusive" then
        finish.character = finish.character + 1
    end
    return { start = start, ["end"] = finish }
end

--- Check whether a range is fully contained in an optional selection.
---@param range lsp.Range
---@param selection lsp.Range|nil
---@return boolean
local function is_in_selection(range, selection)
    return not selection
        or (
            compare_positions(selection.start, range.start) <= 0
            and compare_positions(range["end"], selection["end"]) <= 0
        )
end

--- Collect parameter lists that intentionally start on the line after `(`.
---@param bufnr integer
---@param selection lsp.Range|nil
---@return { range: lsp.Range, option: string }[]
local function get_adaptive_ranges(bufnr, selection)
    local query = get_query()
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not query or not ok or not parser then
        return {}
    end

    local trees = parser:parse()
    if not trees or not trees[1] then
        return {}
    end

    local ranges = {}
    for _, parameters in query:iter_captures(trees[1]:root(), bufnr, 0, -1) do
        local first_parameter = parameters:named_child(0)
        if first_parameter then
            local start_row, start_col, end_row, end_col = parameters:range()
            local range = {
                start = { line = start_row, character = start_col },
                ["end"] = { line = end_row, character = end_col },
            }
            local first_row = first_parameter:range()
            if first_row > start_row and is_in_selection(range, selection) then
                local parent = parameters:parent()
                local option = parent
                        and parent:type() == "constructor_declaration"
                        and CONSTRUCTOR_PARAMETERS_ALIGNMENT
                    or METHOD_PARAMETERS_ALIGNMENT
                ranges[#ranges + 1] = { range = range, option = option }
            end
        end
    end
    return ranges
end

--- Convert a byte-based Tree-sitter position to the JDTLS character encoding.
---@param bufnr integer
---@param position lsp.Position
---@param encoding string
---@return lsp.Position
local function to_lsp_position(bufnr, position, encoding)
    return {
        line = position.line,
        character = vim.lsp.util.character_offset(bufnr, position.line, position.character, encoding),
    }
end

--- Convert a byte-based range to the JDTLS character encoding.
---@param bufnr integer
---@param range lsp.Range
---@param encoding string
---@return lsp.Range
local function to_lsp_range(bufnr, range, encoding)
    return {
        start = to_lsp_position(bufnr, range.start, encoding),
        ["end"] = to_lsp_position(bufnr, range["end"], encoding),
    }
end

--- Build formatting options for the target buffer with optional Eclipse overrides.
---@param bufnr integer
---@param overrides table<string, string>|nil
---@return table
local function formatting_options(bufnr, overrides)
    return vim.tbl_extend("force", {
        tabSize = vim.lsp.util.get_effective_tabstop(bufnr),
        insertSpaces = vim.bo[bufnr].expandtab,
    }, overrides or {})
end

--- Request formatter edits without applying them to the buffer.
---@param client vim.lsp.Client
---@param bufnr integer
---@param method string
---@param range lsp.Range|nil
---@param options table
---@return lsp.TextEdit[]
local function request_edits(client, bufnr, method, range, options)
    local params = vim.lsp.util.make_formatting_params(options)
    params.textDocument = vim.lsp.util.make_text_document_params(bufnr)
    params.range = range

    local response, wait_error = client:request_sync(method, params, FORMAT_TIMEOUT_MS, bufnr)
    local request_error = (response and response.err) or wait_error
    if request_error then
        local message = type(request_error) == "table" and request_error.message or tostring(request_error)
        error(string.format("[LSP][jdtls] %s", message))
    end
    return (response and response.result) or {}
end

--- Check whether an edit overlaps a parameter range.
---@param edit lsp.TextEdit
---@param range lsp.Range
---@return boolean
local function ranges_overlap(edit, range)
    local edit_range = edit.range
    if compare_positions(edit_range.start, edit_range["end"]) == 0 then
        return compare_positions(range.start, edit_range.start) <= 0
            and compare_positions(edit_range.start, range["end"]) < 0
    end
    return compare_positions(edit_range.start, range["end"]) < 0
        and compare_positions(range.start, edit_range["end"]) < 0
end

--- Merge base formatter edits with adaptive range edits from the same document version.
---@param base_edits lsp.TextEdit[]
---@param adaptive_edits { range: lsp.Range, edits: lsp.TextEdit[] }[]
---@return lsp.TextEdit[]
local function merge_edits(base_edits, adaptive_edits)
    local merged = {}
    for _, edit in ipairs(base_edits) do
        local overlaps = false
        for _, adaptive in ipairs(adaptive_edits) do
            if ranges_overlap(edit, adaptive.range) then
                overlaps = true
                break
            end
        end
        if not overlaps then
            merged[#merged + 1] = edit
        end
    end
    for _, adaptive in ipairs(adaptive_edits) do
        vim.list_extend(merged, adaptive.edits)
    end
    return merged
end

--- Format Java with JDTLS, adapting only newline-first declaration parameter lists.
---@param bufnr integer
function M.format(bufnr)
    bufnr = resolve_bufnr(bufnr)
    local client = get_client(bufnr)
    if not client then
        return
    end

    local selection = get_visual_range(bufnr)
    local adaptive_ranges = get_adaptive_ranges(bufnr, selection)
    local base_method = selection and "textDocument/rangeFormatting" or "textDocument/formatting"
    local base_range = selection and to_lsp_range(bufnr, selection, client.offset_encoding) or nil
    local base_edits = request_edits(client, bufnr, base_method, base_range, formatting_options(bufnr))

    local adaptive_edits = {}
    for _, item in ipairs(adaptive_ranges) do
        local range = to_lsp_range(bufnr, item.range, client.offset_encoding)
        local options = formatting_options(bufnr, { [item.option] = DEFAULT_INDENT_ALIGNMENT })
        adaptive_edits[#adaptive_edits + 1] = {
            range = range,
            edits = request_edits(client, bufnr, "textDocument/rangeFormatting", range, options),
        }
    end

    local edits = merge_edits(base_edits, adaptive_edits)
    if #edits > 0 then
        vim.lsp.util.apply_text_edits(edits, bufnr, client.offset_encoding)
    end
end

--- Report JDTLS as available only when both formatter requests are supported.
---@param bufnr integer
---@return string[]
function M.sources(bufnr)
    bufnr = resolve_bufnr(bufnr)
    return get_client(bufnr) and { "jdtls" } or {}
end

--- Register the adaptive Java formatter ahead of LazyVim's generic Conform formatter.
function M.setup()
    if M._registered or not LazyVim or not LazyVim.format then
        return
    end
    M._registered = true
    LazyVim.format.register({
        name = "JDTLS adaptive parameters",
        primary = true,
        priority = 110,
        format = M.format,
        sources = M.sources,
    })
end

return M
