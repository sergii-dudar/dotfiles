-- JDTLS formatter adapter: build on the base profile while overriding selected
-- Eclipse options so that manual wrapping is respected. Two rules are active:
--   * binary expressions -- the base profile joins manually wrapped arithmetic
--     (`+ - * / %`), boolean (`&& || < > <= >= == !=`), bitwise/shift
--     (`& | ^ << >> >>>`) and string-concatenation expressions back onto one
--     line. Instead, a manually wrapped expression
--     keeps the author's own line breaks and operator placement (like IntelliJ's
--     "keep line breaks"), while an expression that fits stays on one line and an
--     overflowing expression uses compact wrapping. This is achieved with global
--     compact-split overrides (`BINARY_PRESERVE_OVERRIDES`) added to every
--     formatter request, so binary preservation costs no separate requests and
--     also covers nested / mixed-precedence expressions.
--   * declaration parameters -- retain on-column wrapping unless the first
--     parameter starts on the line after the opening parenthesis. This is
--     conditional per declaration, so it still needs one range-formatting request
--     per matched parameter list.
--
-- Performance: formatting makes the normal JDTLS request, then one additional
-- synchronous range-formatting request only for each parameter list that matches
-- the parameter rule -- typically none. Every request carries the binary
-- overrides. Add more adaptive rules only when their formatting benefit justifies
-- the extra latency.
--
-- Extension: further Eclipse formatter options can be overridden globally on
-- every request, or layered on a Tree-sitter range like the parameter rule. Keep
-- every request on the original document version, resolve overlapping rule ranges
-- explicitly, and apply the merged edits once.

local M = {}

local METHOD_PARAMETERS_ALIGNMENT = "org.eclipse.jdt.core.formatter.alignment_for_parameters_in_method_declaration"
local CONSTRUCTOR_PARAMETERS_ALIGNMENT =
    "org.eclipse.jdt.core.formatter.alignment_for_parameters_in_constructor_declaration"
local DEFAULT_INDENT_ALIGNMENT = "16"

-- Global preserve overrides for binary expressions, applied to every formatter
-- request, including adaptive ranges. Compact split (16) wraps only when a line
-- overflows and, with `join_wrapped_lines=false`, keeps the author's own line
-- breaks and grouping; an expression that fits stays on one line.
-- `wrap_before_*` is `true` for every operator category: it is both the operator
-- placement used when the formatter has to wrap an overflowing line (operator
-- leads the continuation line, matching the string/boolean style) and, crucially,
-- the setting that lets a manual break placed *before* an operator (`a\n+ b`) be
-- preserved -- with `false` such leading breaks are normalised away and the
-- operands join back onto one line. Leading placement still preserves existing
-- trailing breaks (`a +\nb`), so it is strictly the better choice for keeping
-- manual wrapping. String concatenation additionally requires its `wrap_before`
-- to be enabled for the alignment to take effect at all. Every option is passed
-- per request so the result does not depend on the profile the language server
-- happens to have cached. Applying these globally is deliberate: compact split
-- preserves expressions that already fit, uses compact wrapping for overflowing
-- expressions, and covers nested and mixed-precedence expressions that a
-- single-operator range pass would miss.
local BINARY_PRESERVE_OVERRIDES = {
    ["org.eclipse.jdt.core.formatter.alignment_for_string_concatenation"] = "16",
    ["org.eclipse.jdt.core.formatter.wrap_before_string_concatenation"] = "true",
    ["org.eclipse.jdt.core.formatter.alignment_for_additive_operator"] = "16",
    ["org.eclipse.jdt.core.formatter.wrap_before_additive_operator"] = "true",
    ["org.eclipse.jdt.core.formatter.alignment_for_multiplicative_operator"] = "16",
    ["org.eclipse.jdt.core.formatter.wrap_before_multiplicative_operator"] = "true",
    ["org.eclipse.jdt.core.formatter.alignment_for_logical_operator"] = "16",
    ["org.eclipse.jdt.core.formatter.wrap_before_logical_operator"] = "true",
    ["org.eclipse.jdt.core.formatter.alignment_for_relational_operator"] = "16",
    ["org.eclipse.jdt.core.formatter.wrap_before_relational_operator"] = "true",
    ["org.eclipse.jdt.core.formatter.alignment_for_bitwise_operator"] = "16",
    ["org.eclipse.jdt.core.formatter.wrap_before_bitwise_operator"] = "true",
    ["org.eclipse.jdt.core.formatter.alignment_for_shift_operator"] = "16",
    ["org.eclipse.jdt.core.formatter.wrap_before_shift_operator"] = "true",
    ["org.eclipse.jdt.core.formatter.join_wrapped_lines"] = "false",
}

local FORMAT_TIMEOUT_MS = 3000

local PARAMETERS_QUERY = [[
    [
        (method_declaration parameters: (formal_parameters) @parameters)
        (constructor_declaration parameters: (formal_parameters) @parameters)
    ]
]]

local query_cache = {}

--- Resolve the current-buffer sentinel used by formatter callbacks.
---@param bufnr integer|nil
---@return integer
local function resolve_bufnr(bufnr)
    if not bufnr or bufnr == 0 then
        return vim.api.nvim_get_current_buf()
    end
    return bufnr
end

--- Return a cached compiled Java Tree-sitter query for the given source.
---@param source string
---@return vim.treesitter.Query|nil
local function get_query(source)
    if query_cache[source] then
        return query_cache[source]
    end
    local ok, query = pcall(vim.treesitter.query.parse, "java", source)
    if ok then
        query_cache[source] = query
    end
    return query_cache[source]
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
---@return { range: lsp.Range, overrides: table<string, string> }[]
local function get_adaptive_ranges(bufnr, selection)
    local query = get_query(PARAMETERS_QUERY)
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
                ranges[#ranges + 1] = { range = range, overrides = { [option] = DEFAULT_INDENT_ALIGNMENT } }
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

--- Build formatting options with global binary preservation and optional range overrides.
---@param bufnr integer
---@param overrides table<string, string>|nil
---@return table
local function formatting_options(bufnr, overrides)
    return vim.tbl_extend("force", {
        tabSize = vim.lsp.util.get_effective_tabstop(bufnr),
        insertSpaces = vim.bo[bufnr].expandtab,
    }, BINARY_PRESERVE_OVERRIDES, overrides or {})
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

--- Check whether two ranges share any span.
---@param left lsp.Range
---@param right lsp.Range
---@return boolean
local function ranges_intersect(left, right)
    return compare_positions(left.start, right["end"]) < 0 and compare_positions(right.start, left["end"]) < 0
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
    -- Binary-expression wrapping is preserved through global overrides on every
    -- request; only the parameter rule still needs per-declaration range requests.
    local base_edits = request_edits(client, bufnr, base_method, base_range, formatting_options(bufnr))

    local adaptive_edits = {}
    local accepted_ranges = {}
    for _, item in ipairs(adaptive_ranges) do
        local range = to_lsp_range(bufnr, item.range, client.offset_encoding)
        local overlaps = false
        for _, accepted in ipairs(accepted_ranges) do
            if ranges_intersect(range, accepted) then
                overlaps = true
                break
            end
        end
        if not overlaps then
            accepted_ranges[#accepted_ranges + 1] = range
            local options = formatting_options(bufnr, item.overrides)
            adaptive_edits[#adaptive_edits + 1] = {
                range = range,
                edits = request_edits(client, bufnr, "textDocument/rangeFormatting", range, options),
            }
        end
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
        name = "JDTLS adaptive Java",
        primary = true,
        priority = 110,
        format = M.format,
        sources = M.sources,
    })
end

return M
