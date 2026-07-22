-- JDTLS formatter adapter: keep the base profile's behaviour, but override the
-- Eclipse formatter for selected Tree-sitter ranges so that manual wrapping is
-- respected. Two rules are active:
--   * declaration parameters -- retain on-column wrapping unless the first
--     parameter starts on the line after the opening parenthesis;
--   * binary expressions -- the base profile joins manually wrapped arithmetic
--     (`+ - * / %`), boolean (`&& || < > <= >= == !=`) and string-concatenation
--     expressions back onto one line, so an expression the author split across
--     lines is re-wrapped one operand per line instead. Each operator category
--     is forced explicitly per request (`alignment_for_<category>` plus its
--     `wrap_before_<category>` placement) so the result does not depend on the
--     formatter profile the language server happens to have cached.
--
-- Performance: formatting always makes the normal JDTLS request, then one
-- additional synchronous range-formatting request for every matched range. Add
-- more adaptive rules only when their formatting benefit justifies the extra
-- latency, especially for patterns that may occur many times in a file.
--
-- Extension: other Eclipse formatter options can be overridden for selected
-- Tree-sitter ranges in the same way. Keep every request on the original
-- document version, resolve overlapping rule ranges explicitly, and apply the
-- merged edits once.

local M = {}

local METHOD_PARAMETERS_ALIGNMENT = "org.eclipse.jdt.core.formatter.alignment_for_parameters_in_method_declaration"
local CONSTRUCTOR_PARAMETERS_ALIGNMENT =
    "org.eclipse.jdt.core.formatter.alignment_for_parameters_in_constructor_declaration"
local DEFAULT_INDENT_ALIGNMENT = "16"
-- One operand per line (48), forced (+1), so the joined base output is re-wrapped.
local WRAP_ONE_PER_LINE = "49"

-- Force-wrap settings per Eclipse operator category. `align` forces one operand
-- per line; `wrap_before` / `wrap_before_value` pin the operator placement so the
-- style stays stable regardless of the profile the server has cached (arithmetic
-- and relational operators trail the line, boolean and string operators lead it).
local OPERATOR_CATEGORIES = {
    string_concatenation = {
        align = "org.eclipse.jdt.core.formatter.alignment_for_string_concatenation",
        wrap_before = "org.eclipse.jdt.core.formatter.wrap_before_string_concatenation",
        wrap_before_value = "true",
    },
    additive = {
        align = "org.eclipse.jdt.core.formatter.alignment_for_additive_operator",
        wrap_before = "org.eclipse.jdt.core.formatter.wrap_before_additive_operator",
        wrap_before_value = "false",
    },
    multiplicative = {
        align = "org.eclipse.jdt.core.formatter.alignment_for_multiplicative_operator",
        wrap_before = "org.eclipse.jdt.core.formatter.wrap_before_multiplicative_operator",
        wrap_before_value = "false",
    },
    logical = {
        align = "org.eclipse.jdt.core.formatter.alignment_for_logical_operator",
        wrap_before = "org.eclipse.jdt.core.formatter.wrap_before_logical_operator",
        wrap_before_value = "true",
    },
    relational = {
        align = "org.eclipse.jdt.core.formatter.alignment_for_relational_operator",
        wrap_before = "org.eclipse.jdt.core.formatter.wrap_before_relational_operator",
        wrap_before_value = "false",
    },
}

-- Java binary operators mapped to their Eclipse category. `+` is resolved at
-- runtime: string concatenation when a string literal participates, else additive.
local OPERATOR_CATEGORY = {
    ["+"] = "additive",
    ["-"] = "additive",
    ["*"] = "multiplicative",
    ["/"] = "multiplicative",
    ["%"] = "multiplicative",
    ["&&"] = "logical",
    ["||"] = "logical",
    ["<"] = "relational",
    [">"] = "relational",
    ["<="] = "relational",
    [">="] = "relational",
    ["=="] = "relational",
    ["!="] = "relational",
}
local FORMAT_TIMEOUT_MS = 3000

local PARAMETERS_QUERY = [[
    [
        (method_declaration parameters: (formal_parameters) @parameters)
        (constructor_declaration parameters: (formal_parameters) @parameters)
    ]
]]

local BINARY_EXPRESSION_QUERY = [[
    (binary_expression) @expression
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

--- Report whether any ancestor is itself a binary expression (i.e. not outermost).
---@param node TSNode
---@return boolean
local function has_binary_expression_ancestor(node)
    local parent = node:parent()
    while parent do
        if parent:type() == "binary_expression" then
            return true
        end
        parent = parent:parent()
    end
    return false
end

--- Report whether a `+` expression concatenates at least one string literal,
--- recursing only through the operands of nested `+` expressions.
---@param node TSNode
---@return boolean
local function concatenates_string_literal(node)
    local node_type = node:type()
    if node_type == "string_literal" then
        return true
    end
    if node_type ~= "binary_expression" then
        return false
    end
    for _, field in ipairs({ "left", "right" }) do
        local operand = node:field(field)[1]
        if operand and concatenates_string_literal(operand) then
            return true
        end
    end
    return false
end

--- Resolve formatter overrides for an outermost, manually wrapped binary
--- expression, or nil when the base formatter should keep ownership.
---@param node TSNode
---@param bufnr integer
---@return table<string, string>|nil
local function binary_expression_overrides(node, bufnr)
    if node:type() ~= "binary_expression" then
        return nil
    end
    local start_row, _, end_row = node:range()
    if start_row >= end_row then
        return nil
    end
    if has_binary_expression_ancestor(node) then
        return nil
    end
    local operator = node:field("operator")[1]
    local operator_text = operator and vim.treesitter.get_node_text(operator, bufnr)
    local category = operator_text and OPERATOR_CATEGORY[operator_text]
    if not category then
        return nil
    end
    if operator_text == "+" and concatenates_string_literal(node) then
        category = "string_concatenation"
    end
    local settings = OPERATOR_CATEGORIES[category]
    return {
        [settings.align] = WRAP_ONE_PER_LINE,
        [settings.wrap_before] = settings.wrap_before_value,
    }
end

--- Collect manually wrapped arithmetic/boolean/string expressions to re-wrap.
---@param bufnr integer
---@param selection lsp.Range|nil
---@return { range: lsp.Range, overrides: table<string, string> }[]
local function get_binary_expression_ranges(bufnr, selection)
    local query = get_query(BINARY_EXPRESSION_QUERY)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not query or not ok or not parser then
        return {}
    end

    local trees = parser:parse()
    if not trees or not trees[1] then
        return {}
    end

    local ranges = {}
    for _, expression in query:iter_captures(trees[1]:root(), bufnr, 0, -1) do
        local overrides = binary_expression_overrides(expression, bufnr)
        if overrides then
            local start_row, start_col, end_row, end_col = expression:range()
            local range = {
                start = { line = start_row, character = start_col },
                ["end"] = { line = end_row, character = end_col },
            }
            if is_in_selection(range, selection) then
                ranges[#ranges + 1] = { range = range, overrides = overrides }
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
    vim.list_extend(adaptive_ranges, get_binary_expression_ranges(bufnr, selection))
    local base_method = selection and "textDocument/rangeFormatting" or "textDocument/formatting"
    local base_range = selection and to_lsp_range(bufnr, selection, client.offset_encoding) or nil
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
        name = "JDTLS adaptive parameters",
        primary = true,
        priority = 110,
        format = M.format,
        sources = M.sources,
    })
end

return M
