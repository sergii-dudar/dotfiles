local M = {}

-- Dedicated namespace so we never stomp on jdtls diagnostics
local NS = vim.api.nvim_create_namespace("java_arg_mismatch")
local HL_NS = vim.api.nvim_create_namespace("java_arg_mismatch_hl")

-- Parse a comma-separated type list while respecting generic angle brackets.
-- Uses position tracking (no string concatenation in the loop).
-- e.g. "String, Map<String, Integer>, BigDecimal" -> {"String", "Map<String, Integer>", "BigDecimal"}
local BYTE_LT = string.byte("<")
local BYTE_GT = string.byte(">")
local BYTE_COMMA = string.byte(",")

local function parse_type_list(s)
    local types = {}
    local depth = 0
    local start = 1
    for i = 1, #s do
        local b = s:byte(i)
        if b == BYTE_LT then
            depth = depth + 1
        elseif b == BYTE_GT then
            depth = depth - 1
        elseif b == BYTE_COMMA and depth == 0 then
            local t = s:sub(start, i - 1):match("^%s*(.-)%s*$")
            if t ~= "" then
                types[#types + 1] = t
            end
            start = i + 1
        end
    end
    local t = s:sub(start):match("^%s*(.-)%s*$")
    if t ~= "" then
        types[#types + 1] = t
    end
    return types
end

-- Strip generics, array suffix, and package prefix (lowercase segments) but KEEP
-- nested-class qualifiers (uppercase segments) so a nested class is not collapsed
-- to its inner name and falsely treated as equal to a top-level class.
--   "java.util.List<String>"     -> "List"
--   "ChargesInformation.Charge"  -> "ChargesInformation.Charge"
--   "java.util.Map.Entry"        -> "Map.Entry"
local function simple_name(t)
    local base = t:match("^([%w_%.%[%]]+)") or t
    local parts = {}
    for seg in base:gmatch("[^%.]+") do
        parts[#parts + 1] = seg
    end
    for i, p in ipairs(parts) do
        local c = p:sub(1, 1)
        if c >= "A" and c <= "Z" then
            return table.concat(parts, ".", i)
        end
    end
    return base
end

local function types_differ(expected, provided)
    if expected == provided then
        return false
    end
    return simple_name(expected) ~= simple_name(provided)
end

-- ---------------------------------------------------------------------------
-- Diagnostic message handlers
--
-- Each handler pulls expected/provided type lists out of a specific JDTLS
-- diagnostic message. To support a new message format, add a new entry.
--
--   needle: cheap literal substring used for fast pre-filtering
--   match:  function(msg) -> (expected_types, provided_types) | nil
-- ---------------------------------------------------------------------------
local HANDLERS = {
    -- "The method foo(A, B) in the type Bar is not applicable for the arguments (A, C)"
    {
        needle = "is not applicable for the arguments",
        match = function(msg)
            local expected, provided = msg:match(
                "The method [%w_]+%((.-)%) in the type [%w_<>%[%]%.,%%$]+ is not applicable for the arguments %((.-)%)"
            )
            if not expected then
                return nil
            end
            return parse_type_list(expected), parse_type_list(provided)
        end,
    },
}

local function diag_matches_any_handler(msg)
    for _, h in ipairs(HANDLERS) do
        if msg:find(h.needle, 1, true) then
            return true
        end
    end
    return false
end

local function extract_types(msg)
    for _, h in ipairs(HANDLERS) do
        if msg:find(h.needle, 1, true) then
            local expected, provided = h.match(msg)
            if expected then
                return expected, provided
            end
        end
    end
    return nil
end

-- Walk UP the treesitter tree from (row, col) until we find a method_invocation
-- or object_creation_expression, then return its argument_list child.
-- Accepts a pre-parsed root to avoid redundant parser:parse() calls.
local function find_argument_list(root, row, col)
    local node = root:named_descendant_for_range(row, col, row, col)
    while node do
        local t = node:type()
        if t == "method_invocation" or t == "object_creation_expression" or t == "explicit_generic_invocation" then
            for child in node:iter_children() do
                if child:type() == "argument_list" then
                    return child
                end
            end
        end
        node = node:parent()
    end
    return nil
end

-- Collect argument nodes from an argument_list, skipping parentheses and commas.
local function collect_arg_nodes(arg_list)
    local args = {}
    for child in arg_list:iter_children() do
        local t = child:type()
        if t ~= "(" and t ~= ")" and t ~= "," then
            args[#args + 1] = child
        end
    end
    return args
end

-- Process a single diagnostic against a pre-parsed tree root,
-- appending any per-argument highlights to `out`.
local function process_diag(root, diag, out)
    local msg = diag.message
    if not msg then
        return
    end

    local expected, provided = extract_types(msg)
    if not expected then
        return
    end

    -- Nothing to highlight when fewer args are provided than expected
    -- (can't pinpoint which ones are wrong without overload resolution)
    if #provided < #expected then
        return
    end

    local row = diag.range and diag.range.start and diag.range.start.line or 0
    local col = diag.range and diag.range.start and diag.range.start.character or 0

    local arg_list = find_argument_list(root, row, col)
    if not arg_list then
        return
    end

    local arg_nodes = collect_arg_nodes(arg_list)

    -- Highlight type-mismatched args in the expected range
    for i = 1, math.min(#expected, #arg_nodes) do
        if types_differ(expected[i], provided[i]) then
            local sr, sc, er, ec = arg_nodes[i]:range()
            out[#out + 1] = {
                lnum = sr,
                col = sc,
                end_lnum = er,
                end_col = ec,
                severity = vim.diagnostic.severity.WARN,
                message = string.format("arg %d: expected %s, got %s", i, expected[i], provided[i]),
                source = "jdtls-arg",
            }
        end
    end

    -- Highlight extra arguments beyond what the method accepts
    for i = #expected + 1, #arg_nodes do
        local sr, sc, er, ec = arg_nodes[i]:range()
        out[#out + 1] = {
            lnum = sr,
            col = sc,
            end_lnum = er,
            end_col = ec,
            severity = vim.diagnostic.severity.WARN,
            message = string.format("extra argument: %s", provided[i] or "?"),
            source = "jdtls-arg",
        }
    end
end

-- Process LSP-format diagnostics for one buffer and return per-argument diagnostics
-- in Neovim native format (lnum/col/end_lnum/end_col).
local function build_arg_diags(bufnr, lsp_diagnostics)
    -- Pre-scan: bail out before touching treesitter if no matching diagnostics
    local has_match = false
    for _, diag in ipairs(lsp_diagnostics) do
        if diag.message and diag_matches_any_handler(diag.message) then
            has_match = true
            break
        end
    end
    if not has_match then
        return {}
    end

    -- Parse the treesitter tree once for the whole batch
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return {}
    end
    local trees = parser:parse()
    if not trees or not trees[1] then
        return {}
    end
    local root = trees[1]:root()

    local result = {}
    for _, diag in ipairs(lsp_diagnostics) do
        process_diag(root, diag, result)
    end
    return result
end

-- Entry point: call from publishDiagnostics handler with pre-filtered Java diagnostics.
function M.apply(bufnr, lsp_diagnostics)
    if not vim.api.nvim_buf_is_valid(bufnr) or not vim.api.nvim_buf_is_loaded(bufnr) then
        return
    end
    local diags = build_arg_diags(bufnr, lsp_diagnostics)
    vim.diagnostic.set(NS, bufnr, diags)

    vim.api.nvim_buf_clear_namespace(bufnr, HL_NS, 0, -1)
    for _, d in ipairs(diags) do
        vim.api.nvim_buf_set_extmark(bufnr, HL_NS, d.lnum, d.col, {
            end_row = d.end_lnum,
            end_col = d.end_col,
            hl_group = "JavaFormatBad",
        })
    end
end

return M
