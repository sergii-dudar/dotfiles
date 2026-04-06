local M = {}

-- Dedicated namespace so we never stomp on jdtls diagnostics
local NS = vim.api.nvim_create_namespace("java_arg_mismatch")

-- Matches JDTLS "not applicable for the arguments" messages, e.g.:
--   The method foo(String, Integer) in the type Bar is not applicable for the arguments (String, String)
local METHOD_PATTERN =
    "The method [%w_]+%((.-)%) in the type [%w_<>%[%]%.,%%$]+ is not applicable for the arguments %((.-)%)"
local APPLICABLE_NEEDLE = "is not applicable for the arguments"

-- Parse a comma-separated type list while respecting generic angle brackets.
-- Uses position tracking (no string concatenation in the loop).
-- e.g. "String, Map<String, Integer>, BigDecimal" -> {"String", "Map<String, Integer>", "BigDecimal"}
local function parse_type_list(s)
    local types = {}
    local depth = 0
    local start = 1
    for i = 1, #s do
        local c = s:sub(i, i)
        if c == "<" then
            depth = depth + 1
        elseif c == ">" then
            depth = depth - 1
        elseif c == "," and depth == 0 then
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

-- Strip generics and package prefix to get a simple class name.
-- e.g. "java.util.List<String>" -> "List"
local function simple_name(t)
    local base = t:match("^([%w_%.%[%]]+)") or t
    return base:match("[^%.]+$") or base
end

local function types_differ(expected, provided)
    if expected == provided then
        return false
    end
    return simple_name(expected) ~= simple_name(provided)
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
    if not msg or not msg:find(APPLICABLE_NEEDLE, 1, true) then
        return
    end

    local expected_str, provided_str = msg:match(METHOD_PATTERN)
    if not expected_str then
        return
    end

    local expected = parse_type_list(expected_str)
    local provided = parse_type_list(provided_str)

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
        if diag.message and diag.message:find(APPLICABLE_NEEDLE, 1, true) then
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
    vim.diagnostic.set(NS, bufnr, build_arg_diags(bufnr, lsp_diagnostics))
end

return M
