local M = {}

-- Dedicated namespace so we never stomp on jdtls diagnostics
local NS = vim.api.nvim_create_namespace("java_arg_mismatch")

-- Matches JDTLS "not applicable for the arguments" messages, e.g.:
--   The method foo(String, Integer) in the type Bar is not applicable for the arguments (String, String)
local METHOD_PATTERN =
    "The method [%w_]+%((.-)%) in the type [%w_<>%[%]%.,%%$]+ is not applicable for the arguments %((.-)%)"

-- Matches constructor variant:
--   The constructor Foo(String) is not applicable for the arguments (Integer)
local CTOR_PATTERN = "The constructor [%w_<>%[%]%.,%%$]+ is not applicable for the arguments %((.-)%)"

-- Parse a comma-separated type list while respecting generic angle brackets.
-- e.g. "String, Map<String, Integer>, BigDecimal" -> {"String", "Map<String, Integer>", "BigDecimal"}
local function parse_type_list(s)
    local types = {}
    local depth = 0
    local current = ""
    for i = 1, #s do
        local c = s:sub(i, i)
        if c == "<" then
            depth = depth + 1
            current = current .. c
        elseif c == ">" then
            depth = depth - 1
            current = current .. c
        elseif c == "," and depth == 0 then
            local trimmed = current:match("^%s*(.-)%s*$")
            if trimmed ~= "" then
                table.insert(types, trimmed)
            end
            current = ""
        else
            current = current .. c
        end
    end
    local trimmed = current:match("^%s*(.-)%s*$")
    if trimmed ~= "" then
        table.insert(types, trimmed)
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
local function find_argument_list(bufnr, row, col)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return nil
    end
    local trees = parser:parse()
    if not trees or not trees[1] then
        return nil
    end
    local root = trees[1]:root()
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
            table.insert(args, child)
        end
    end
    return args
end

-- Process LSP-format diagnostics for one buffer and return per-argument diagnostics
-- in Neovim native format (lnum/col/end_lnum/end_col).
local function build_arg_diags(bufnr, lsp_diagnostics)
    local result = {}
    for _, diag in ipairs(lsp_diagnostics) do
        local msg = diag.message
        if not msg or diag.source ~= "Java" then
            goto continue
        end

        -- Early exit: skip the treesitter work if the message doesn't look like a type-mismatch
        if not msg:find("is not applicable for the arguments", 1, true) then
            goto continue
        end

        local expected_str, provided_str = msg:match(METHOD_PATTERN)
        if not expected_str then
            goto continue
        end

        local expected = parse_type_list(expected_str)
        local provided = parse_type_list(provided_str)

        -- Nothing to highlight when fewer args are provided than expected
        -- (can't pinpoint which ones are wrong without overload resolution)
        if #provided < #expected then
            goto continue
        end

        local row = diag.range and diag.range.start and diag.range.start.line or 0
        local col = diag.range and diag.range.start and diag.range.start.character or 0

        local arg_list = find_argument_list(bufnr, row, col)
        if not arg_list then
            goto continue
        end

        local arg_nodes = collect_arg_nodes(arg_list)

        -- Highlight type-mismatched args in the expected range
        for i = 1, math.min(#expected, #arg_nodes) do
            if types_differ(expected[i], provided[i]) then
                local sr, sc, er, ec = arg_nodes[i]:range()
                table.insert(result, {
                    lnum = sr,
                    col = sc,
                    end_lnum = er,
                    end_col = ec,
                    severity = vim.diagnostic.severity.WARN,
                    message = string.format("arg %d: expected %s, got %s", i, expected[i], provided[i]),
                    source = "jdtls-arg",
                })
            end
        end

        -- Highlight extra arguments beyond what the method accepts
        for i = #expected + 1, #arg_nodes do
            local sr, sc, er, ec = arg_nodes[i]:range()
            table.insert(result, {
                lnum = sr,
                col = sc,
                end_lnum = er,
                end_col = ec,
                severity = vim.diagnostic.severity.WARN,
                message = string.format("extra argument: %s", provided[i] or "?"),
                source = "jdtls-arg",
            })
        end

        ::continue::
    end
    return result
end

-- Entry point: call from publishDiagnostics handler.
-- lsp_diagnostics is the raw LSP diagnostic list (range uses {start={line,character}}).
function M.apply(bufnr, lsp_diagnostics)
    if not vim.api.nvim_buf_is_valid(bufnr) or not vim.api.nvim_buf_is_loaded(bufnr) then
        return
    end
    if vim.bo[bufnr].filetype ~= "java" then
        vim.diagnostic.set(NS, bufnr, {})
        return
    end

    local diags = build_arg_diags(bufnr, lsp_diagnostics)
    vim.diagnostic.set(NS, bufnr, diags)
end

return M
