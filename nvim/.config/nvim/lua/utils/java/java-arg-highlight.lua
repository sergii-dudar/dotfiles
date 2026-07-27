-- Java diagnostic highlighter: pinpoint argument, type, and unresolved-symbol errors.
-- Parses selected JDTLS diagnostics and overlays focused expected-vs-actual highlights.
--
-- • apply — parse diagnostics and render focused Java highlights for a buffer

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

local RESULT_TYPE_FROM_ARGUMENT_METHODS = {
    cast = true,
    concatMap = true,
    flatMap = true,
    flatMapMany = true,
    handle = true,
    map = true,
    ofType = true,
    switchMap = true,
    ["then"] = true,
    thenReturn = true,
    transform = true,
    transformDeferred = true,
}

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

-- Strip generics and package prefix to get a simple class name.
-- e.g. "java.util.List<String>" -> "List"
local function simple_name(t)
    local base = t:match("^([%w_%.%[%]]+)") or t
    return base:match("[^%.]+$") or base
end

-- Count uppercase-starting segments (class name depth) after stripping generics.
-- Distinguishes a top-level class from a nested class with the same inner name:
--   "Charge"                    -> 1
--   "ChargesInformation.Charge" -> 2  (different from top-level Charge)
--   "java.util.List"            -> 1
local function class_depth(t)
    local base = t:match("^([%w_%.%[%]]+)") or t
    local count = 0
    local started = false
    for seg in base:gmatch("[^%.]+") do
        local c = seg:sub(1, 1)
        if not started and c >= "A" and c <= "Z" then
            started = true
        end
        if started then
            count = count + 1
        end
    end
    return count
end

local function types_differ(expected, provided)
    if expected == provided then
        return false
    end
    if simple_name(expected) ~= simple_name(provided) then
        return true
    end
    -- Same simple name but different nesting depth means different types
    -- (e.g. top-level "Charge" vs nested "ChargesInformation.Charge")
    return class_depth(expected) ~= class_depth(provided)
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

--- Return the expression or block that supplies a lambda's result.
local function lambda_result_node(node)
    if node:type() ~= "lambda_expression" then
        return node
    end

    local result
    for child in node:iter_children() do
        if child:named() then
            result = child
        end
    end
    return result or node
end

--- Narrow a multiline fluent-chain mismatch to its final result-shaping argument.
local function find_type_mismatch_target(bufnr, root, diag)
    local range = diag.range
    if not range or not range.start or not range["end"] then
        return nil
    end

    local start = range.start
    local finish = range["end"]
    local node = root:named_descendant_for_range(start.line, start.character, finish.line, finish.character)
    if not node then
        return nil
    end

    if node:type() ~= "method_invocation" or start.line == finish.line then
        return node
    end

    local objects = node:field("object")
    local names = node:field("name")
    local object = objects and objects[1]
    local name = names and names[1]
    if not object or object:type() ~= "method_invocation" or not name then
        return node
    end

    local method_name = vim.treesitter.get_node_text(name, bufnr)
    if not RESULT_TYPE_FROM_ARGUMENT_METHODS[method_name] then
        return node
    end

    for child in node:iter_children() do
        if child:type() == "argument_list" then
            local args = collect_arg_nodes(child)
            if #args == 1 then
                return lambda_result_node(args[1])
            end
            break
        end
    end

    return node
end

-- ---------------------------------------------------------------------------
-- Diagnostic message handlers
--
-- Each handler describes how to produce focused highlights from a specific
-- JDTLS diagnostic message. To support a new message format, add a new entry.
--
--   needle:   cheap literal substring used for fast pre-filtering
--   priority: higher-priority overlapping diagnostics suppress this handler
--   process:  function(root, diag, out, bufnr) — appends highlight entries to `out`
-- ---------------------------------------------------------------------------
local HANDLERS = {
    -- "The method foo(A, B) in the type Bar is not applicable for the arguments (A, C)"
    {
        needle = "is not applicable for the arguments",
        priority = 50,
        process = function(root, diag, out)
            local msg = diag.message
            local expected_str, provided_str = msg:match(
                "The method [%w_]+%((.-)%) in the type [%w_<>%[%]%.,%%$]+ is not applicable for the arguments %((.-)%)"
            )
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
        end,
    },

    -- "Type mismatch: cannot convert from Mono<Object> to Mono<? extends Response>"
    {
        needle = "Type mismatch: cannot convert from",
        priority = 10,
        --- Highlight the expression responsible for a JDTLS type mismatch.
        process = function(root, diag, out, bufnr)
            local provided, expected = diag.message:match("^Type mismatch: cannot convert from (.-) to (.+)$")
            if not provided or not expected then
                return
            end

            local target = find_type_mismatch_target(bufnr, root, diag)
            if not target then
                return
            end

            local sr, sc, er, ec = target:range()
            out[#out + 1] = {
                lnum = sr,
                col = sc,
                end_lnum = er,
                end_col = ec,
                severity = vim.diagnostic.severity.ERROR,
                message = string.format("expected %s, got %s", expected, provided),
                source = "jdtls-type",
            }
        end,
    },

    -- "Amount cannot be resolved to a type" — missing import, highlight the token
    {
        needle = "cannot be resolved to a type",
        priority = 100,
        process = function(_, diag, out)
            local type_name = diag.message:match("^([%w_%.]+) cannot be resolved to a type")
            if not type_name then
                return
            end

            local row = diag.range and diag.range.start and diag.range.start.line or 0
            local col = diag.range and diag.range.start and diag.range.start.character or 0
            local end_row = diag.range and diag.range["end"] and diag.range["end"].line or row
            local end_col = diag.range and diag.range["end"] and diag.range["end"].character or (col + #type_name)

            out[#out + 1] = {
                lnum = row,
                col = col,
                end_lnum = end_row,
                end_col = end_col,
                severity = vim.diagnostic.severity.WARN,
                message = string.format("need explicitly import %s", type_name),
                source = "jdtls-arg",
            }
        end,
    },

    -- "ACCEPTED cannot be resolved to a variable" — missing static import, highlight the token
    {
        needle = "cannot be resolved to a variable",
        priority = 100,
        --- Highlight an unresolved variable at the exact JDTLS diagnostic range.
        process = function(_, diag, out)
            local variable_name = diag.message:match("^([%w_%.]+) cannot be resolved to a variable")
            if not variable_name then
                return
            end

            local row = diag.range and diag.range.start and diag.range.start.line or 0
            local col = diag.range and diag.range.start and diag.range.start.character or 0
            local end_row = diag.range and diag.range["end"] and diag.range["end"].line or row
            local end_col = diag.range and diag.range["end"] and diag.range["end"].character or (col + #variable_name)

            out[#out + 1] = {
                lnum = row,
                col = col,
                end_lnum = end_row,
                end_col = end_col,
                severity = vim.diagnostic.severity.WARN,
                message = string.format("need explicitly import %s", variable_name),
                source = "jdtls-arg",
            }
        end,
    },
}

--- Return the highest handler priority matching a diagnostic message.
local function diag_handler_priority(msg)
    local priority
    for _, h in ipairs(HANDLERS) do
        local handler_priority = h.priority or 0
        if msg:find(h.needle, 1, true) and (not priority or handler_priority > priority) then
            priority = handler_priority
        end
    end
    return priority
end

--- Return whether one zero-indexed LSP position is strictly before another.
local function position_before(left, right)
    if left.line ~= right.line then
        return left.line < right.line
    end
    return (left.character or 0) < (right.character or 0)
end

--- Return whether two end-exclusive LSP diagnostic ranges overlap.
local function diagnostic_ranges_overlap(left, right)
    if not left or not left.start or not left["end"] or not right or not right.start or not right["end"] then
        return false
    end
    return position_before(left.start, right["end"]) and position_before(right.start, left["end"])
end

--- Return whether an overlapping diagnostic has a more targeted handler.
local function has_higher_priority_overlap(diag, lsp_diagnostics)
    local priority = diag.message and diag_handler_priority(diag.message)
    if not priority then
        return false
    end

    for _, other in ipairs(lsp_diagnostics) do
        if other ~= diag and other.message then
            local other_priority = diag_handler_priority(other.message)
            if other_priority and other_priority > priority and diagnostic_ranges_overlap(diag.range, other.range) then
                return true
            end
        end
    end
    return false
end

--- Return whether any custom highlighter handles a diagnostic message.
local function diag_matches_any_handler(msg)
    return diag_handler_priority(msg) ~= nil
end

--- Dispatch a diagnostic unless a more targeted overlapping diagnostic supersedes it.
local function process_diag(bufnr, root, diag, lsp_diagnostics, out)
    local msg = diag.message
    if not msg or has_higher_priority_overlap(diag, lsp_diagnostics) then
        return
    end
    for _, h in ipairs(HANDLERS) do
        if msg:find(h.needle, 1, true) then
            h.process(root, diag, out, bufnr)
        end
    end
end

-- Process LSP-format diagnostics for one buffer and return focused diagnostics
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
        process_diag(bufnr, root, diag, lsp_diagnostics, result)
    end
    return result
end

-- Entry point: call from publishDiagnostics handler with pre-filtered Java diagnostics.
--- Render focused highlights from selected Java diagnostics.
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
