-- JDTLS formatter adapter: build on the base profile while overriding selected
-- Eclipse options so that manual wrapping is respected and the result matches
-- IntelliJ's formatter. These rules are active:
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
--   * lambda expression bodies -- Eclipse has no wrap point after the arrow of
--     a lambda with an expression body, so `() ->\n new Foo()` is always joined
--     onto the arrow line, whatever the profile says. No formatter option can
--     keep it, so the break is restored on the client: the body is tracked with
--     extmarks across the formatter edits and, when it ended up behind its
--     arrow, moved back to its own line with a continuation indent relative to
--     the arrow line (like IntelliJ). The remaining lines of a multi-line body
--     move by the same indent. This costs no formatter request.
--   * parameter annotations -- an annotation kept on its own line in front of a
--     method parameter or record component (`@Valid\n Foo foo`) is joined by
--     Eclipse as well (its only alternative is to break after every annotation).
--     The break is restored with the same extmark mechanism.
--   * every other manual line break -- IntelliJ keeps a line break wherever the
--     author put one; Eclipse only keeps it at its own wrap points and joins the
--     rest (`cond ?\n a : b`, `Type name\n = value`, `builder().\n method()`,
--     a leading `,`, a cast on its own line, ...). The first token of every line
--     is tracked the same way and put back on its own line when the formatter
--     joined it; the call-indentation pass then indents it. Breaks IntelliJ
--     removes itself (`else`, `catch`, `finally`, an opening brace) stay joined.
--   * switch arrows and line length -- global overrides (`WRAP_PRESERVE_OVERRIDES`)
--     keep a manual break after `case X ->` and stop Eclipse from splitting
--     lines that exceed the profile's `lineSplit`; IntelliJ does not hard-wrap.
--   * call indentation -- IntelliJ and Eclipse indent wrapped call arguments and
--     chained calls differently. After the edits are applied the affected lines
--     are re-indented on the client from the Tree-sitter tree, see
--     `reindent_call_continuations`. This costs no formatter request either.
--
-- Performance: formatting makes the normal JDTLS request, then one additional
-- synchronous range-formatting request only for each parameter list that matches
-- the parameter rule -- typically none. Every request carries the global
-- overrides. The break tracking (one extmark per line) and the re-indentation run
-- on the client and add a few milliseconds per 500 lines. Add more rules that
-- need requests only when their formatting benefit justifies the extra latency.
--
-- Extension: further Eclipse formatter options can be overridden globally on
-- every request, or layered on a Tree-sitter range like the parameter rule. Keep
-- every request on the original document version, resolve overlapping rule ranges
-- explicitly, and apply the merged edits once.

local M = {}

local METHOD_PARAMETERS_ALIGNMENT = "org.eclipse.jdt.core.formatter.alignment_for_parameters_in_method_declaration"
local CONSTRUCTOR_PARAMETERS_ALIGNMENT =
    "org.eclipse.jdt.core.formatter.alignment_for_parameters_in_constructor_declaration"
local RECORD_COMPONENTS_ALIGNMENT = "org.eclipse.jdt.core.formatter.alignment_for_record_components"
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

-- Global overrides that keep IntelliJ-style wrapping, applied to every formatter
-- request like the binary ones. Compact split (16) for the switch arrow options
-- keeps a manual break after `case X ->` (the profile's `0` joins it). IntelliJ
-- never splits a long line on its own, so `lineSplit` is raised far beyond any
-- real line; without it Eclipse wraps everything longer than the profile limit.
local WRAP_PRESERVE_OVERRIDES = {
    ["org.eclipse.jdt.core.formatter.alignment_for_switch_case_with_arrow"] = "16",
    ["org.eclipse.jdt.core.formatter.alignment_for_expressions_in_switch_case_with_arrow"] = "16",
    ["org.eclipse.jdt.core.formatter.lineSplit"] = "9999",
    -- Comments are not formatted (`java.format.comments.enabled = false`), but the
    -- header region in front of the first type still is unless this is off too.
    ["org.eclipse.jdt.core.formatter.comment.format_header"] = "false",
    -- A comment in the first column (commented-out code) stays there.
    ["org.eclipse.jdt.core.formatter.never_indent_line_comments_on_first_column"] = "true",
    ["org.eclipse.jdt.core.formatter.never_indent_block_comments_on_first_column"] = "true",
    -- 3 = preserve: the content of a text block keeps its indentation, as in IntelliJ.
    ["org.eclipse.jdt.core.formatter.text_block_indentation"] = "3",
}

local FORMAT_TIMEOUT_MS = 3000

-- Indent levels of a wrapped line relative to the line it continues; mirrors
-- `org.eclipse.jdt.core.formatter.continuation_indentation` of the base profile.
local CONTINUATION_INDENT_LEVELS = 2

local LAMBDA_BODY_QUERY = [[
    (lambda_expression body: (_) @body)
]]

local PARAMETER_MODIFIERS_QUERY = [[
    [
        (formal_parameter (modifiers) @modifiers)
        (spread_parameter (modifiers) @modifiers)
        (enum_constant (modifiers) @modifiers)
    ]
]]

local ANNOTATION_TYPES = { annotation = true, marker_annotation = true }
local COMMENT_TYPES = { line_comment = true, block_comment = true }
local CHAIN_TYPES = { method_invocation = true, field_access = true }
local CLOSING_TYPES = { [")"] = true, ["]"] = true, ["}"] = true, [">"] = true }
local BODY_TYPES = {
    program = true,
    block = true,
    class_body = true,
    interface_body = true,
    enum_body_declarations = true,
    constructor_body = true,
    enum_body = true,
    switch_block = true,
}

local break_namespace

--- Return the extmark namespace used to track restored line breaks across formatter edits.
---@return integer
local function get_break_namespace()
    break_namespace = break_namespace or vim.api.nvim_create_namespace("jdtls-adaptive-formatter-breaks")
    return break_namespace
end

local PARAMETERS_QUERY = [[
    [
        (method_declaration parameters: (formal_parameters) @parameters)
        (constructor_declaration parameters: (formal_parameters) @parameters)
        (record_declaration parameters: (formal_parameters) @parameters)
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
                local parent_type = parent and parent:type()
                local option = parent_type == "constructor_declaration" and CONSTRUCTOR_PARAMETERS_ALIGNMENT
                    or parent_type == "record_declaration" and RECORD_COMPONENTS_ALIGNMENT
                    or METHOD_PARAMETERS_ALIGNMENT
                ranges[#ranges + 1] = { range = range, overrides = { [option] = DEFAULT_INDENT_ALIGNMENT } }
            end
        end
    end
    return ranges
end

--- Check whether a node contains a string literal spanning several lines (text block).
---@param node TSNode
---@return boolean
local function has_multiline_string(node)
    local start_row, _, end_row = node:range()
    if start_row == end_row then
        return false
    end
    if node:type() == "string_literal" then
        return true
    end
    for child in node:iter_children() do
        if has_multiline_string(child) then
            return true
        end
    end
    return false
end

--- Collect expression lambda bodies that intentionally start on the line after `->`.
---@param bufnr integer
---@param selection lsp.Range|nil
---@return { kind: string, start: lsp.Position, finish: lsp.Position, shift_rest: boolean }[]
local function get_wrapped_lambda_bodies(bufnr, selection)
    local query = get_query(LAMBDA_BODY_QUERY)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not query or not ok or not parser then
        return {}
    end

    local trees = parser:parse()
    if not trees or not trees[1] then
        return {}
    end

    local bodies = {}
    for _, body in query:iter_captures(trees[1]:root(), bufnr, 0, -1) do
        local arrow = body:prev_sibling()
        if body:type() ~= "block" and arrow and arrow:type() == "->" then
            local _, _, arrow_row, arrow_col = arrow:range()
            local start_row, start_col, end_row, end_col = body:range()
            local lambda_row, lambda_col = body:parent():range()
            local range = {
                start = { line = lambda_row, character = lambda_col },
                ["end"] = { line = end_row, character = end_col },
            }
            if start_row == arrow_row + 1 and is_in_selection(range, selection) then
                -- Only whitespace may follow the arrow: a trailing comment there keeps the break anyway.
                local arrow_line = vim.api.nvim_buf_get_lines(bufnr, arrow_row, arrow_row + 1, true)[1] or ""
                if arrow_line:sub(arrow_col + 1):match("^%s*$") then
                    bodies[#bodies + 1] = {
                        kind = "lambda",
                        start = { line = start_row, character = start_col },
                        finish = { line = end_row, character = end_col },
                        -- Re-indenting the lines of a text block would change its content.
                        shift_rest = end_row > start_row and not has_multiline_string(body),
                    }
                end
            end
        end
    end
    return bodies
end

--- Collect annotations of parameters and enum constants that are intentionally followed by a line break.
---@param bufnr integer
---@param selection lsp.Range|nil
---@return { kind: string, start: lsp.Position, anchor: lsp.Position }[]
local function get_parameter_annotation_breaks(bufnr, selection)
    local query = get_query(PARAMETER_MODIFIERS_QUERY)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not query or not ok or not parser then
        return {}
    end

    local trees = parser:parse()
    if not trees or not trees[1] then
        return {}
    end

    local breaks = {}
    for _, modifiers in query:iter_captures(trees[1]:root(), bufnr, 0, -1) do
        local parameter = modifiers:parent()
        local start_row, start_col, end_row, end_col = parameter:range()
        local range = {
            start = { line = start_row, character = start_col },
            ["end"] = { line = end_row, character = end_col },
        }
        if end_row > start_row and is_in_selection(range, selection) then
            -- The annotations and modifiers of the parameter, followed by its type.
            local elements = {}
            for child in modifiers:iter_children() do
                elements[#elements + 1] = child
            end
            elements[#elements + 1] = modifiers:next_sibling()
            for index = 2, #elements do
                local previous, current = elements[index - 1], elements[index]
                local _, _, previous_row = previous:range()
                local current_row, current_col = current:range()
                if
                    ANNOTATION_TYPES[previous:type()]
                    and not COMMENT_TYPES[current:type()]
                    and current_row == previous_row + 1
                then
                    breaks[#breaks + 1] = {
                        kind = "annotation",
                        start = { line = current_row, character = current_col },
                        anchor = { line = start_row, character = start_col },
                    }
                end
            end
        end
    end
    return breaks
end

--- Check whether a position lies inside a multi-line string (text block) that started earlier.
---@param node TSNode
---@param row integer
---@param col integer
---@return boolean
local function starts_inside_string(node, row, col)
    -- A token of a string sits at most two levels below its `string_literal`.
    for _ = 1, 3 do
        if not node then
            return false
        end
        if node:type() == "string_literal" then
            local string_row, string_col = node:start()
            if string_row ~= row or string_col ~= col then
                return true
            end
        end
        node = node:parent()
    end
    return false
end

--- Check whether IntelliJ itself moves a line-starting token up to the previous line.
---@param leaf TSNode
---@return boolean
local function is_joined_by_intellij(leaf)
    local leaf_type = leaf:type()
    local parent = leaf:parent()
    if leaf_type == "else" or leaf_type == "catch" or leaf_type == "finally" then
        return true
    end
    if leaf_type == "{" then
        -- Only the brace of a lambda block keeps a line of its own.
        local owner = parent and parent:parent()
        return not (parent and parent:type() == "block" and owner and owner:type() == "lambda_expression")
    end
    return false
end

--- Collect the first token of every line, so any line break the formatter removes can be restored.
---@param bufnr integer
---@param selection lsp.Range|nil
---@param known { start: lsp.Position }[] breaks that are already tracked by a more specific rule
---@return { kind: string, start: lsp.Position }[]
local function get_line_start_breaks(bufnr, selection, known)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return {}
    end
    local trees = parser:parse()
    local root = trees and trees[1] and trees[1]:root()
    if not root or root:has_error() then
        return {}
    end

    local tracked = {}
    for _, item in ipairs(known) do
        tracked[item.start.line] = true
    end
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, true)
    local first_row = math.max(selection and selection.start.line + 1 or 1, 1)
    local last_row = selection and math.min(selection["end"].line, #lines - 1) or #lines - 1
    local breaks = {}
    for row = first_row, last_row do
        local line = lines[row + 1]
        local col = #line:match("^%s*")
        if col < #line and not tracked[row] then
            local leaf = root:descendant_for_range(row, col, row, col + 1)
            local leaf_row, leaf_col
            if leaf then
                leaf_row, leaf_col = leaf:start()
            end
            if
                leaf
                and leaf_row == row
                and leaf_col == col
                and not COMMENT_TYPES[leaf:type()]
                and not starts_inside_string(leaf, row, col)
                and not is_joined_by_intellij(leaf)
            then
                breaks[#breaks + 1] = { kind = "line", start = { line = row, character = col } }
            end
        end
    end
    return breaks
end

--- Track the collected line breaks with extmarks so they survive the formatter edits.
---@param bufnr integer
---@param breaks { kind: string, start: lsp.Position, finish: lsp.Position|nil, shift_rest: boolean|nil }[]
---@return { kind: string, start_mark: integer, end_mark: integer|nil, shift_rest: boolean|nil }[]
local function track_breaks(bufnr, breaks)
    local namespace = get_break_namespace()
    -- Restoring walks the buffer top-down: a later break may sit on a line an earlier one created.
    table.sort(breaks, function(left, right)
        return compare_positions(left.start, right.start) < 0
    end)
    local tracked = {}
    for _, item in ipairs(breaks) do
        tracked.has_line_starts = tracked.has_line_starts or item.kind == "line"
        tracked[#tracked + 1] = {
            kind = item.kind,
            start_mark = vim.api.nvim_buf_set_extmark(bufnr, namespace, item.start.line, item.start.character, {
                right_gravity = true,
            }),
            end_mark = item.finish
                and vim.api.nvim_buf_set_extmark(bufnr, namespace, item.finish.line, item.finish.character, {
                    right_gravity = false,
                }),
            anchor_mark = item.anchor
                and vim.api.nvim_buf_set_extmark(bufnr, namespace, item.anchor.line, item.anchor.character, {
                    right_gravity = true,
                }),
            shift_rest = item.shift_rest,
        }
    end
    return tracked
end

--- Move tracked lambda bodies, parameter types and other line starts that the formatter joined back to their own line.
---@param bufnr integer
---@param tracked { kind: string, start_mark: integer, end_mark: integer|nil, shift_rest: boolean|nil }[]
---@return table<integer, boolean> restored rows whose indent is provisional and left to the re-indentation
local function restore_breaks(bufnr, tracked)
    local restored = {}
    local indent_unit = vim.bo[bufnr].expandtab
            and string.rep(" ", vim.lsp.util.get_effective_tabstop(bufnr) * CONTINUATION_INDENT_LEVELS)
        or string.rep("\t", CONTINUATION_INDENT_LEVELS)
    local namespace = get_break_namespace()

    -- Nearly every tracked line start is still first on its line. Drop those with
    -- one read of the buffer and its marks; a restored break never joins a line again.
    local pending = tracked
    if tracked.has_line_starts then
        local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, true)
        local positions = {}
        for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(bufnr, namespace, 0, -1, {})) do
            positions[mark[1]] = mark
        end
        pending = {}
        for _, item in ipairs(tracked) do
            local mark = positions[item.start_mark]
            if item.kind ~= "line" or (mark and (lines[mark[2] + 1] or ""):sub(1, mark[3]):match("%S")) then
                pending[#pending + 1] = item
            end
        end
    end

    for _, item in ipairs(pending) do
        local start = vim.api.nvim_buf_get_extmark_by_id(bufnr, namespace, item.start_mark, {})
        local finish = item.end_mark and vim.api.nvim_buf_get_extmark_by_id(bufnr, namespace, item.end_mark, {}) or {}
        local row, col = start[1], start[2]
        if row and col then
            local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, true)[1] or ""
            local before = line:sub(1, col)
            local arrow_end = item.kind == "lambda" and before:match("^.*%->()%s*$")
            if item.kind == "annotation" and before:match("%S") then
                -- The annotations and the type of a parameter share its column, also
                -- when the parameter itself starts behind `(` (on-column alignment).
                local anchor = item.anchor_mark
                        and vim.api.nvim_buf_get_extmark_by_id(bufnr, namespace, item.anchor_mark, {})
                    or {}
                local indent = anchor[2] and string.rep(" ", anchor[2]) or before:match("^%s*")
                local text_end = #before:gsub("%s+$", "")
                vim.api.nvim_buf_set_text(bufnr, row, text_end, row, col, { "", indent })
            elseif item.kind == "line" and before:match("%S") then
                -- Later breaks only add rows below this one, so the row number stays valid.
                local text_end = #before:gsub("%s+$", "")
                vim.api.nvim_buf_set_text(bufnr, row, text_end, row, col, { "", before:match("^%s*") .. indent_unit })
                restored[row + 1] = true
            elseif arrow_end then
                local indent = before:match("^%s*") .. indent_unit
                vim.api.nvim_buf_set_text(bufnr, row, arrow_end - 1, row, col, { "", indent })
                if item.shift_rest and finish[1] and finish[1] > row then
                    -- The body moved one row down; its remaining lines follow the new indent.
                    -- Insert per line: replacing whole lines would drop the extmarks of nested lambdas.
                    local rest = vim.api.nvim_buf_get_lines(bufnr, row + 2, finish[1] + 2, true)
                    for index, rest_line in ipairs(rest) do
                        if rest_line:match("%S") then
                            vim.api.nvim_buf_set_text(bufnr, row + 1 + index, 0, row + 1 + index, 0, { indent_unit })
                        end
                    end
                end
            end
        end
    end
    return restored
end

--- Return the `object` (qualifier) of a method invocation or field access.
---@param node TSNode
---@return TSNode|nil
local function chain_object(node)
    return node:field("object")[1]
end

--- Return the name behind the `.` of a method invocation or field access.
---@param node TSNode
---@return TSNode|nil
local function chain_name(node)
    return node:field("name")[1] or node:field("field")[1]
end

--- Return the `.` token of a method invocation or field access.
---@param node TSNode
---@return TSNode|nil
local function chain_dot(node)
    for child in node:iter_children() do
        if child:type() == "." then
            return child
        end
    end
    return nil
end

--- Return the outermost expression of the call chain a node belongs to (`a.b().c()` for `a.b()`).
---@param node TSNode
---@return TSNode
local function chain_top(node)
    while true do
        local parent = node:parent()
        if not parent or not CHAIN_TYPES[parent:type()] then
            return node
        end
        local object = chain_object(parent)
        if not object or not object:equal(node) then
            return node
        end
        node = parent
    end
end

--- Check whether a node is an object creation with a class body (`new Foo() { ... }`).
---@param node TSNode
---@return boolean
local function is_anonymous_class(node)
    if node:type() ~= "object_creation_expression" then
        return false
    end
    for child in node:iter_children() do
        if child:type() == "class_body" then
            return true
        end
    end
    return false
end

--- Re-indent wrapped call arguments and chained calls the way IntelliJ does.
---
--- Eclipse indents a wrapped line relative to the line the wrapped construct
--- starts on. IntelliJ indents it relative to the nearest enclosing expression
--- that starts a line (the anchor) and adds one continuation indent for every
--- enclosing expression in between that does not start a line itself but sits
---   * in an argument list that is wrapped (an argument starts a line, or the
---     closing parentheses of both the list and the expression do), or
---   * behind a `.` in a call chain that is wrapped (a `.` starts a line).
--- So `obj.foo(a,\n b)\n .bar()` puts `b` two continuation indents in, and a
--- chained call behind a multi-line argument stays one indent in.
---
--- Every line is recomputed top-down from the final indent of its anchor line:
--- arguments, closing parentheses, chained calls, `?`/`:` of a ternary, lambda
--- bodies and the content of lambda blocks and anonymous classes get their
--- IntelliJ indent, every other line keeps the offset Eclipse gave it.
--- A restored line break has no Eclipse offset: it gets one continuation indent.
--- Lines inside text blocks are left alone. Only space-indented buffers without
--- syntax errors are handled.
---@param bufnr integer
---@param first_row integer first row to re-indent (0-based)
---@param last_row integer|nil last row to re-indent, nil for the end of the buffer
---@param restored table<integer, boolean> rows of restored line breaks: Eclipse never indented them
local function reindent_call_continuations(bufnr, first_row, last_row, restored)
    if not vim.bo[bufnr].expandtab then
        return
    end
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return
    end
    local trees = parser:parse()
    local root = trees and trees[1] and trees[1]:root()
    if not root or root:has_error() then
        return
    end

    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, true)
    local tabstop = vim.lsp.util.get_effective_tabstop(bufnr)
    local unit = tabstop * CONTINUATION_INDENT_LEVELS
    local final_indents = {}

    --- Return the indent Eclipse gave a row.
    local function eclipse_indent(row)
        return #lines[row + 1]:match("^%s*")
    end

    --- Return the indent a row ends up with.
    local function final_indent(row)
        return final_indents[row] or eclipse_indent(row)
    end

    --- Check whether a node is the first thing on its line.
    local function starts_line(node)
        local row, col = node:start()
        return col == eclipse_indent(row)
    end

    --- Tell how an argument list is wrapped: "argument" when an argument starts a
    --- line, "paren" when only the closing parenthesis does, nil when neither.
    local function list_wrapping(list)
        local wrapping = nil
        for child in list:iter_children() do
            local child_type = child:type()
            if child_type == ")" then
                wrapping = wrapping or (starts_line(child) and "paren" or nil)
            elseif child:named() and not COMMENT_TYPES[child_type] and starts_line(child) then
                return "argument"
            end
        end
        return wrapping
    end

    --- Check whether a node ends with a closing token on a line of its own whose
    --- opening token stands on the node's first line (`bar(\n a\n )`, `x -> {\n ...\n }`).
    local function ends_with_line_start(node)
        local last = node
        while last:child_count() > 0 do
            last = last:child(last:child_count() - 1)
        end
        local closed = last:parent()
        if not CLOSING_TYPES[last:type()] or not closed or not starts_line(last) then
            return false
        end
        return (closed:start()) == (node:start())
    end

    --- Check whether an argument that does not start a line adds an indent to its content:
    --- its list has an argument on a new line, or both closing parentheses stand on their own lines.
    local function argument_indents(node, list)
        local wrapping = list_wrapping(list)
        return wrapping == "argument" or (wrapping == "paren" and ends_with_line_start(node))
    end

    --- Return the row a chain link starts a line on: its `.`, or its name behind a trailing `.`.
    local function link_line(node)
        local dot = chain_dot(node)
        if not dot then
            return nil
        end
        if starts_line(dot) then
            return (dot:start())
        end
        local name = chain_name(node)
        if name and starts_line(name) then
            return (name:start())
        end
        return nil
    end

    --- Check whether any link of a call chain starts a line.
    local function chain_wrapped(top)
        local node = top
        while node and CHAIN_TYPES[node:type()] do
            if link_line(node) then
                return true
            end
            node = chain_object(node)
        end
        return false
    end

    local resolve

    --- Resolve the anchor of the content of an argument list: the line of the
    --- call's own `.` when that starts a line, otherwise whatever the chain hangs on.
    local function resolve_from_list(list, extra)
        local call = list:parent()
        if not call then
            return nil
        end
        if call:type() == "method_invocation" and chain_object(call) then
            local line = link_line(call)
            if line then
                return line, extra
            end
            local top = chain_top(call)
            if chain_wrapped(top) then
                extra = extra + unit
            end
            return resolve(top, extra)
        end
        return resolve(chain_top(call), extra)
    end

    --- Walk up from an enclosing node to the anchor line, summing the IntelliJ-only indents on the way.
    ---@return integer|nil anchor_row
    ---@return integer|nil extra
    resolve = function(node, extra)
        while node do
            if starts_line(node) then
                return (node:start()), extra
            end
            if node:type() == "argument_list" then
                return resolve_from_list(node, extra)
            end
            local parent = node:parent()
            if not parent then
                return nil
            end
            if parent:type() == "argument_list" then
                -- The arguments of `new Foo(...)` never add this indent; method, `super(...)` and `this(...)` calls do.
                local owner = parent:parent()
                if
                    node:named()
                    and owner
                    and owner:type() ~= "object_creation_expression"
                    and argument_indents(node, parent)
                then
                    -- IntelliJ gives an anonymous class a plain indent here, not a continuation one.
                    extra = extra + (is_anonymous_class(node) and tabstop or unit)
                end
                return resolve_from_list(parent, extra)
            end
            node = parent
        end
        return nil
    end

    --- Return the column parameters are aligned on when the first one stands behind `(` on an earlier row.
    local function aligned_column(parameters, row)
        local first = parameters:named_child(0)
        if not first or starts_line(first) then
            return nil
        end
        local first_row, first_col = first:start()
        if first_row >= row then
            return nil
        end
        return final_indent(first_row) + first_col - eclipse_indent(first_row)
    end

    --- Compute the IntelliJ indent of the line starting at (row, col), or nil to keep it.
    local function target_indent(row, col)
        local leaf = root:descendant_for_range(row, col, row, col + 1)
        if not leaf then
            return nil
        end
        -- A line that starts inside a text block is content, not indentation.
        if starts_inside_string(leaf, row, col) then
            return nil
        end
        local leaf_row, leaf_col = leaf:start()
        if leaf_row ~= row or leaf_col ~= col then
            -- Inside a multi-line token (block comment): follow its first line.
            if leaf_row < row and starts_line(leaf) then
                return final_indent(leaf_row) + col - eclipse_indent(leaf_row)
            end
            return nil
        end

        -- The largest node that starts with this line's first token.
        local own = leaf
        while true do
            local parent = own:parent()
            if not parent then
                return nil
            end
            local parent_row, parent_col = parent:start()
            if parent_row ~= row or parent_col ~= col then
                break
            end
            own = parent
        end
        local parent = own:parent()
        local parent_type, leaf_type = parent:type(), leaf:type()

        local anchor, extra, own_indent
        local chain_link = CHAIN_TYPES[parent_type] and chain_object(parent) ~= nil
        if chain_link and (leaf_type == "." or leaf:equal(chain_name(parent) or parent)) then
            anchor, extra = resolve(chain_top(parent), 0)
            own_indent = unit
        elseif leaf_type == ";" then
            -- A stray `;` stands like a statement of its own; the `;` of a statement goes under its first line.
            if BODY_TYPES[parent_type] then
                anchor, extra = resolve(parent, 0)
                own_indent = parent_type == "program" and 0 or tabstop
            else
                anchor, extra, own_indent = (parent:start()), 0, 0
            end
        elseif parent_type == "type_arguments" and own:named() then
            -- `extends Base<` + type arguments on their own lines: IntelliJ keeps them on the column of `extends`.
            local header = parent:parent()
            while header and header:type() ~= "superclass" and header:type() ~= "super_interfaces" do
                header = header:parent()
            end
            if not header or not starts_line(header) then
                return nil
            end
            anchor, extra, own_indent = (header:start()), 0, 0
        elseif parent_type == "ternary_expression" and (leaf_type == "?" or leaf_type == ":") then
            anchor, extra = resolve(parent, 0)
            own_indent = unit
        elseif parent_type == "lambda_expression" and leaf_type ~= "->" then
            -- An expression body on its own line, or a comment in front of it. The
            -- brace of a block body on its own line stays on the column of its anchor.
            anchor, extra = resolve(parent, 0)
            own_indent = own:type() == "block" and 0 or unit
        elseif
            (parent_type == "block" and parent:parent() and parent:parent():type() == "lambda_expression")
            or (parent_type == "class_body" and parent:parent() and is_anonymous_class(parent:parent()))
        then
            -- Statements of a lambda block and members of an anonymous class.
            anchor, extra = resolve(parent, 0)
            own_indent = leaf_type == "}" and 0 or tabstop
        elseif parent_type == "argument_list" and leaf_type == ")" then
            anchor, extra = resolve_from_list(parent, 0)
            own_indent = 0
        elseif parent_type == "argument_list" and own:named() and not COMMENT_TYPES[own:type()] then
            anchor, extra = resolve_from_list(parent, 0)
            own_indent = unit
        elseif
            parent_type == "method_declaration"
            and (own:equal(parent:field("name")[1] or parent) or own:equal(parent:field("type")[1] or parent))
        then
            -- A method name or return type on its own line stays on the column of the declaration.
            anchor, extra, own_indent = (parent:start()), 0, 0
        elseif leaf_type == "while" and parent_type == "do_statement" then
            anchor, extra, own_indent = (parent:start()), 0, 0
        elseif parent_type == "formal_parameters" and own:named() and aligned_column(parent, row) then
            -- Parameters behind `(` are aligned on its column; a restored break above moves that column.
            return aligned_column(parent, row)
        elseif restored[row] then
            anchor, extra = resolve(parent, 0)
            own_indent = CLOSING_TYPES[leaf_type] and 0 or unit
        else
            anchor, extra = resolve(parent, 0)
            -- Eclipse laid the line out before the breaks were restored: a restored
            -- row stands for the line it was joined to.
            local parent_row = parent:start()
            while restored[parent_row] do
                parent_row = parent_row - 1
            end
            -- Statements and members are indented from their declaration, wherever its `{` stands.
            if anchor and parent_row ~= anchor and parent_row < row and not BODY_TYPES[parent_type] then
                -- Eclipse placed the line relative to the line its construct starts on: follow that line.
                anchor, extra = parent_row, 0
            end
            own_indent = anchor and col - eclipse_indent(anchor)
        end
        if not anchor or anchor >= row then
            return nil
        end
        return final_indent(anchor) + own_indent + extra
    end

    last_row = math.min(last_row or #lines - 1, #lines - 1)
    local changes = {}
    for row = first_row, last_row do
        local line = lines[row + 1]
        local col = #line:match("^%s*")
        -- Column 0 holds top-level declarations and first-column comments: never indented.
        if col > 0 and col < #line and not line:sub(1, col):find("\t", 1, true) then
            local target = target_indent(row, col)
            if target and target >= 0 and target ~= col then
                final_indents[row] = target
                changes[#changes + 1] = { row = row, col = col, target = target }
            end
        end
    end
    for _, change in ipairs(changes) do
        vim.api.nvim_buf_set_text(bufnr, change.row, 0, change.row, change.col, { string.rep(" ", change.target) })
    end
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
    }, BINARY_PRESERVE_OVERRIDES, WRAP_PRESERVE_OVERRIDES, overrides or {})
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

--- Format Java with JDTLS, then restore the wrapping and indentation IntelliJ would keep.
---@param bufnr integer
function M.format(bufnr)
    bufnr = resolve_bufnr(bufnr)
    local client = get_client(bufnr)
    if not client then
        return
    end

    local selection = get_visual_range(bufnr)
    local adaptive_ranges = get_adaptive_ranges(bufnr, selection)
    local breaks = get_wrapped_lambda_bodies(bufnr, selection)
    vim.list_extend(breaks, get_parameter_annotation_breaks(bufnr, selection))
    vim.list_extend(breaks, get_line_start_breaks(bufnr, selection, breaks))
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
    local restored = {}
    local first_row = selection and selection.start.line or 0
    local last_row = selection and selection["end"].line or nil
    if #edits > 0 and (selection or #breaks > 0) then
        -- The formatter joins lines IntelliJ keeps apart (a lambda body behind its
        -- arrow, a parameter behind its annotations, ...); the extmarks follow the
        -- line starts through the edits so the breaks can be restored. The edits and
        -- the restored breaks move the rows of a selection, so it is tracked as well.
        local namespace = get_break_namespace()
        local tracked = track_breaks(bufnr, breaks)
        local selection_marks = selection
            and {
                vim.api.nvim_buf_set_extmark(bufnr, namespace, selection.start.line, selection.start.character, {
                    right_gravity = false,
                }),
                vim.api.nvim_buf_set_extmark(bufnr, namespace, selection["end"].line, selection["end"].character, {
                    right_gravity = false,
                }),
            }
        vim.lsp.util.apply_text_edits(edits, bufnr, client.offset_encoding)
        restored = restore_breaks(bufnr, tracked)
        if selection_marks then
            first_row = vim.api.nvim_buf_get_extmark_by_id(bufnr, namespace, selection_marks[1], {})[1] or first_row
            last_row = vim.api.nvim_buf_get_extmark_by_id(bufnr, namespace, selection_marks[2], {})[1] or last_row
        end
        vim.api.nvim_buf_clear_namespace(bufnr, namespace, 0, -1)
    elseif #edits > 0 then
        vim.lsp.util.apply_text_edits(edits, bufnr, client.offset_encoding)
    end
    -- Runs without formatter edits as well: a buffer that is already in Eclipse's
    -- shape still gets IntelliJ's call indentation.
    reindent_call_continuations(bufnr, first_row, last_row, restored)
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
