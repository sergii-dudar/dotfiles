-- Toggle the Java expression under the cursor between a lambda and a method
-- reference, e.g. `t -> t.name()` <-> `Person::name`, `x -> this.getX()` <->
-- `this::getX`, `x -> Foo.bar(x)` <-> `Foo::bar`.
--
-- Delegates to jdtls' own "Convert to lambda expression" / "Convert to method
-- reference" quick assists (rather than a from-scratch treesitter rewrite) so the
-- direction that needs full type resolution -- unbound instance (`Type::method`),
-- `this::method`, static (`Type::method`), and constructor (`Type::new`) references
-- -- is produced correctly by the compiler-backed server.
--
-- jdtls sometimes wraps the result of "Convert to method reference" in a redundant
-- functional-interface cast when the receiver carries generic type arguments, e.g.
--   (Function<? super Supplier<String>, ? extends String>) Supplier<String>::get
-- instead of just `Supplier<String>::get`
-- (see https://github.com/eclipse-jdt/eclipse.jdt.ui/issues/1520, fixed upstream but
-- still shipping in many jdtls versions). After converting to a method reference,
-- this strips that cast back out.
--
-- Public API:
--   toggle()  convert the lambda/method-reference under the cursor to the other form

local lsp_lang_common = require("utils.lang.lsp-common")

local M = {}

local TO_METHOD_REFERENCE = "Convert to method reference"
local TO_LAMBDA = "Convert to lambda expression"

-- ============================================================================
-- Redundant cast removal
-- ============================================================================

---@param bufnr integer
---@param row integer 0-indexed
---@return integer col 0-indexed column of the first non-blank character (0 if the line is blank)
local function first_nonblank_col(bufnr, row)
    local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1] or ""
    local col = line:find("%S")
    return col and (col - 1) or 0
end

--- Depth-first search for a `cast_expression` whose casted value is directly a
--- `method_reference` (the shape jdtls' buggy quick assist produces).
---@param node TSNode
---@return TSNode|nil cast_node
---@return TSNode|nil method_ref_node
local function find_redundant_cast(node)
    if node:type() == "cast_expression" then
        local value = node:field("value")[1]
        if value and value:type() == "method_reference" then
            return node, value
        end
    end
    for child in node:iter_children() do
        local cast_node, method_ref_node = find_redundant_cast(child)
        if cast_node then
            return cast_node, method_ref_node
        end
    end
    return nil
end

--- Remove a redundant `(FunctionalIfaceType<...>) receiver::method` cast within
--- `[start_row, end_row]` (0-indexed, inclusive), collapsing it to just
--- `receiver::method`. Scoped to the statement enclosing `start_row` so it never
--- touches unrelated code elsewhere in the file.
---@param bufnr integer
---@param start_row integer 0-indexed
---@param end_row integer 0-indexed
---@return boolean removed
local function strip_redundant_cast(bufnr, start_row, end_row)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return false
    end

    local line_count = vim.api.nvim_buf_line_count(bufnr)
    start_row = math.max(0, math.min(start_row, line_count - 1))
    end_row = math.max(start_row, math.min(end_row, line_count - 1))

    local root = parser:parse()[1]:root()

    for row = start_row, end_row do
        local col = first_nonblank_col(bufnr, row)
        local anchor = root:named_descendant_for_range(row, col, row, col)
        if anchor then
            -- Climb to the enclosing statement so the search stays scoped to what
            -- the conversion just touched, not the whole file.
            local scope = anchor
            while
                scope:parent()
                and not scope:type():match("statement$")
                and scope:type() ~= "local_variable_declaration"
            do
                scope = scope:parent()
            end

            local cast_node, method_ref_node = find_redundant_cast(scope)
            if cast_node and method_ref_node then
                local text = vim.treesitter.get_node_text(method_ref_node, bufnr)
                local srow, scol, erow, ecol = cast_node:range()
                vim.api.nvim_buf_set_text(bufnr, srow, scol, erow, ecol, vim.split(text, "\n", { plain = true }))
                return true
            end
        end
    end
    return false
end

--- Compute the 0-indexed `[min_start_line, max_end_line]` touched by a
--- `WorkspaceEdit`, across both the `changes` map and `documentChanges` shapes.
---@param edit lsp.WorkspaceEdit|nil
---@return integer|nil start_row
---@return integer|nil end_row
function M.edit_row_range(edit)
    if not edit then
        return nil, nil
    end

    local min_row, max_row

    local function consume(edits)
        for _, text_edit in ipairs(edits or {}) do
            local range = text_edit.range
            if range then
                min_row = min_row and math.min(min_row, range.start.line) or range.start.line
                max_row = max_row and math.max(max_row, range["end"].line) or range["end"].line
            end
        end
    end

    for _, edits in pairs(edit.changes or {}) do
        consume(edits)
    end
    for _, change in ipairs(edit.documentChanges or {}) do
        consume(change.edits)
    end

    return min_row, max_row
end

-- ============================================================================
-- Public toggle
-- ============================================================================

--- Convert the lambda/method-reference expression under the cursor to the other
--- form, by requesting jdtls' code actions and applying the first exact match for
--- either direction's title.
function M.toggle()
    local bufnr = vim.api.nvim_get_current_buf()
    local clients = vim.lsp.get_clients({ bufnr = bufnr })
    if #clients == 0 then
        vim.notify("No LSP clients attached", vim.log.levels.INFO)
        return
    end

    local offset_encoding = clients[1].offset_encoding or "utf-16"
    local params = vim.lsp.util.make_range_params(0, offset_encoding)
    local cursor_lnum = vim.api.nvim_win_get_cursor(0)[1] - 1
    ---@diagnostic disable-next-line: inject-field
    params.context = {
        diagnostics = vim.lsp.diagnostic.from(vim.diagnostic.get(bufnr, { lnum = cursor_lnum })),
        triggerKind = vim.lsp.protocol.CodeActionTriggerKind.Invoked,
    }

    vim.lsp.buf_request_all(bufnr, "textDocument/codeAction", params, function(results)
        local match, match_client_id, is_to_method_ref
        for client_id, result in pairs(results) do
            for _, action in ipairs(result.result or {}) do
                if action.title == TO_METHOD_REFERENCE then
                    match, match_client_id, is_to_method_ref = action, client_id, true
                elseif action.title == TO_LAMBDA and not match then
                    match, match_client_id, is_to_method_ref = action, client_id, false
                end
            end
        end

        if not match then
            vim.schedule(function()
                vim.notify("No lambda <-> method reference conversion available here", vim.log.levels.INFO)
            end)
            return
        end

        vim.schedule(function()
            local client = vim.lsp.get_client_by_id(match_client_id)
            if not client then
                return
            end
            lsp_lang_common.apply_lsp_action(match, client, function(applied_action)
                if not is_to_method_ref then
                    return
                end
                local start_row, end_row = M.edit_row_range(applied_action.edit)
                if not start_row then
                    return
                end
                strip_redundant_cast(bufnr, start_row, end_row)
            end)
        end)
    end)
end

return M
