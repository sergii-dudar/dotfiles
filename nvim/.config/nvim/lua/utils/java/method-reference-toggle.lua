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
-- Some jdtls versions can also abort the entire code-action request while building
-- an unrelated "add missing method" assist for a valid `this::method` reference.
-- If that happens, a deliberately narrow Tree-sitter fallback expands a uniquely
-- declared same-class `this::method` reference locally. All other conversions stay
-- compiler-backed.
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
-- JDTLS error fallback
-- ============================================================================

local TYPE_DECLARATIONS = {
    annotation_type_declaration = true,
    class_declaration = true,
    enum_declaration = true,
    interface_declaration = true,
    record_declaration = true,
}

local EXECUTABLE_DECLARATIONS = {
    compact_constructor_declaration = true,
    constructor_declaration = true,
    method_declaration = true,
}

--- Collect every identifier below a node so generated lambda parameters do not
--- shadow names already used in the enclosing executable.
---@param node TSNode
---@param bufnr integer
---@param names table<string, boolean>
local function collect_identifier_names(node, bufnr, names)
    if node:type() == "identifier" then
        names[vim.treesitter.get_node_text(node, bufnr)] = true
    end
    for child in node:iter_children() do
        collect_identifier_names(child, bufnr, names)
    end
end

--- Return a same-class method's parameter names only when `method_name` has one
--- unambiguous declaration in the type containing `method_reference`.
---@param method_reference TSNode
---@param method_name string
---@param bufnr integer
---@return string[]|nil parameter_names
local function same_class_method_parameter_names(method_reference, method_name, bufnr)
    local type_node = method_reference:parent()
    while type_node and not TYPE_DECLARATIONS[type_node:type()] do
        type_node = type_node:parent()
    end
    if not type_node then
        return nil
    end

    local body = type_node:field("body")[1]
    if not body then
        return nil
    end

    local match
    for child in body:iter_children() do
        if child:type() == "method_declaration" then
            local name_node = child:field("name")[1]
            if name_node and vim.treesitter.get_node_text(name_node, bufnr) == method_name then
                if match then
                    return nil
                end
                match = child
            end
        end
    end
    if not match then
        return nil
    end

    local parameters = match:field("parameters")[1]
    if not parameters then
        return nil
    end

    local names = {}
    for parameter in parameters:iter_children() do
        if parameter:type() == "formal_parameter" or parameter:type() == "spread_parameter" then
            local name_node = parameter:field("name")[1]
            if not name_node then
                return nil
            end
            table.insert(names, vim.treesitter.get_node_text(name_node, bufnr))
        end
    end
    return names
end

--- Expand a uniquely declared same-class `this::method` reference without JDTLS.
--- This is intentionally limited to the form that can be reconstructed without
--- type resolution and is used only after the server's code-action request fails.
---@param bufnr integer
---@param row integer 0-indexed cursor row from the failed request
---@param col integer 0-indexed cursor column from the failed request
---@return boolean converted
local function try_same_class_this_fallback(bufnr, row, col)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return false
    end

    local tree = parser:parse()[1]
    if not tree then
        return false
    end

    local method_reference = tree:root():named_descendant_for_range(row, col, row, col)
    while method_reference and method_reference:type() ~= "method_reference" do
        method_reference = method_reference:parent()
    end
    if not method_reference then
        return false
    end

    local parts = {}
    for child in method_reference:iter_children() do
        if child:named() then
            table.insert(parts, child)
        end
    end
    if #parts ~= 2 or parts[1]:type() ~= "this" or parts[2]:type() ~= "identifier" then
        return false
    end

    local method_name = vim.treesitter.get_node_text(parts[2], bufnr)
    local parameter_names = same_class_method_parameter_names(method_reference, method_name, bufnr)
    if not parameter_names then
        return false
    end

    local scope = method_reference
    while scope:parent() and not EXECUTABLE_DECLARATIONS[scope:type()] do
        scope = scope:parent()
    end

    local used_names = {}
    collect_identifier_names(scope, bufnr, used_names)

    local lambda_names = {}
    for index, parameter_name in ipairs(parameter_names) do
        local base = parameter_name ~= "" and parameter_name or ("arg" .. index)
        local candidate = base
        local suffix = 1
        while used_names[candidate] do
            candidate = base .. suffix
            suffix = suffix + 1
        end
        used_names[candidate] = true
        table.insert(lambda_names, candidate)
    end

    local lambda_parameters
    if #lambda_names == 1 then
        lambda_parameters = lambda_names[1]
    else
        lambda_parameters = "(" .. table.concat(lambda_names, ", ") .. ")"
    end
    local replacement = lambda_parameters
        .. " -> this."
        .. method_name
        .. "("
        .. table.concat(lambda_names, ", ")
        .. ")"

    local start_row, start_col, end_row, end_col = method_reference:range()
    vim.api.nvim_buf_set_text(bufnr, start_row, start_col, end_row, end_col, { replacement })
    return true
end

--- Build a concise user-facing message for a failed JDTLS code-action request.
---@param err lsp.ResponseError
---@return string
local function code_action_error_message(err)
    local data = type(err.data) == "string" and err.data or ""
    local invalid_identifier = data:match("Invalid identifier%s*:%s*([^\r\n]+)")
    if invalid_identifier then
        return "JDTLS code-action request failed: Invalid identifier: " .. invalid_identifier .. " (see :LspLog)"
    end
    return "JDTLS code-action request failed: " .. (err.message or "unknown error") .. " (see :LspLog)"
end

-- ============================================================================
-- Public toggle
-- ============================================================================

--- Convert the lambda/method-reference expression under the cursor to the other
--- form using jdtls, with a narrow same-class `this::method` fallback when the
--- server's entire code-action request fails.
function M.toggle()
    local bufnr = vim.api.nvim_get_current_buf()
    local client = vim.lsp.get_clients({ bufnr = bufnr, name = "jdtls" })[1]
    if not client then
        vim.notify("No jdtls client attached", vim.log.levels.INFO)
        return
    end

    local request_changedtick = vim.api.nvim_buf_get_changedtick(bufnr)
    local offset_encoding = client.offset_encoding or "utf-16"
    local params = vim.lsp.util.make_range_params(0, offset_encoding)
    local cursor = vim.api.nvim_win_get_cursor(0)
    local cursor_lnum = cursor[1] - 1
    ---@diagnostic disable-next-line: inject-field
    params.context = {
        diagnostics = vim.lsp.diagnostic.from(vim.diagnostic.get(bufnr, { lnum = cursor_lnum })),
        triggerKind = vim.lsp.protocol.CodeActionTriggerKind.Invoked,
    }

    client:request("textDocument/codeAction", params, function(err, actions)
        local match, is_to_method_ref
        for _, action in ipairs(actions or {}) do
            if action.title == TO_METHOD_REFERENCE then
                match, is_to_method_ref = action, true
            elseif action.title == TO_LAMBDA and not match then
                match, is_to_method_ref = action, false
            end
        end

        if not match then
            vim.schedule(function()
                if
                    err
                    and vim.api.nvim_buf_is_valid(bufnr)
                    and vim.api.nvim_buf_get_changedtick(bufnr) == request_changedtick
                then
                    local fallback_ok, converted = pcall(try_same_class_this_fallback, bufnr, cursor_lnum, cursor[2])
                    if fallback_ok and converted then
                        return
                    end
                end

                if err then
                    vim.notify(code_action_error_message(err), vim.log.levels.WARN)
                else
                    vim.notify("No lambda <-> method reference conversion available here", vim.log.levels.INFO)
                end
            end)
            return
        end

        vim.schedule(function()
            local active_client = vim.lsp.get_client_by_id(client.id)
            if not active_client then
                return
            end
            lsp_lang_common.apply_lsp_action(match, active_client, function(applied_action)
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
    end, bufnr)
end

return M
