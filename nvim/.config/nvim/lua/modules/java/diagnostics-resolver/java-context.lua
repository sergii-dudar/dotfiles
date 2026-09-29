--- Shared Java Tree-sitter context helpers for diagnostic resolvers.

local M = {}

local TYPE_DECLARATIONS = {
    class_declaration = true,
    interface_declaration = true,
}

--- Return the Java Tree-sitter root for a buffer.
---@param bufnr integer
---@return TSNode|nil
local function java_root(bufnr)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    return tree and tree:root() or nil
end

--- Find the method declaration owning a diagnostic position.
---@param bufnr integer
---@param diagnostic table
---@return TSNode|nil
function M.method_at_diagnostic(bufnr, diagnostic)
    local root = java_root(bufnr)
    if not root then
        return nil
    end

    local row = diagnostic.lnum or vim.api.nvim_win_get_cursor(0)[1] - 1
    local col = diagnostic.col or 0
    local node = root:named_descendant_for_range(row, col, row, col)
    while node and node:type() ~= "method_declaration" do
        node = node:parent()
    end
    return node
end

--- Find the class or interface containing a Java syntax node.
---@param node TSNode|nil
---@return TSNode|nil
function M.enclosing_type(node)
    node = node and node:parent() or nil
    while node and not TYPE_DECLARATIONS[node:type()] do
        node = node:parent()
    end
    return node
end

--- Return the buffer insertion row immediately after a syntax node.
---@param node TSNode
---@return integer zero-based row
local function row_after_node(node)
    local _, _, end_row, end_col = node:range()
    return end_col == 0 and end_row or end_row + 1
end

--- Insert generated member lines immediately after a mapper method.
--- A blank line is added on both sides when another member follows directly.
---@param bufnr integer
---@param method TSNode
---@param generated_lines string[] lines without surrounding blank separators
---@return integer insert_row zero-based row before imports are applied
function M.insert_after_method(bufnr, method, generated_lines)
    local insert_row = row_after_node(method)
    local lines = { "" }
    vim.list_extend(lines, generated_lines)

    local next_line = vim.api.nvim_buf_get_lines(bufnr, insert_row, insert_row + 1, false)[1]
    if next_line and next_line:match("%S") and not next_line:match("^%s*}") then
        lines[#lines + 1] = ""
    end

    vim.api.nvim_buf_set_lines(bufnr, insert_row, insert_row, false, lines)
    return insert_row
end

--- Return the indentation prefix from a buffer line.
---@param bufnr integer
---@param row integer zero-based row
---@return string
function M.line_indent(bufnr, row)
    local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1] or ""
    return line:match("^%s*") or ""
end

--- Return one indentation unit using the target buffer's options.
---@param bufnr integer
---@return string
function M.indent_unit(bufnr)
    if vim.api.nvim_get_option_value("expandtab", { buf = bufnr }) == false then
        return "\t"
    end

    local width = vim.api.nvim_get_option_value("shiftwidth", { buf = bufnr })
    if not width or width == 0 then
        width = vim.api.nvim_get_option_value("tabstop", { buf = bufnr })
    end
    return string.rep(" ", width and width > 0 and width or 4)
end

local COMMENT_NODES = {
    line_comment = true,
    block_comment = true,
}

--- Remove every whitespace character so signatures compare independent of layout.
---@param value string
---@return string
local function compact(value)
    return (value:gsub("%s+", ""))
end

--- Reduce a Java type to its erased simple name for overload comparison.
--- `java.util.List<api.Balance>` -> `List`, `Outer.Inner[]` -> `Inner[]`.
---@param type_name string
---@return string
local function erased_simple_type(type_name)
    local erased = compact(type_name):gsub("%b<>", "")
    return erased:match("([^%.]+)$") or erased
end

--- Collect the methods declared in a buffer from its syntax tree.
---@param bufnr integer
---@return { name: string, signature: string, parameter_types: string[] }[]|nil methods nil without a usable syntax tree
local function declared_methods(bufnr)
    local ok, methods = pcall(function()
        local root = java_root(bufnr)
        if not root then
            return nil
        end

        local query = vim.treesitter.query.parse("java", "(method_declaration) @method")
        local result = {}
        for _, node in query:iter_captures(root, bufnr, 0, -1) do
            local type_node = node:field("type")[1]
            local name_node = node:field("name")[1]
            local parameters_node = node:field("parameters")[1]
            if type_node and name_node and parameters_node then
                local parameters = {}
                local parameter_types = {}
                for parameter in parameters_node:iter_children() do
                    local kind = parameter:type()
                    if kind == "formal_parameter" or kind == "spread_parameter" then
                        local parameter_type = parameter:field("type")[1] or parameter:named_child(0)
                        local parameter_name = parameter:field("name")[1]
                        local type_text = parameter_type and vim.treesitter.get_node_text(parameter_type, bufnr) or ""
                        local name_text = parameter_name and vim.treesitter.get_node_text(parameter_name, bufnr) or ""
                        parameters[#parameters + 1] = type_text .. " " .. name_text
                        parameter_types[#parameter_types + 1] = erased_simple_type(type_text)
                    end
                end

                local name = vim.treesitter.get_node_text(name_node, bufnr)
                result[#result + 1] = {
                    name = name,
                    signature = vim.treesitter.get_node_text(type_node, bufnr) .. " " .. name .. "(" .. table.concat(
                        parameters,
                        ", "
                    ) .. ")",
                    parameter_types = parameter_types,
                }
            end
        end
        return result
    end)

    if not ok then
        return nil
    end
    return methods
end

--- Check whether a buffer position lies inside a Java comment.
---@param bufnr integer
---@param row integer zero-based row
---@param col integer zero-based column
---@param line string text of the row
---@return boolean
local function in_comment(bufnr, row, col, line)
    -- A commented-out line is recognised from its text alone, syntax tree or not.
    local prefix = line:sub(1, col)
    if prefix:match("^%s*//") or prefix:match("^%s*/?%*") then
        return true
    end

    -- The syntax tree additionally finds trailing and inline comments.
    local ok, commented = pcall(function()
        local root = java_root(bufnr)
        if not root then
            return false
        end
        local node = root:named_descendant_for_range(row, col, row, col)
        while node do
            if COMMENT_NODES[node:type()] then
                return true
            end
            node = node:parent()
        end
        return false
    end)
    return ok and commented == true
end

--- Check whether a method with the given signature is already declared in the buffer.
--- Declarations are matched structurally, so a signature split across lines is found.
--- A textual match is accepted as well, unless it sits in a comment or is the tail of
--- a longer type name.
---@param bufnr integer
---@param signature string `<return type> <name>(<parameter type> <parameter name>)`
---@return boolean
function M.method_exists(bufnr, signature)
    local expected = compact(signature)
    for _, method in ipairs(declared_methods(bufnr) or {}) do
        if compact(method.signature) == expected then
            return true
        end
    end

    for index, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)) do
        local start = line:find(signature, 1, true)
        if start then
            local preceding = start > 1 and line:sub(start - 1, start - 1) or ""
            if not preceding:match("[%w_$]") and not in_comment(bufnr, index - 1, start - 1, line) then
                return true
            end
        end
    end
    return false
end

--- Check whether declaring a method would clash with an existing overload.
--- Java rejects two methods sharing a name and erased parameter types, whatever
--- their return types are.
---@param bufnr integer
---@param name string
---@param parameter_types string[] source spellings of the parameter types
---@return boolean
function M.method_name_taken(bufnr, name, parameter_types)
    local expected = {}
    for index, parameter_type in ipairs(parameter_types) do
        expected[index] = erased_simple_type(parameter_type)
    end

    for _, method in ipairs(declared_methods(bufnr) or {}) do
        if method.name == name and #method.parameter_types == #expected then
            local same = true
            for index, parameter_type in ipairs(expected) do
                if method.parameter_types[index] ~= parameter_type then
                    same = false
                    break
                end
            end
            if same then
                return true
            end
        end
    end
    return false
end

--- Choose the first candidate method name that can be declared in the buffer.
--- The preferred name comes first, so nothing changes unless it would clash.
---@param bufnr integer
---@param names string[] candidate method names in order of preference
---@param parameter_types string[] source spellings of the parameter types
---@param render fun(name: string): string renders the signature declared under a name
---@return string|nil signature signature to declare
---@return string|nil existing signature that is already declared, when there is one
function M.available_signature(bufnr, names, parameter_types, render)
    for _, name in ipairs(names) do
        local signature = render(name)
        if M.method_exists(bufnr, signature) then
            return nil, signature
        end
        if not M.method_name_taken(bufnr, name, parameter_types) then
            return signature, nil
        end
    end
    return nil, nil
end

--- Return the row range of the method declaration containing a buffer position.
--- Annotations belong to the declaration, so a cursor on `@Mapping` is inside it.
---@param bufnr integer
---@param row integer zero-based row
---@param col integer zero-based column
---@return integer|nil start_row zero-based
---@return integer|nil end_row zero-based, inclusive
function M.method_rows_at(bufnr, row, col)
    local ok, start_row, end_row = pcall(function()
        local method = M.method_at_diagnostic(bufnr, { lnum = row, col = col })
        if not method then
            return nil, nil
        end
        local first, _, last = method:range()
        return first, last
    end)
    if not ok then
        return nil, nil
    end
    return start_row, end_row
end

return M
