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

return M
