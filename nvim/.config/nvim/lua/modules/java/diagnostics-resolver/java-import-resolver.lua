--- Cursor-targeted Java import resolution for generated diagnostic fixes.

local lsp_java = require("utils.lang.java.lsp-java")

local M = {}

--- Resolve an import at a generated type position without moving the user's cursor.
---@param bufnr integer
---@param import_cursor integer[]|nil one-based line and zero-based column
---@return boolean scheduled
function M.resolve_at(bufnr, import_cursor)
    if not import_cursor then
        return false
    end

    vim.defer_fn(function()
        if vim.api.nvim_get_current_buf() ~= bufnr then
            return
        end

        local restore_cursor = vim.api.nvim_win_get_cursor(0)
        vim.api.nvim_win_set_cursor(0, import_cursor)
        pcall(lsp_java.resolve_imports)
        vim.api.nvim_win_set_cursor(0, restore_cursor)
    end, 250)
    return true
end

return M
