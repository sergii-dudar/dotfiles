local M = {}

---@return table
function M.build_cmd()
    vim.notify("java cmd")
    -- TODO:
    return { "echo", "hello" }
end

return M
