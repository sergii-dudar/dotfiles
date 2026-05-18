local M = {}

function M.keys(...)
    return table.concat({ ... }, " + ")
end

return M