local M = {}

function M.isMac()
    local handle = io.popen("uname")
    local result = handle:read("*a")
    handle:close()
    return result:find("Darwin") ~= nil
end

return M
