local M = {}

function M.log(msg)
    local f = io.open("/tmp/hypr.log", "a")

    if f then
        f:write(os.date("[%H:%M:%S] ") .. tostring(msg) .. "\n")
        f:close()
    end
end

return M