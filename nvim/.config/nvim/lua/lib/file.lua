local M = {}

function M.read_file(filepath)
    local file = io.open(filepath, "r")
    if not file then
        return nil, "Could not open file"
    end
    local content = file:read("*all")
    file:close()
    return content
end

return M
