local M = {}

local file_extensions = {
    avsc = true,
    class = true,
    csv = true,
    feature = true,
    gql = true,
    graphql = true,
    gradle = true,
    http = true,
    java = true,
    json = true,
    md = true,
    properties = true,
    proto = true,
    sql = true,
    txt = true,
    xml = true,
    yaml = true,
    yml = true,
}

local function has_path_separator(value)
    return value:find("/", 1, true) ~= nil or value:find("\\", 1, true) ~= nil
end

local function has_known_file_extension(value)
    local ext = value:match("%.([%w_-]+)$")
    return ext ~= nil and file_extensions[ext:lower()] == true
end

--- Transform Java `gf` candidates while preserving real resource file paths.
---@param fname string
---@return string
function M.transform(fname)
    fname = tostring(fname or "")
    if fname == "" or has_path_separator(fname) or has_known_file_extension(fname) then
        return fname
    end
    return (fname:gsub("%.", "/"))
end

return M