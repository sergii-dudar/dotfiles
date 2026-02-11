local M = {}

function M.starts_with(str, prefix)
    return str:sub(1, #prefix) == prefix
end

function M.split_by_last_dot(str)
    local before, after = string.match(str, "^(.*)%.([^%.]+)$")
    return before, after
end

function M.contains(str, substr)
    return string.find(str, substr, 1, true)
end

--- Convert table with key\values to single unique string (as lua have no buildin concept equals & hashcode)
local function table_to_hash(tbl)
    local parts = {}
    for k, v in pairs(tbl) do
        table.insert(parts, tostring(k) .. "=" .. tostring(v))
    end
    table.sort(parts)
    return table.concat(parts, ";")
end

-- -- Examples:
-- print(M.starts_with("org.apache.commons.lang3.ObjectUtils", "org.apache.commons.lang3"))
-- print(M.starts_with("org.apache.commons.lang3", "org.apache.commons.lang3"))

return M
