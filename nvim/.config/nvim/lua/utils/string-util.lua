local M = {}

M.starts_with = function(str, prefix)
    return str:sub(1, #prefix) == prefix
end

M.split_by_last_dot = function(str)
    local before, after = string.match(str, "^(.*)%.([^%.]+)$")
    return before, after
end

-- -- Examples:
-- print(M.starts_with("org.apache.commons.lang3.ObjectUtils", "org.apache.commons.lang3"))
-- print(M.starts_with("org.apache.commons.lang3", "org.apache.commons.lang3"))

return M