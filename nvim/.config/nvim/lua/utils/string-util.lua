-- String manipulation helpers.
--
-- • starts_with — check if string starts with prefix
-- • split_by_last_dot — split "a.b.c" into ("a.b", "c")
-- • split — split string by delimiter
-- • contains — substring check
-- • is_not_empty — nil/empty guard
-- • any_eq — check if string equals any in a list

local M = {}

--- Check whether a string starts with a prefix.
function M.starts_with(str, prefix)
    return str:sub(1, #prefix) == prefix
end

--- Split a string at its last dot.
function M.split_by_last_dot(str)
    local before, after = string.match(str, "^(.*)%.([^%.]+)$")
    return before, after
end

--- Split a string by a delimiter.
function M.split(str, delimiter)
    return vim.split(str, delimiter, { plain = true })
end

--- Check whether a string contains a substring.
function M.contains(str, substr)
    return string.find(str, substr, 1, true)
end

--- Check whether a string is not nil or empty.
function M.is_not_empty(str)
    return str ~= nil and str ~= ""
end

--- Check whether a string equals any value in a list.
function M.any_eq(target_string, match_table)
    for _, current_match in ipairs(match_table) do
        if target_string == current_match then
            return true
        end
    end
    return false
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
