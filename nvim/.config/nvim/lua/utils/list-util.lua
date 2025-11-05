local M = {}

M.find = function(tbl, finder)
    for _, v in ipairs(tbl) do
        if finder(v) then
            return v
        end
    end

    return nil
end

M.any_match = function(target_string, match_table)
    -- vim.notify(
    --     "input: " .. target_string .. " --- " .. require("utils.common-util").table_to_string(match_table),
    --     vim.log.levels.INFO
    -- )
    for _, current_match in ipairs(match_table) do
        if string.match(target_string, current_match) then
            return true
        end
    end
    return false
end

M.find_by = function(tbl, key, value)
    for _, subtable in ipairs(tbl) do
        if subtable[key] == value then
            return subtable
        end
    end
end

return M