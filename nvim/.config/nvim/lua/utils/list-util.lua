local M = {}

--- Find first by filter function
---@param tbl [table]
---@param predicate function(table)
---@return table|nil
M.findFirst = function(tbl, predicate)
    for _, v in ipairs(tbl) do
        if predicate(v) then
            return v
        end
    end
    return nil
end

--- Find all by filter function
---@param tbl [table]
---@param predicate function(table)
---@return [table]
M.findAll = function(tbl, predicate)
    local result = {}
    for _, v in ipairs(tbl) do
        if predicate(v) then
            table.insert(result, v)
        end
    end
    return result
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

M.to_unique_list = function(tbl)
    local seen_map = {}
    local unique = {}

    for _, v in ipairs(tbl) do
        if not seen_map[v] then
            seen_map[v] = true
            table.insert(unique, v)
        end
    end

    return unique
end

--- Return iterator that during interation will use exactly same order that was used when items in table was inserted.
M.sorted_iter = function(list)
    local i = {}
    for k in next, list do
        table.insert(i, k)
    end
    table.sort(i)
    return function()
        local k = table.remove(i)
        if k ~= nil then
            return k, list[k]
        end
    end
end

return M
