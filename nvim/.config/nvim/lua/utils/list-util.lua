-- List/table searching and filtering utilities.
--
-- • findFirst — find first element matching predicate
-- • findAll — collect all elements matching predicate
-- • any_match — check if target string contains any substring from list
-- • find_by — find element by key-value match
-- • to_unique_list — deduplicate a list

local M = {}

---@param tbl [table]
---@param predicate function(table)
---@return table|nil
function M.findFirst(tbl, predicate)
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
function M.findAll(tbl, predicate)
    local result = {}
    for _, v in ipairs(tbl) do
        if predicate(v) then
            table.insert(result, v)
        end
    end
    return result
end

--- Check whether the target string matches any value in the list.
function M.any_match(target_string, match_table)
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

--- Find the first element whose key matches the value.
function M.find_by(tbl, key, value)
    for _, subtable in ipairs(tbl) do
        if subtable[key] == value then
            return subtable
        end
    end
end

--- Return a deduplicated copy of a list.
function M.to_unique_list(tbl)
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

--[[ local logging = require("utils.logging-util")
local log = logging.new({ name = "java-refactor", filename = "java-refactor.log" })

local tab = {}
table.insert(tab, "1")
table.insert(tab, "2")
table.insert(tab, "3")
table.insert(tab, "4")
table.insert(tab, "5")

for _, value in ipairs(tab) do
    print(_)
    log.debug(_)
end

log.debug("---")
for i = #tab, 1, -1 do
    local value = tab[i]
    print(i)
    log.debug(i)
end
log.debug("++++") ]]

return M
