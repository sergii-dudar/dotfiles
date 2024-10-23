local M = {}

function M.find(tbl, finder)
    for _, v in ipairs(tbl) do
        if finder(v) then
            return v
        end
    end

    return nil
end

function M.find_by(tbl, key, value)
    for _, subtable in ipairs(tbl) do
        if subtable[key] == value then
            return subtable
        end
    end
end

return M