local M = {}

function M.keys(...)
    return table.concat({ ... }, " + ")
end

function M.dump(tbl, indent)
    indent = indent or 0

    if type(tbl) ~= "table" then
        return tostring(tbl)
    end

    local formatting = string.rep("  ", indent)
    local result = "{\n"

    for k, v in pairs(tbl) do
        result = result .. formatting .. "  " .. tostring(k) .. " = "

        if type(v) == "table" then
            result = result .. M.dump(v, indent + 1)
        else
            result = result .. tostring(v)
        end

        result = result .. ",\n"
    end

    result = result .. formatting .. "}"

    return result
end

return M
