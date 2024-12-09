local awful = require("awful")

M = {}

M.notify = function(msg)
    awful.util.spawn('notify-send "' .. msg .. '" -t 700')
end

M.concat_tables = function(...)
    local result = {}
    for _, t in ipairs({ ... }) do
        for _, v in ipairs(t) do
            table.insert(result, v)
        end
    end
    return result
end

M.concat_match_tables = function(...)
    local result = {}
    for _, t in ipairs({ ... }) do
        if t.is_match and t.is_match() then
            for _, v in ipairs(t.table) do
                table.insert(result, v)
            end
        else
            for _, v in ipairs(t) do
                table.insert(result, v)
            end
        end
    end
    return result
end

M.filter_if_not_match = function(input)
    if input.is_match() then
        return input.items
    end
    return {}
end

M.directory_exists = function(dir)
    local f = io.open(dir, "r")
    if f then
        f:close()
        return true
    end
    return false
end

return M
