local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

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

M.decore_with_background = function(widget, bg_color)
    return wibox.widget({
        {
            --wibox.container.margin(my_widget, left, right, top, bottom)
            wibox.container.margin(widget, 10, 10, 0, 2),
            shape = function(cr, width, height)
                gears.shape.rounded_rect(cr, width, height, 10)
            end,
            bg = bg_color,
            widget = wibox.container.background,
        },
        margins = { top = 1, bottom = 1, left = 0, right = 0 },
        widget = wibox.container.margin, -- Wrap the widget in a margin container
    })
end

return M
