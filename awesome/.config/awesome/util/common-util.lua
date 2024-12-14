local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local vars = require("modules.variables")

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

---@param widget (table) - widget to decoration
---@param bg_color (string) - decoration color
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

---@param content (string) span content
---@param foreground (string|nil) span content foreground color
---@param font_size (integer|nil) span content font size
M.to_span = function(content, foreground, font_size)
    content = content or ""
    foreground = foreground ~= nil and string.format("foreground='%s'", foreground) or ""

    font_size = font_size or vars.font.default_size
    local font = string.format("font='%s'", vars.font.to_size(font_size))
    return string.format("<span %s %s>%s</span>", foreground, font, content)
end

M.calculate_window_width = function(factor_width)
    factor_width = factor_width or vars.settings.default_factor_width
    return vars.settings.screen_width * factor_width
end

M.calculate_window_height = function(factor_height)
    factor_height = factor_height or vars.settings.default_factor_height
    return vars.settings.screen_height * factor_height
end

M.widget_margin = function(widget, left, right, top, bottom)
    return wibox.container.margin(widget, left, right, top, bottom)
end

return M
