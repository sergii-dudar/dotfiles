local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local vars = require("modules.variables")

local notify = function(msg)
    awful.util.spawn('notify-send "' .. msg .. '" -t 700')
end

local concat_tables = function(...)
    local result = {}
    for _, t in ipairs({ ... }) do
        for _, v in ipairs(t) do
            table.insert(result, v)
        end
    end
    return result
end

local concat_match_tables = function(...)
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

local filter_if_not_match = function(input)
    if input.is_match() then
        return input.items
    end
    return {}
end

local directory_exists = function(dir)
    local f = io.open(dir, "r")
    if f then
        f:close()
        return true
    end
    return false
end

local add_bg_hover_to_widget = function(widget, bg_color)
    if not widget.bg then
        widget = wibox.widget({
            widget,
            shape = function(cr, width, height)
                gears.shape.rectangle(cr, width, height)
            end,
            bg = vars.widget.bg_color,
            widget = wibox.container.background,
        })
    end

    widget:connect_signal("mouse::enter", function()
        widget.bg = bg_color
    end)

    widget:connect_signal("mouse::leave", function()
        widget.bg = vars.widget.bg_color
    end)

    return widget
end

---@param widget (table) - widget to decoration
---@param bg_color (string|nil) - decoration color
local decore_with_background = function(widget, bg_color, margin_left, margin_right, margin_top, margin_bottom)
    bg_color = bg_color or vars.widget.bg_color
    margin_left = margin_left or 10
    margin_right = margin_right or 10
    margin_top = margin_top or 0
    margin_bottom = margin_bottom or 0

    local bg_widget = wibox.widget({
        --wibox.container.margin(my_widget, left, right, top, bottom)
        --wibox.container.margin(widget, 10, 10, 0, 2),
        wibox.container.margin(widget, margin_left, margin_right, margin_top, margin_bottom),
        shape = function(cr, width, height)
            --gears.shape.rounded_rect(cr, width, height, 10)
            gears.shape.rectangle(cr, width, height)
        end,
        bg = bg_color,
        widget = wibox.container.background,
    })

    bg_widget = add_bg_hover_to_widget(bg_widget, vars.widget.bg_color_hover)

    return wibox.widget({
        bg_widget,
        margins = { top = 1, bottom = 1, left = 0, right = 0 },
        widget = wibox.container.margin, -- Wrap the widget in a margin container
    })
end

local decore_with_background_center = function(widget, bg_color)
    return decore_with_background(widget, bg_color, 10, 10, 0, 2)
end
local decore_with_background_left = decore_with_background
local decore_with_background_right = decore_with_background

---@param content (string) span content
---@param foreground (string|nil) span content foreground color
---@param font_size (integer|nil) span content font size
local to_span = function(content, foreground, font_size)
    content = content or ""
    foreground = foreground ~= nil and string.format("foreground='%s'", foreground) or ""
    font_size = font_size or vars.font.default_size
    local font = string.format("font='%s'", vars.font.to_size(font_size))
    return string.format("<span %s %s>%s</span>", foreground, font, content)
end

---@param content (string) span content
---@param foreground (string|nil) span content foreground color
---@param font_size (integer|nil) span content font size
local text_widget = function(content, foreground, font_size)
    font_size = font_size or vars.font.default_size
    return wibox.widget({
        widget = wibox.widget.textbox,
        markup = to_span(content, foreground, font_size),
        align = "center",
        valign = "center",
        font = vars.font.to_size(font_size),
    })
end

local calculate_window_width = function(factor_width)
    factor_width = factor_width or vars.settings.default_factor_width
    return vars.settings.screen_width * factor_width
end

local calculate_window_height = function(factor_height)
    factor_height = factor_height or vars.settings.default_factor_height
    return vars.settings.screen_height * factor_height
end

local widget_margin = function(widget, left, right, top, bottom)
    return wibox.container.margin(widget, left, right, top, bottom)
end

---@class IconOptions
---@field target_widget (any) main widget to apply icon
---@field icon_content (string) span content
---@field right_icon_content (string|nil) span content to right from wideget if provided
---@field icon_foreground (string|nil) span content foreground color
---@field icon_font_size (integer|nil) span content font size
---@field icon_right_margin (integer|nil) space margin between icon and target widget
---@param opts IconOptions of options
local add_icon_to_widget = function(opts)
    opts.icon_right_margin = opts.icon_right_margin or 3
    local icon_widget = text_widget(opts.icon_content, opts.icon_foreground, opts.icon_font_size)

    if opts.right_icon_content then
        local right_icon_widget = text_widget(opts.right_icon_content, opts.icon_foreground, opts.icon_font_size)
        return wibox.widget({
            widget_margin(icon_widget, 0, opts.icon_right_margin),
            opts.target_widget,
            widget_margin(right_icon_widget, opts.icon_right_margin, 0),
            layout = wibox.layout.fixed.horizontal,
        })
    end

    return wibox.widget({
        widget_margin(icon_widget, 0, opts.icon_right_margin),
        opts.target_widget,
        layout = wibox.layout.fixed.horizontal,
    })
end

---@class WidgetIconOptions
---@field target_widget (any) main widget to apply icon
---@field icon_widget (any) icon widget
---@field icon_right_margin (integer|nil) space margin between icon and target widget
---@param opts WidgetIconOptions of options
local add_icon_widget_to_widget = function(opts)
    opts.icon_right_margin = opts.icon_right_margin or 3

    return wibox.widget({
        widget_margin(opts.icon_widget, 0, opts.icon_right_margin),
        opts.target_widget,
        layout = wibox.layout.fixed.horizontal,
    })
end

local to_icon_widget_space = function(size)
    return to_span(" ", "#8caaee", size)
end

local group_widgets = function(...)
    return wibox.widget({
        layout = wibox.layout.fixed.horizontal,
        table.unpack({ ... }),
    })
end

-- Function to read and parse .env file
local load_env_file = function(file_path)
    local env_table = {} -- Table to store environment variables

    local file = io.open(file_path, "r") -- Open the file for reading
    if not file then
        print("Error: Could not open .env file at " .. file_path)
        return env_table
    end

    for line in file:lines() do
        -- Ignore empty lines and comments
        if line:match("^%s*#") or line:match("^%s*$") then
            goto continue
        end

        -- Parse KEY=VALUE pairs
        local key, value = line:match("^%s*([%w_]+)%s*=%s*(.+)%s*$")
        if key and value then
            -- Remove surrounding quotes, if any
            value = value:gsub("^['\"](.-)['\"]$", "%1")
            env_table[key] = value
            --os.setenv(key, value)
        end

        ::continue::
    end

    file:close()
    return env_table
end

--- function to load personal private values
local loal_private_var = function(private_key)
    local env_file_path = vars.path.home_dir .. "/private.env"
    local envs_table = load_env_file(env_file_path)
    return envs_table[private_key]
end

local to_text_icon_runner = function(content, font_zise, fg_color, shell_left_click, shell_right_click)
    local widget = wibox.widget({
        widget = wibox.widget.textbox,
        markup = to_span(content, fg_color, font_zise),
        align = "center",
        valign = "center",
        font = vars.font.to_size(font_zise),
    })
    if shell_left_click then
        widget:buttons(gears.table.join(awful.button({}, 1, function()
            awful.spawn.with_shell(shell_left_click)
        end)))
    end

    if shell_right_click then
        widget:buttons(gears.table.join(awful.button({}, 3, function()
            awful.spawn.with_shell(shell_right_click)
        end)))
    end

    return widget
end

local to_imagebox_runner = function(image_path, shell_left_click, shell_right_click)
    local widget = wibox.widget({
        widget = wibox.widget.imagebox,
        image = image_path,
        resize = true,
    })
    if shell_left_click then
        widget:buttons(gears.table.join(awful.button({}, 1, function()
            awful.spawn.with_shell(shell_left_click)
        end)))
    end

    if shell_right_click then
        widget:buttons(gears.table.join(awful.button({}, 3, function()
            awful.spawn.with_shell(shell_right_click)
        end)))
    end

    return widget
end

return {
    notify = notify,
    concat_tables = concat_tables,
    concat_match_tables = concat_match_tables,
    filter_if_not_match = filter_if_not_match,
    directory_exists = directory_exists,
    to_span = to_span,
    text_widget = text_widget,
    calculate_window_width = calculate_window_width,
    calculate_window_height = calculate_window_height,
    widget_margin = widget_margin,

    add_icon_to_widget = add_icon_to_widget,
    add_icon_widget_to_widget = add_icon_widget_to_widget,

    to_icon_widget_space = to_icon_widget_space,
    group_widgets = group_widgets,

    local_env_key = loal_private_var,

    to_text_icon_runner = to_text_icon_runner,
    to_imagebox_runner = to_imagebox_runner,
    add_bg_hover_to_widget = add_bg_hover_to_widget,

    --decore_with_background = decore_with_background,
    decore_with_background_center = decore_with_background_center,
    decore_with_background_right = decore_with_background_right,
    decore_with_background_left = decore_with_background_left,
    vars = {
        icon_widget_space = to_icon_widget_space(vars.widget.icon_widget_space),
    },
}
