local wezterm = require("wezterm")
local keys = require("keys")

local M = {}

---@param config Config
M.setup = function(config)
    config.alternate_buffer_wheel_scroll_speed = 1
    config.bypass_mouse_reporting_modifiers = keys.mod
    config.mouse_bindings = {
        -- Don't open links without modifier
        {
            event = { Up = { streak = 1, button = "Left" } },
            action = wezterm.action.CompleteSelection("ClipboardAndPrimarySelection"),
        },
        --[[{
            event = { Up = { streak = 1, button = "Left" } },
            mods = keys.mod,
            action = wezterm.action.CompleteSelectionOrOpenLinkAtMouseCursor("ClipboardAndPrimarySelection"),
        },]]
        -- Open links by mouse left click with pressed CTRL modifier
        {
            event = { Up = { streak = 1, button = 'Left' } },
            mods = 'CTRL',
            action = wezterm.action.OpenLinkAtMouseCursor,
        }
    }
end

return M
