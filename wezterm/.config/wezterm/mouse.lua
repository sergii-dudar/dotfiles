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
        -- Open links by mouse left click with pressed CTRL modifier (outside tmux: with mouse reporting on, wezterm
        -- passes ctrl+click to the pane unless the bypass modifier above is held, and tmux opens the link itself:
        -- .tmux.conf C-MouseDown1Pane -> scripts/tmux/open-url.sh, so the gesture is the same inside tmux)
        {
            event = { Up = { streak = 1, button = 'Left' } },
            mods = 'CTRL',
            action = wezterm.action.OpenLinkAtMouseCursor,
        }
    }
end

return M
