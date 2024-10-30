local wezterm = require("wezterm") --[[@as Wezterm]]

local act = wezterm.action
local M = {}

--M.mod = wezterm.target_triple:find("windows") and "ALT" or "SUPER"
M.mod = "SUPER"

---@param config Config
function M.setup(config)
  --config.disable_default_key_bindings = true
  config.keys = {
    -- Clipboard
    { mods = 'CTRL', key = "c", action = act.CopyTo("Clipboard") },
    { mods = 'CTRL', key = "v", action = act.PasteFrom("Clipboard") },

    --[[{ key = 'F11', action = wezterm.action.ToggleFullScreen },
    {
        key = 'c',
        mods = 'CTRL',
        action = wezterm.action_callback(function(window, pane)
            local sel = window:get_selection_text_for_pane(pane)
            if (not sel or sel == '') then
                window:perform_action(wezterm.action.SendKey{ key='c', mods='CTRL' }, pane)
            else
                window:perform_action(wezterm.action{ CopyTo = 'ClipboardAndPrimarySelection' }, pane)
            end
        end),
    },
    { key = 'v', mods = 'CTRL', action = wezterm.action{ PasteFrom = 'Clipboard' } },
      -- MAC
    { key = 'c', mods = 'ALT', action = wezterm.action{ CopyTo = 'ClipboardAndPrimarySelection' } },
    { key = 'v', mods = 'ALT', action = wezterm.action{ PasteFrom = 'Clipboard' } },]]
  }
end

return M
