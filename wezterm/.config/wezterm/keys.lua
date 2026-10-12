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
        --{ mods = 'CTRL', key = "c", action = act.CopyTo("Clipboard") },
        { mods = "CTRL", key = "v", action = act.PasteFrom("Clipboard") },
        --{ mods = 'CMD', key = 'k', action = act.DisableDefaultAssignment },

        -- --- parity with alacritty.toml ---
        -- Ctrl+, and Ctrl+. have no legacy control code. alacritty and ghostty emit CSI u for them on their own; wezterm
        -- sends a bare , and . (key probe through tmux, 2026-10-12, wezterm 20261010). nvim pickers map <C-,>/<C-.> and
        -- tmux has root bindings for C-, / C-. , so send the same bytes. The probe showed Ctrl+6 (0x1e) and Ctrl+Shift+T
        -- (0x14) already match; Ctrl+/ arrives as 0x1f = <C-_> where alacritty/ghostty give CSI u <C-/>, LazyVim binds
        -- both, so it is left alone.
        { mods = "CTRL", key = ",", action = act.SendString("\x1b[44;5u") },
        { mods = "CTRL", key = ".", action = act.SendString("\x1b[46;5u") },

        -- translate the mouse selection -> tmux popup (scripts/tmux/translate.sh). Mouse-up already copied the
        -- selection (mouse.lua), but wezterm can hand the text over itself, so use the script's `pipe` mode: the
        -- selection travels as an argv element (no shell quoting) and sh feeds it to the script's stdin.
        -- background_child_process returns at once, so the GUI never waits for the popup.
        {
            mods = "CTRL|SHIFT",
            key = "t",
            action = wezterm.action_callback(function(window, pane)
                local sel = window:get_selection_text_for_pane(pane) or ""
                wezterm.background_child_process({
                    "sh",
                    "-c",
                    'printf "%s" "$1" | ~/dotfiles/scripts/tmux/translate.sh pipe',
                    "sh",
                    sel,
                })
            end),
        },

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
