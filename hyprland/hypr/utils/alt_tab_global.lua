-- Alt-Tab global window switcher for Hyprland Lua config
-- Focuses the most recently focused window that isn't the current one.
-- Usage: local alt_tab = require("scripts/alt_tab_global")
--        alt_tab.switch()

local M = {}

function M.switch()
    local active = hl.get_active_window()
    if not active then
        return
    end

    local windows = hl.get_windows()

    -- Sort by focus history (lowest = most recent)
    table.sort(windows, function(a, b)
        return (a.focus_history_id or a.focusHistoryID or 0) < (b.focus_history_id or b.focusHistoryID or 0)
    end)

    -- Focus the first window that isn't the active one
    for _, w in ipairs(windows) do
        if w.address ~= active.address then
            hl.dispatch(hl.dsp.focus({ window = "address:" .. w.address }))
            return
        end
    end
end

return M
