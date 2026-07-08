local function inputName()
    return hs.keycodes.currentLayout()
        or hs.keycodes.currentMethod()
        or hs.keycodes.currentSourceID()
        or "Unknown input"
end

-- Keep handles on the overlays currently on screen (one per monitor) plus any
-- pending draw, so each language switch replaces the previous overlays cleanly.
-- We show the alert on *every* screen rather than a single "active" one:
-- hs.screen.mainScreen()/primaryScreen() point at the focused window's monitor,
-- which isn't necessarily the monitor you're looking at when you switch
-- language on a multi-monitor setup. Drawing on all screens guarantees it's
-- always visible where you are.
local currentAlerts = {}
local pendingTimer = nil

local function closeCurrentAlerts()
    for _, id in ipairs(currentAlerts) do
        hs.alert.closeSpecific(id, 0)
    end
    currentAlerts = {}
end

local function showInputSource()
    -- Replace the previous overlays explicitly instead of hs.alert.closeAll(0),
    -- which races an immediately-following show() and can leave it undrawn.
    closeCurrentAlerts()

    -- Coalesce rapid language switches into a single draw.
    if pendingTimer then
        pendingTimer:stop()
    end

    -- Defer one runloop tick so the previous overlays finish tearing down
    -- before the new ones are drawn.
    pendingTimer = hs.timer.doAfter(0.03, function()
        pendingTimer = nil
        local name = inputName()

        -- Big short overlay near the top of every screen
        for _, scr in ipairs(hs.screen.allScreens()) do
            currentAlerts[#currentAlerts + 1] = hs.alert.show(name, { textSize = 48, atScreenEdge = 1 }, scr, 0.4)
        end

        -- Optional macOS Notification Center banner
        -- hs.notify.new({ title = "Input source", informativeText = name }):send()
    end)
end

hs.keycodes.inputSourceChanged(showInputSource)
