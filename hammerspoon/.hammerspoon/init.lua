local function inputName()
    return hs.keycodes.currentLayout()
        or hs.keycodes.currentMethod()
        or hs.keycodes.currentSourceID()
        or "Unknown input"
end

local function showInputSource()
    local name = inputName()

    -- Big short overlay in the middle of screen
    hs.alert.closeAll(0)
    hs.alert.show(name, { textSize = 48 }, hs.screen.mainScreen(), 0.4)

    -- Optional macOS Notification Center banner
    -- hs.notify.new({ title = "Input source", informativeText = name }):send()
end

hs.keycodes.inputSourceChanged(showInputSource)
