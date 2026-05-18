local c = require("utils.common-vars")
local keys = require("utils.common-util").keys

local function cycle_next(prev)
    return function()
        local win = hl.get_active_window()
        local lay_index = win and win.fullscreen or 0
        if prev then
            -- hl.dispatch(hl.dsp.window.cycle_next({ next = false }))
            hl.dispatch(hl.dsp.layout("cycleprev"))
        else
            -- hl.dispatch(hl.dsp.window.cycle_next())
            hl.dispatch(hl.dsp.layout("cyclenext"))
        end
        local full_mode_name = c.full_mode_names[lay_index]
        if full_mode_name then
            hl.dispatch(hl.dsp.window.fullscreen({ mode = full_mode_name }))
        end
    end
end
-- hl.bind(keys(mainMod, vup), hl.dsp.layout("cycleprev"))
-- hl.bind(keys(mainMod, vdown), hl.dsp.layout("cyclenext"))
hl.bind(keys(c.mainMod, c.vup), cycle_next(true))
hl.bind(keys(c.mainMod, c.vdown), cycle_next(false))
hl.bind(keys(c.mainMod, c.shift, c.vup), hl.dsp.layout("swapprev"))
hl.bind(keys(c.mainMod, c.shift, c.vdown), hl.dsp.layout("swapnext"))

-- Specific
hl.bind(keys(c.mainMod, "S"), hl.dsp.layout("orientationcycle"))

hl.bind(keys(c.mainMod, "Tab"), hl.dsp.window.fullscreen({ mode = "maximized" }))
hl.bind(keys(c.mainMod, "F"), hl.dsp.window.fullscreen({ mode = "fullscreen" }))
