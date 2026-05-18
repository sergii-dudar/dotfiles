local c = require("utils.common-vars")
local keys = require("utils.common-util").keys

-- Navigations

hl.bind(keys(c.mainMod, c.shift, c.vleft), hl.dsp.window.move({ direction = "l" }))
hl.bind(keys(c.mainMod, c.shift, c.vright), hl.dsp.window.move({ direction = "r" }))
-- hl.bind(keys(mod, shift, vup), hl.dsp.window.move({ direction = "u" }))
-- hl.bind(keys(mod, shift, vdown), hl.dsp.window.move({ direction = "d" }))
hl.bind(keys(c.mainMod, c.shift, c.vup), hl.dsp.window.swap({ prev = true }))
hl.bind(keys(c.mainMod, c.shift, c.vdown), hl.dsp.window.swap({ next = true }))

local function cycle_next(prev)
    return function()
        local win = hl.get_active_window()
        local lay_index = win and win.fullscreen or 0
        if prev then
            hl.dispatch(hl.dsp.window.cycle_next({ next = false }))
        else
            hl.dispatch(hl.dsp.window.cycle_next())
        end
        local full_mode_name = c.full_mode_names[lay_index]
        if full_mode_name then
            hl.dispatch(hl.dsp.window.fullscreen({ mode = full_mode_name }))
        end
    end
end
-- hl.bind(keys(mainMod, vup), hl.dsp.window.cycle_next({ next = false }))
-- hl.bind(keys(mainMod, vdown), hl.dsp.window.cycle_next())
hl.bind(keys(c.mainMod, c.vup), cycle_next(true))
hl.bind(keys(c.mainMod, c.vdown), cycle_next(false))

-- Special
hl.bind(keys(c.mainMod, "S"), hl.dsp.layout("togglesplit")) -- (toggle vertical\horisontal)
hl.bind(keys(c.alt, "S"), hl.dsp.layout("swapsplit"))
