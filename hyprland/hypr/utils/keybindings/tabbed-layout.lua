local c = require("utils.common-vars")
local keys = require("utils.common-util").keys

-- Tabbed
hl.bind(keys(c.mainMod, "F"), hl.dsp.group.toggle()) -- tabbed
hl.bind(keys(c.mainMod, "code:49"), hl.dsp.focus({ last = true })) -- focuscurrentorlast
hl.bind(keys(c.mainMod, "code:59"), hl.dsp.group.prev()) -- b - back
hl.bind(keys(c.mainMod, "code:60"), hl.dsp.group.next()) -- f - forward
hl.bind(keys(c.mainMod, c.shift, "code:59"), hl.dsp.window.move({ into_group = "l" })) -- Moves the active window into a group left
hl.bind(keys(c.mainMod, c.shift, "code:60"), hl.dsp.window.move({ into_group = "r" })) -- Moves the active window into a group right
