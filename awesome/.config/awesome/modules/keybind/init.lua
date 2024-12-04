local keyboard_bind = require("modules.keybind.keyboard-bind")
local mouse_bind = require("modules.keybind.mouse-bind")
local tag_bind = require("modules.keybind.tag-bind")
local client_bind = require("modules.keybind.client-bind")

local M = {}

M.taglist_buttons = mouse_bind.taglist_buttons
M.tasklist_buttons = mouse_bind.tasklist_buttons
M.clientbuttons = mouse_bind.clientbuttons
M.globalkeys = tag_bind.extend_globalkeys(keyboard_bind.globalkeys)
M.clientkeys = client_bind.clientkeys

return M
