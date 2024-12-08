pcall(require, "luarocks.loader")

local awful = require("awful")
require("awful.autofocus")
-- Theme handling library
local beautiful = require("beautiful")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Modules
local vars = require("modules.variables")
local funcs = require("modules.funcs")
local keybind = require("modules.keybind")

beautiful.init(vars.current_theme_path)
beautiful.icon_theme = "Dracula"
beautiful.icon_theme_path = { "/usr/share/icons/" }

local opts = { keybind = keybind }
require("modules.error-handling").setup()
require("modules.winrules").setup(opts)
require("modules.signals").setup()
require("modules.wibar").setup(opts)

-- awful.screen.connect_for_each_screen(function(s)
--     beautiful.at_screen_connect(s)
-- end)

root.keys(keybind.globalkeys)

funcs.run_shell_autostarts()
