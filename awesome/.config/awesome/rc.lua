pcall(require, "luarocks.loader")

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

local opts = { keybind = keybind }
require("modules.error-handling").setup()
require("modules.winrules").setup(opts)
require("modules.signals").setup()
require("modules.wibar").setup(opts)

root.keys(keybind.globalkeys)

funcs.run_shell_autostarts()
