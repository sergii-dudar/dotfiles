pcall(require, "luarocks.loader")

require("awful.autofocus")
local beautiful = require("beautiful")

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
