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
local keybind = require("modules.keybind")
beautiful.init(vars.current_theme_path)

require("modules.error-handling").setup()
require("modules.winrules").setup()
require("modules.signals").setup()
require("modules.wibar")

root.keys(keybind.globalkeys)

-- Autostart applications
awful.spawn.with_shell([[
  if [ ! -f /tmp/awesome_startup_done ]; then
    ~/dotfiles/bin/wmscripts/autostart_once.sh
    touch /tmp/awesome_startup_done
  fi
]])

awful.spawn.with_shell("~/dotfiles/bin/wmscripts/autostart_always.sh")
