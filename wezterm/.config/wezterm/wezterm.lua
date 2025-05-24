local wezterm = require("wezterm")

local config = wezterm.config_builder()
-- wezterm.log_info("reloading")

require("mouse").setup(config)
require("keys").setup(config)
require("links").setup(config)
require("appearance").setup(config, wezterm)

config.freetype_load_target = "Light"
config.freetype_render_target = "HorizontalLcd"

config.initial_cols = 160
config.initial_rows = 54
config.webgpu_power_preference = "HighPerformance"
config.enable_tab_bar = false
config.enable_wayland = false

-- Remove all padding
config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }

-- Remove the title bar from the window
--config.window_decorations = "INTEGRATED_BUTTONS | RESIZE"
config.window_decorations = "RESIZE"
--config.window_background_opacity = 0.75
--config.macos_window_background_blur = 10

-- Use zsh by default
--local util = require("util")
--config.default_prog = { '/bin/zsh' } -- util.isMac() and { '/bin/zsh' } or { '/usr/bin/zsh' }

-- Don't hide cursor when typing
config.hide_mouse_cursor_when_typing = false

-- disable cursor blinking
config.cursor_blink_rate = 0

-- enable kitty keyboard protocol to support recognizing external keys, like `CMD` on macos
--config.enable_kitty_keyboard = true
--config.enable_csi_u_key_encoding = false

config.window_close_confirmation = "NeverPrompt"

return config
