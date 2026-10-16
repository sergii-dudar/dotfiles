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
-- RESIZE: no title bar, window stays resizable (the tiler needs that; NONE would drop it). The two macOS flags give the
-- alacritty/ghostty look: no window shadow (on macOS 26 the shadow paints a light 1px rim around the window, the "border"
-- ghostty needed macos-window-shadow = false for) and square corners instead of the rounded ones macOS puts on every
-- titled window. Padding is 0 above; the leftover space when the tiler's size is not a whole number of cells stays at
-- the right/bottom edge like in alacritty (window_content_alignment could centre it instead).
config.window_decorations = "RESIZE|MACOS_FORCE_DISABLE_SHADOW|MACOS_FORCE_SQUARE_CORNERS"
-- Plain background with the same slight transparency as alacritty (0.98; ghostty sits at 0.95). Text stays opaque.
-- The background image that used to replace this is commented out in appearance.lua.
config.window_background_opacity = 0.98 -- same as alacritty opacity (ghostty, foot too); was 0.94
--config.macos_window_background_blur = 10

-- Use zsh by default
--local util = require("util")
--config.default_prog = { '/bin/zsh' } -- util.isMac() and { '/bin/zsh' } or { '/usr/bin/zsh' }

-- Hide the mouse pointer while typing: same as alacritty mouse.hide_when_typing, ghostty mouse-hide-while-typing, foot
config.hide_mouse_cursor_when_typing = true

-- disable cursor blinking
config.cursor_blink_rate = 0

-- enable kitty keyboard protocol to support recognizing external keys, like `CMD` on macos
--config.enable_kitty_keyboard = true
--config.enable_csi_u_key_encoding = false

-- kitty graphics protocol for snacks.image in nvim (images in markdown/javadoc). Off by default in wezterm, which then
-- ignores every image snacks sends. wezterm has no unicode placeholders, so snacks cannot draw images inline in the buffer
config.enable_kitty_graphics = true

-- Both Option keys act as Alt (ESC prefix), like alacritty option_as_alt = "Both" and ghostty macos-option-as-alt.
-- The left one is already wezterm's default; the right one composes characters unless told otherwise. macOS only.
config.send_composed_key_when_right_alt_is_pressed = false

-- TERM stays at wezterm's default xterm-256color: tmux identifies wezterm from its XTVERSION reply and grants undercurl,
-- strikethrough and hyperlinks itself (see terminal-features in tmux/.tmux.conf), nvim recognises TERM_PROGRAM=WezTerm,
-- and `term = "wezterm"` would need the wezterm terminfo installed on every ssh target.

config.window_close_confirmation = "NeverPrompt"

return config