-- Pull in the wezterm API
local wezterm = require('wezterm')

-- This will hold the configuration.
local config = wezterm.config_builder()
wezterm.log_info("reloading")

require("mouse").setup(config)
require("keys").setup(config)
require("links").setup(config)
local util = require("util")

local act = wezterm.action
local home = os.getenv("HOME")

--print('hello')

-- For example, changing the color scheme:

-- 'Monokai Remastered',
-- 'Elementary (Gogh)',
-- 'iTerm2 Tango Dark',
-- 'Summerfruit Dark (base16)',
-- 'Monokai Dark (Gogh)',
-- 'Pencil Dark (Gogh)',
-- 'Pro (Gogh)',
-- 'OneHalfDark',
-- 'Papercolor Dark (Gogh)',
-- 'Pandora',
-- 'Summerfruit Dark (base16)',
-- 'Aardvark Blue',
-- 'Andromeda',
-- 'Apple System Colors',
-- 'Calamity',
-- 'Chalk',
-- 'ChallengerDeep',
-- 'Catppuccin Mocha'

--config.color_scheme = 'Elementary (Gogh)'
config.color_scheme = 'Andromeda'

--wezterm.gui.get_appearance()
config.font = wezterm.font 'CaskaydiaCove Nerd Font' --'Hack Nerd Font'
config.harfbuzz_features = { 'calt=0' }

config.font_size = 21.0
config.freetype_load_target = 'Light'
config.freetype_render_target = 'HorizontalLcd'

config.initial_cols = 160
config.initial_rows = 54
config.webgpu_power_preference = "HighPerformance"
config.enable_tab_bar = false
config.window_close_confirmation = 'NeverPrompt'

-- Remove all padding
config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }

-- Remove the title bar from the window
--config.window_decorations = "INTEGRATED_BUTTONS | RESIZE"
config.window_decorations = "RESIZE"
--config.window_background_opacity = 0.75
--config.macos_window_background_blur = 10

-- Use zsh by default
--config.default_prog = { '/bin/zsh' } -- util.isMac() and { '/bin/zsh' } or { '/usr/bin/zsh' }

-- Don't hide cursor when typing
config.hide_mouse_cursor_when_typing = false

local dimmer = { brightness = 0.2 }
config.background = {
    {
        source = {
            File = home..'/wallpapers/png/a_cat_walking_on_a_hill.png',
        },
        -- The texture tiles vertically but not horizontally.
        -- When we repeat it, mirror it so that it appears "more seamless".
        -- An alternative to this is to set `width = "100%"` and have
        -- it stretch across the display
        repeat_x = 'Mirror',
        hsb = dimmer,
        -- When the viewport scrolls, move this layer 10% of the number of
        -- pixels moved by the main viewport. This makes it appear to be
        -- further behind the text.
        attachment = { Parallax = 0.1 },
    }
}

-- and finally, return the configuration to wezterm
return config