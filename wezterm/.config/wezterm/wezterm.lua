-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

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

config.color_scheme = 'Elementary (Gogh)'

--wezterm.gui.get_appearance()
config.font = wezterm.font 'Hack Nerd Font'

config.font_size = 18.0
config.freetype_load_target = 'Light'
config.freetype_render_target = 'HorizontalLcd'

config.initial_cols = 120
config.initial_rows = 40

config.enable_tab_bar = false

-- Remove all padding
config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }

config.keys = {
    { key = 'F11', action = wezterm.action.ToggleFullScreen },
}

-- Remove the title bar from the window
config.window_decorations = "INTEGRATED_BUTTONS | RESIZE"

-- Use zsh by default
config.default_prog = { '/usr/bin/zsh' }

-- Don't hide cursor when typing
config.hide_mouse_cursor_when_typing = false

local dimmer = { brightness = 0.2 }
config.background = {
    {
        source = {
            File = '/home/serhii/wallpapers/png/a_cat_walking_on_a_hill.png',
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