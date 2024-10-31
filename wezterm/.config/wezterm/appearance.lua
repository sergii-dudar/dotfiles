local M = {}

local get_color_scheme = function()
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
    return 'Andromeda'
end

function M.setup(config, wezterm)
    local home = os.getenv("HOME")

    config.color_scheme = get_color_scheme()

    -- FONT
    --wezterm.gui.get_appearance()
    config.font = wezterm.font 'CaskaydiaCove Nerd Font' --'Hack Nerd Font'
    config.font_size = 21.0

    -- disable ligature globally
    config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

    -- BACKGROUND IMAGE
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
            hsb = { brightness = 0.2 },
            -- When the viewport scrolls, move this layer 10% of the number of
            -- pixels moved by the main viewport. This makes it appear to be
            -- further behind the text.
            attachment = { Parallax = 0.1 },
        }
    }
end

return M
