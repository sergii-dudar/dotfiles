local M = {}

local get_color_scheme = function()
    -- darker
    -- return "TerminixDark (Gogh)"

    -- good
    -- return "Atelier Seaside (base16)"
    -- return "Atelierseaside (dark) (terminal.sexy)"
    -- return "AyuDark (Gogh)"
    -- return "Breath Darker (Gogh)"
    -- return "Brogrammer"
    -- return "Cai (Gogh)"
    -- return "Circus (base16)"
    -- return "Floraverse"
    -- return "Summerfruit Dark (base16)"

    -- best
    -- return "Brewer (base16)"
    -- return "Colors (base16)"
    -- return "duckbones"
    -- return "Terminix Dark (Gogh)"

    -- return "Ef-Dark"
    -- return "Equilibrium Dark (base16)"
    -- return "GitHub Dark"

    -- vscode palette, the same 16 colours + foreground as alacritty themes/vscode.toml and ghostty "Dark+" (NOT the two
    -- "Vs Code Dark+ (Gogh)" schemes, those are a different palette with the same name). Background overridden below.
    return "Dark+"
end

function M.setup(config, wezterm)
    local home = os.getenv("HOME")

    config.color_scheme = get_color_scheme()

    config.colors = {
        -- Dark+ ships #1e1e1e; alacritty.toml overrides its theme with this value, ghostty/foot too (was #181818).
        background = "#121212",
        cursor_bg = "#bbbbbb",
        cursor_fg = "#181818",
        cursor_border = "#bbbbbb",
        -- green mouse selection, same in alacritty/ghostty/foot (tmux copy-mode stays yellow); was #444444 / #dddddd
        selection_bg = "#3b7a57",
        selection_fg = "#f0f0f0",
    }

    -- FONT
    --wezterm.gui.get_appearance()
    config.font = wezterm.font("CaskaydiaCove Nerd Font") --'Hack Nerd Font'
    config.font_size = 23.0

    -- disable ligature globally
    config.harfbuzz_features = { "calt=0", "clig=0", "liga=0" }

    -- BACKGROUND IMAGE -- disabled: alacritty and ghostty run a plain, slightly transparent background, so wezterm does
    -- the same (window_background_opacity in wezterm.lua). To bring the image back, uncomment the block. Its two knobs:
    -- hsb.brightness (0.04 = almost black, 1.0 = the photo as is) and a per-layer opacity = 0.0..1.0 (how much of the
    -- colour-scheme background shows through the image, 1.0 = image fully covers it). `home` above is kept for the path.
    --[[
    config.background = {
        {
            source = {
                File = home .. "/wallpapers/jpg/Wall147.jpg",
            },
            -- The texture tiles vertically but not horizontally.
            -- When we repeat it, mirror it so that it appears "more seamless".
            -- An alternative to this is to set `width = "100%"` and have
            -- it stretch across the display
            repeat_x = "Mirror",
            hsb = { brightness = 0.04 },
            -- opacity = 0.9,
            -- When the viewport scrolls, move this layer 10% of the number of
            -- pixels moved by the main viewport. This makes it appear to be
            -- further behind the text.
            attachment = { Parallax = 0.1 },
        },
    }
    ]]
end

return M