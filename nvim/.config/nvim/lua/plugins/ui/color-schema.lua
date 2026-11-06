return {
    -- ##############  FAVORITE ###################
    -- {
    --     "catppuccin/nvim",
    --     name = "catppuccin",
    --     priority = 1000,
    --     optional = false,
    --     lazy = false,
    --     opts = {
    --         custom_highlights = function()
    --             return {
    --                 Visual = { bg = "#373267" },
    --             }
    --         end,
    --     },
    --     --[[opts = {
    --       term_colors = true,
    --       transparent_background = true
    --   }]]
    -- },
    -- {
    --     "LazyVim/LazyVim",
    --     opts = {
    --         -- colorscheme = "catppuccin-mocha",
    --         -- colorscheme = "gruvbox",
    --     },
    -- },
    -- ############## MOVE EYE (Monokai, Gruvbox, Everforest) ###################
    -- {
    --     "",
    --     lazy = false,
    --     optional = false,
    --     priority = 1000,
    --     config = function()
    --         vim.cmd.colorscheme("")
    --     end,
    -- },
    {
        "sainnhe/gruvbox-material",
        lazy = false,
        optional = false,
        priority = 1000,
        config = function()
            -- Available values: 'hard', 'medium'(default), 'soft'
            vim.g.gruvbox_material_background = "hard"
            vim.g.gruvbox_material_foreground = "material" -- 'material', 'mix', 'original'
            vim.g.gruvbox_material_enable_italic = true
            vim.g.gruvbox_material_enable_bold = true
            -- `'grey background'`, `'green background'`, `'blue background'`, `'red background'`, `'reverse'`
            vim.g.gruvbox_material_visual = "red background"
            -- vim.g.gruvbox_material_cursor = "green" -- `'auto'`, `'red'`, `'orange'`, `'yellow'`, `'green'`, `'aqua'`, `'blue'`, `'purple'`
            vim.g.gruvbox_material_float_style = "blend" -- `'bright'`, `'dim'`, `'blend'`
            vim.g.gruvbox_material_statusline_style = "default" -- `'default'`, `'mix'`, `'original'`
            -- vim.g.gruvbox_material_sign_column_background = "grey"
            -- vim.g.gruvbox_material_ui_contrast = "low"
            -- vim.g.gruvbox_material_colors_override = {
            --   bg0 = { "#1d2021", "234" },
            --   bg2 = { "#282828", "235" },
            -- }
            -- Darker than "hard" (bg0 #1d2021, bg_dim #141617). bg0 = editor/float background, bg_dim = neo-tree
            -- and other dimmed surfaces; keep bg_dim below bg0 so the side panel stays distinguishable.
            vim.g.gruvbox_material_colors_override = {
                bg0 = { "#1b1d1e", "233" },
                bg_dim = { "#131516", "232" },
            }
            -- vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#FF0000', bg = 'NONE', bold = true })
            vim.cmd.colorscheme("gruvbox-material")
            vim.api.nvim_set_hl(0, "CursorLineNr", { link = "GreenBold" })

            -- nvim-treesitter/nvim-treesitter-context: the theme paints it bg3 (#3c3836), too bright on the darker bg0
            -- override above. #212425 (a touch above the stock hard bg0 #1d2021) sits between that bg0 and CursorLine
            -- (#282828); the same value as CursorLine would merge the bar with the cursor line right below it.
            vim.api.nvim_set_hl(0, "TreesitterContext", { fg = "#ddc7a1", bg = "#212425" }) -- fg1, as in the theme

            -- unblevable/quick-scope
            vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = "#ff0000", underline = true, bold = true })
            vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = "aqua", underline = true, bold = true })

            -- folke/flash.nvim (gruvbox-material ships no Flash* groups, so flash would link Label/Match/Current to
            -- Substitute/Search/IncSearch: yellow, green and red boxes with the same dark text). Palette: hard/material.
            vim.api.nvim_set_hl(0, "FlashBackdrop", { fg = "#7c6f64" }) -- grey0: dim the rest, no italic
            vim.api.nvim_set_hl(0, "FlashMatch", { fg = "#ddc7a1", bg = "#504945" }) -- fg1 on bg5: quiet box per match
            vim.api.nvim_set_hl(0, "FlashCurrent", { fg = "#1d2021", bg = "#89b482" }) -- bg0 on aqua: match under cursor
            vim.api.nvim_set_hl(
                0,
                "FlashLabel",
                { fg = "#1d2021", bg = "#fabd2f", bold = true, underline = true, italic = true }
            ) -- bg0 on bright gruvbox yellow: jump labels (palette yellow #d8a657 is the muted Substitute colour, orange #e78a4e the calmer option)
            --
            -- INFO: undercurl needs the outer terminal to have the tmux `usstyle` feature. Ghostty is auto-detected;
            -- alacritty needs TERM=alacritty (terminfo installed) + the `alacritty*:usstyle` terminal-features line
            -- in tmux.conf, otherwise tmux drops the undercurl and nothing is drawn. Underline fallback kept below.
            vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", { undercurl = true, sp = "#ea6962" })
            vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", { undercurl = true, sp = "#d8a657" })
            vim.api.nvim_set_hl(0, "DiagnosticUnderlineInfo", { undercurl = true, sp = "#7daea3" })
            vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", { undercurl = true, sp = "#a9b665" })

            -- plain underline fallback (was the default while undercurl did not work with macos alacritty + tmux)
            -- vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", { underline = true, sp = "#ea6962" })
            -- vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", { underline = true, sp = "#d8a657" })
            -- vim.api.nvim_set_hl(0, "DiagnosticUnderlineInfo", { underline = true, sp = "#7daea3" })
            -- vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", { underline = true, sp = "#a9b665" })

            -- Highlight groups: users can override via nvim_set_hl before this module loads.
            -- vim.api.nvim_set_hl(0, "JavaFormatOk", { default = true, link = "DiagnosticOk" })
            -- vim.api.nvim_set_hl(0, "JavaFormatOk", { default = true, link = "Grey" })
            -- vim.api.nvim_set_hl(0, "JavaFormatBad", { default = true, link = "DiagnosticError" })
            vim.api.nvim_set_hl(0, "JavaFormatOk", { fg = "#808080", bold = true, default = true, italic = true })
            vim.api.nvim_set_hl(0, "JavaFormatBad", { fg = "#ea6962", bold = true, default = true, italic = true })

            vim.api.nvim_set_hl(0, "GrayBold", { fg = "#808080", bold = true, default = true })

            vim.api.nvim_set_hl(0, "SnacksPickerInputBorder", { fg = "#a9b665" })
            vim.api.nvim_set_hl(0, "SnacksTitle", { link = "SnacksPickerTitle", default = true })

            -- 1. Define custom highlight group
            -- vim.api.nvim_set_hl(0, "YankHighlight", { bg = "#3b4261", fg = "#ff9e64" })

            -- #2B3339 #32302f
            vim.api.nvim_set_hl(0, "Visual", { fg = "NONE", bg = "#3c3836" })

            -- vim.api.nvim_set_hl(0, "JavaFormatOk", { fg = "#89b4fa", bold = true })
            -- vim.api.nvim_set_hl(0, "JavaFormatBad", { fg = "#f38ba8", bold = true })
        end,
    },
    -- {
    --     "sainnhe/everforest",
    --     lazy = false,
    --     optional = false,
    --     priority = 1000,
    --     enabled = false,
    --     config = function()
    --         -- Everforest is a green based color scheme; it's designed to be warm and soft in order to protect developers' eyes.
    --
    --         -- Optionally configure and load the colorscheme
    --         -- directly inside the plugin declaration.
    --
    --         -- Same setup as the gruvbox-material block, with the everforest hard/dark palette. everforest has no
    --         -- counterpart for gruvbox_material_foreground / enable_bold / visual / statusline_style.
    --         vim.g.everforest_enable_italic = true
    --         vim.g.everforest_background = "hard" -- `'hard'`, `'medium'`, `'soft'`
    --         vim.g.everforest_float_style = "blend" -- `'bright'`, `'dim'`, `'blend'`
    --         -- Darker than "hard" (bg0 #272e33, bg_dim #1e2326). bg0 = editor/float background, bg_dim = neo-tree
    --         -- and other dimmed surfaces; keep bg_dim below bg0 so the side panel stays distinguishable.
    --         vim.g.everforest_colors_override = {
    --             bg0 = { "#1e2326", "234" },
    --             bg_dim = { "#171b1d", "233" },
    --         }
    --         -- vim.cmd("set background=dark") -- dark,  light
    --         vim.cmd.colorscheme("everforest")
    --         vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "#a7c080", bold = true }) -- green (no GreenBold group here)
    --
    --         -- nvim-treesitter/nvim-treesitter-context: the theme paints it bg2 (#374145), too bright on the darker bg0
    --         -- override above. #293235 sits between that bg0 and CursorLine (bg1 #2e383c); the same value as CursorLine
    --         -- would merge the bar with the cursor line right below it.
    --         vim.api.nvim_set_hl(0, "TreesitterContext", { fg = "#d3c6aa", bg = "#293235" }) -- fg, as in the theme
    --
    --         -- unblevable/quick-scope
    --         vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = "#ff0000", underline = true, bold = true })
    --         vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = "aqua", underline = true, bold = true })
    --
    --         -- folke/flash.nvim (everforest ships no Flash* groups either, see the gruvbox-material block).
    --         vim.api.nvim_set_hl(0, "FlashBackdrop", { fg = "#7a8478" }) -- grey0: dim the rest, no italic
    --         vim.api.nvim_set_hl(0, "FlashMatch", { fg = "#d3c6aa", bg = "#4f5b58" }) -- fg on bg5: quiet box per match
    --         vim.api.nvim_set_hl(0, "FlashCurrent", { fg = "#1e2326", bg = "#83c092" }) -- bg0 on aqua: match under cursor
    --         vim.api.nvim_set_hl(
    --             0,
    --             "FlashLabel",
    --             { fg = "#1e2326", bg = "#fabd2f", bold = true, underline = true, italic = true }
    --         ) -- bg0 on the same bright yellow as the gruvbox block: jump labels (palette yellow #dbbc7f is the muted Substitute colour)
    --
    --         -- undercurl: see the INFO note in the gruvbox-material block
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", { undercurl = true, sp = "#e67e80" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", { undercurl = true, sp = "#dbbc7f" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineInfo", { undercurl = true, sp = "#7fbbb3" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", { undercurl = true, sp = "#a7c080" })
    --
    --         vim.api.nvim_set_hl(0, "JavaFormatOk", { fg = "#808080", bold = true, default = true, italic = true })
    --         vim.api.nvim_set_hl(0, "JavaFormatBad", { fg = "#e67e80", bold = true, default = true, italic = true })
    --
    --         vim.api.nvim_set_hl(0, "GrayBold", { fg = "#808080", bold = true, default = true })
    --
    --         vim.api.nvim_set_hl(0, "SnacksPickerInputBorder", { fg = "#a7c080" })
    --         vim.api.nvim_set_hl(0, "SnacksTitle", { link = "SnacksPickerTitle", default = true })
    --
    --         -- neutral grey instead of the theme's reddish bg_visual (#4c3743)
    --         vim.api.nvim_set_hl(0, "Visual", { fg = "NONE", bg = "#414b50" }) -- bg3
    --     end,
    -- },
    -- {
    --     "sainnhe/sonokai",
    --     lazy = false,
    --     optional = false,
    --     priority = 1000,
    --     config = function()
    --         vim.g.sonokai_enable_italic = true
    --         -- Available values:   `'default'`, `'atlantis'`, `'andromeda'`, `'shusia'`, `'maia'`, `'espresso'`
    --         vim.g.sonokai_style = "shusia" -- atlantis, andromeda, maia
    --         -- Same setup as the gruvbox-material block, with the sonokai shusia palette. sonokai has no counterpart
    --         -- for gruvbox_material_background / foreground / enable_bold / visual / statusline_style.
    --         vim.g.sonokai_float_style = "blend" -- `'bright'`, `'dim'`, `'blend'`
    --         -- Darker than stock shusia (bg0 #2d2a2e, bg_dim #211f21, black #1a181a): every level moves one step down.
    --         -- bg0 = editor/float background, bg_dim = neo-tree and other dimmed surfaces, black = window separators.
    --         vim.g.sonokai_colors_override = {
    --             bg0 = { "#211f21", "234" },
    --             bg_dim = { "#1a181a", "233" },
    --             black = { "#131113", "232" },
    --         }
    --         vim.cmd.colorscheme("sonokai")
    --         vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "#9ecd6f", bold = true }) -- green (no GreenBold group here)
    --
    --         -- nvim-treesitter/nvim-treesitter-context: the theme paints it bg2 (#3b383e), too bright on the darker bg0
    --         -- override above. #302e33 sits between that bg0 and CursorLine (bg1 #37343a); the same value as CursorLine
    --         -- would merge the bar with the cursor line right below it.
    --         vim.api.nvim_set_hl(0, "TreesitterContext", { fg = "#e3e1e4", bg = "#302e33" }) -- fg, as in the theme
    --
    --         -- unblevable/quick-scope
    --         vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = "#ff0000", underline = true, bold = true })
    --         vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = "aqua", underline = true, bold = true })
    --
    --         -- folke/flash.nvim (sonokai ships no Flash* groups either, see the gruvbox-material block). sonokai has no
    --         -- aqua, so the match under the cursor takes blue.
    --         vim.api.nvim_set_hl(0, "FlashBackdrop", { fg = "#605d68" }) -- grey_dim: dim the rest, no italic
    --         vim.api.nvim_set_hl(0, "FlashMatch", { fg = "#e3e1e4", bg = "#49464e" }) -- fg on bg4: quiet box per match
    --         vim.api.nvim_set_hl(0, "FlashCurrent", { fg = "#211f21", bg = "#7accd7" }) -- bg0 on blue: match under cursor
    --         vim.api.nvim_set_hl(
    --             0,
    --             "FlashLabel",
    --             { fg = "#211f21", bg = "#ffd866", bold = true, underline = true, italic = true }
    --         ) -- bg0 on a brighter yellow: jump labels (palette yellow #e5c463 is the Substitute colour)
    --
    --         -- undercurl: see the INFO note in the gruvbox-material block
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", { undercurl = true, sp = "#f85e84" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", { undercurl = true, sp = "#e5c463" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineInfo", { undercurl = true, sp = "#7accd7" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", { undercurl = true, sp = "#9ecd6f" })
    --
    --         vim.api.nvim_set_hl(0, "JavaFormatOk", { fg = "#808080", bold = true, default = true, italic = true })
    --         vim.api.nvim_set_hl(0, "JavaFormatBad", { fg = "#f85e84", bold = true, default = true, italic = true })
    --
    --         vim.api.nvim_set_hl(0, "GrayBold", { fg = "#808080", bold = true, default = true })
    --
    --         vim.api.nvim_set_hl(0, "SnacksPickerInputBorder", { fg = "#9ecd6f" })
    --         vim.api.nvim_set_hl(0, "SnacksTitle", { link = "SnacksPickerTitle", default = true })
    --
    --         -- bg3; the theme's own Visual is bg4 (#49464e), which the flash match boxes above use
    --         vim.api.nvim_set_hl(0, "Visual", { fg = "NONE", bg = "#423f46" })
    --     end,
    -- },
    -- {
    --     "navarasu/onedark.nvim",
    --     priority = 1000,
    --     optional = false,
    --     lazy = false,
    --     config = function()
    --         -- +++
    --         -- https://github.com/navarasu/onedark.nvim
    --         require("onedark").setup({
    --             style = "warmer", -- 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer', 'light'
    --         })
    --         -- Enable theme
    --         require("onedark").load()
    --     end,
    -- },
    -- {
    --     "tanvirtin/monokai.nvim",
    --     priority = 1000,
    --     optional = false,
    --     lazy = false,
    --     config = function()
    --         require("monokai").setup({})
    --         -- require("monokai").setup({ palette = require("monokai").pro })
    --         -- require("monokai").setup({ palette = require("monokai").soda })
    --         -- require("monokai").setup({ palette = require("monokai").ristretto })
    --     end,
    -- },
    -- { -- very dark, gool to work at night ;)
    --     "vague2k/vague.nvim",
    --     lazy = false, -- make sure we load this during startup if it is your main colorscheme
    --     priority = 1000, -- make sure to load this before all the other plugins
    --     config = function()
    --         require("vague").setup({
    --             -- optional configuration here
    --         })
    --         vim.cmd("colorscheme vague")
    --     end,
    -- },
}
