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

            -- saghen/blink.cmp: the menu follows Pmenu (bg3 #3c3836), a grey slab on the darker bg0 override above,
            -- next to a documentation window that already sits on NormalFloat. Menu, documentation and signature
            -- now all look like the other floats (float_style = "blend"): editor background, grey border.
            vim.api.nvim_set_hl(0, "BlinkCmpMenu", { link = "NormalFloat" })
            vim.api.nvim_set_hl(0, "BlinkCmpMenuBorder", { link = "FloatBorder" })
            vim.api.nvim_set_hl(0, "BlinkCmpDocBorder", { link = "FloatBorder" })
            vim.api.nvim_set_hl(0, "BlinkCmpDocSeparator", { link = "FloatBorder" })
            vim.api.nvim_set_hl(0, "BlinkCmpSignatureHelpBorder", { link = "FloatBorder" })
            -- these follow PmenuKind / PmenuExtra, which carry the bg3 background and would leave lighter patches
            vim.api.nvim_set_hl(0, "BlinkCmpKind", { fg = "#a9b665" }) -- green
            vim.api.nvim_set_hl(0, "BlinkCmpSource", { fg = "#a89984" }) -- grey2
            vim.api.nvim_set_hl(0, "BlinkCmpLabelDetail", { fg = "#a89984" })
            vim.api.nvim_set_hl(0, "BlinkCmpLabelDescription", { fg = "#a89984" })
            vim.api.nvim_set_hl(0, "BlinkCmpLabelDeprecated", { fg = "#a89984" })

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
    --         -- Same setup as the gruvbox-material block, with the onedark warmer palette.
    --         require("onedark").setup({
    --             style = "warmer", -- 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer', 'light'
    --             -- Darker than stock warmer (bg0 #232326, bg_d #1b1c1e). bg0 = editor background, bg_d = neo-tree;
    --             -- keep bg_d below bg0 so the side panel stays distinguishable.
    --             colors = {
    --                 bg0 = "#1b1c1e",
    --                 bg_d = "#131416",
    --             },
    --         })
    --         -- Enable theme
    --         require("onedark").load()
    --         -- floats on the editor background, what float_style = "blend" does in gruvbox-material (stock: bg1 #2c2d31)
    --         vim.api.nvim_set_hl(0, "NormalFloat", { fg = "#a7aab0", bg = "#1b1c1e" })
    --         vim.api.nvim_set_hl(0, "FloatBorder", { fg = "#5a5b5e", bg = "#1b1c1e" }) -- grey, as in the theme
    --         vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "#8fb573", bold = true }) -- green
    --
    --         -- nvim-treesitter/nvim-treesitter-context: the theme has no group for it, so it would follow NormalFloat and
    --         -- vanish into the editor background. #252629 sits between bg0 and CursorLine (bg1 #2c2d31); the same value
    --         -- as CursorLine would merge the bar with the cursor line right below it.
    --         vim.api.nvim_set_hl(0, "TreesitterContext", { fg = "#a7aab0", bg = "#252629" })
    --
    --         -- unblevable/quick-scope
    --         vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = "#ff0000", underline = true, bold = true })
    --         vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = "aqua", underline = true, bold = true })
    --
    --         -- folke/flash.nvim (onedark ships no Flash* groups either, see the gruvbox-material block). The palette has
    --         -- nothing between bg3 (the Visual colour) and grey, so the match box is one step above bg3.
    --         vim.api.nvim_set_hl(0, "FlashBackdrop", { fg = "#5a5b5e" }) -- grey: dim the rest, no italic
    --         vim.api.nvim_set_hl(0, "FlashMatch", { fg = "#a7aab0", bg = "#45464b" }) -- fg: quiet box per match
    --         vim.api.nvim_set_hl(0, "FlashCurrent", { fg = "#1b1c1e", bg = "#51a8b3" }) -- bg0 on cyan: match under cursor
    --         vim.api.nvim_set_hl(
    --             0,
    --             "FlashLabel",
    --             { fg = "#1b1c1e", bg = "#fabd2f", bold = true, underline = true, italic = true }
    --         ) -- bg0 on the same bright yellow as the gruvbox block: jump labels (palette yellow #dbb671 is muted)
    --
    --         -- undercurl: see the INFO note in the gruvbox-material block
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", { undercurl = true, sp = "#de5d68" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", { undercurl = true, sp = "#dbb671" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineInfo", { undercurl = true, sp = "#57a5e5" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", { undercurl = true, sp = "#8fb573" })
    --
    --         vim.api.nvim_set_hl(0, "JavaFormatOk", { fg = "#808080", bold = true, default = true, italic = true })
    --         vim.api.nvim_set_hl(0, "JavaFormatBad", { fg = "#de5d68", bold = true, default = true, italic = true })
    --
    --         vim.api.nvim_set_hl(0, "GrayBold", { fg = "#808080", bold = true, default = true })
    --
    --         vim.api.nvim_set_hl(0, "SnacksPickerInputBorder", { fg = "#8fb573" })
    --         vim.api.nvim_set_hl(0, "SnacksTitle", { link = "SnacksPickerTitle", default = true })
    --
    --         vim.api.nvim_set_hl(0, "Visual", { fg = "NONE", bg = "#37383d" }) -- bg3, the theme's own Visual colour
    --     end,
    -- },
    -- {
    --     "tanvirtin/monokai.nvim",
    --     priority = 1000,
    --     optional = false,
    --     lazy = false,
    --     config = function()
    --         -- Same setup as the gruvbox-material block, with the monokai classic palette.
    --         local monokai = require("monokai")
    --         monokai.setup({
    --             -- Darker than stock classic: base2 (#26292C) is the editor, line number and sign column background.
    --             palette = vim.tbl_extend("force", monokai.classic, { base2 = "#1c1e20" }),
    --         })
    --         -- require("monokai").setup({})
    --         -- require("monokai").setup({ palette = require("monokai").pro })
    --         -- require("monokai").setup({ palette = require("monokai").soda })
    --         -- require("monokai").setup({ palette = require("monokai").ristretto })
    --         -- floats on the editor background, what float_style = "blend" does in gruvbox-material (stock: base1)
    --         vim.api.nvim_set_hl(0, "NormalFloat", { bg = "#1c1e20" })
    --         -- window separators: the theme only sets VertSplit (brown), so WinSeparator stays on the default white
    --         vim.api.nvim_set_hl(0, "WinSeparator", { fg = "#504945" })
    --         -- neo-tree: the theme has no groups for it; darker side panel, what bg_dim gives in gruvbox-material
    --         vim.api.nvim_set_hl(0, "NeoTreeNormal", { fg = "#f8f8f0", bg = "#141517" })
    --         vim.api.nvim_set_hl(0, "NeoTreeNormalNC", { fg = "#f8f8f0", bg = "#141517" })
    --         vim.api.nvim_set_hl(0, "NeoTreeEndOfBuffer", { fg = "#141517", bg = "#141517" })
    --         vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "#a6e22e", bold = true }) -- green
    --
    --         -- nvim-treesitter/nvim-treesitter-context: the theme has no group for it, so it would follow NormalFloat and
    --         -- vanish into the editor background. base1 (#272a30) sits between base2 and CursorLine (base3 #2E323C); the
    --         -- same value as CursorLine would merge the bar with the cursor line right below it.
    --         vim.api.nvim_set_hl(0, "TreesitterContext", { fg = "#f8f8f0", bg = "#272a30" })
    --
    --         -- unblevable/quick-scope
    --         vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = "#ff0000", underline = true, bold = true })
    --         vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = "aqua", underline = true, bold = true })
    --
    --         -- folke/flash.nvim (monokai ships no Flash* groups either, see the gruvbox-material block). Yellow is the
    --         -- Search colour here, so the jump labels take orange.
    --         vim.api.nvim_set_hl(0, "FlashBackdrop", { fg = "#8F908A" }) -- grey: dim the rest, no italic
    --         vim.api.nvim_set_hl(0, "FlashMatch", { fg = "#f8f8f0", bg = "#4d5154" }) -- white on base5: quiet box per match
    --         vim.api.nvim_set_hl(0, "FlashCurrent", { fg = "#1c1e20", bg = "#66d9ef" }) -- base2 on aqua: match under cursor
    --         vim.api.nvim_set_hl(
    --             0,
    --             "FlashLabel",
    --             { fg = "#1c1e20", bg = "#fd971f", bold = true, underline = true, italic = true }
    --         ) -- base2 on orange: jump labels
    --
    --         -- undercurl: see the INFO note in the gruvbox-material block
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", { undercurl = true, sp = "#e95678" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", { undercurl = true, sp = "#e6db74" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineInfo", { undercurl = true, sp = "#66d9ef" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", { undercurl = true, sp = "#a6e22e" })
    --
    --         vim.api.nvim_set_hl(0, "JavaFormatOk", { fg = "#808080", bold = true, default = true, italic = true })
    --         vim.api.nvim_set_hl(0, "JavaFormatBad", { fg = "#e95678", bold = true, default = true, italic = true })
    --
    --         vim.api.nvim_set_hl(0, "GrayBold", { fg = "#808080", bold = true, default = true })
    --
    --         vim.api.nvim_set_hl(0, "SnacksPickerInputBorder", { fg = "#a6e22e" })
    --         vim.api.nvim_set_hl(0, "SnacksTitle", { link = "SnacksPickerTitle", default = true })
    --
    --         vim.api.nvim_set_hl(0, "Visual", { fg = "NONE", bg = "#333842" }) -- base4, the theme's own Visual colour
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
    --         -- Same setup as the gruvbox-material block, with the vague palette. The stock background (#141415) is
    --         -- already darker than the gruvbox one, so it stays.
    --         -- floats on the editor background, what float_style = "blend" does in gruvbox-material (stock: inactiveBg)
    --         vim.api.nvim_set_hl(0, "NormalFloat", { fg = "#cdcdcd", bg = "#141415" })
    --         -- neo-tree: the theme leaves it on the editor background; darker side panel, what bg_dim gives in gruvbox-material
    --         vim.api.nvim_set_hl(0, "NeoTreeNormal", { fg = "#cdcdcd", bg = "#0d0d0e" })
    --         vim.api.nvim_set_hl(0, "NeoTreeNormalNC", { fg = "#cdcdcd", bg = "#0d0d0e" })
    --         vim.api.nvim_set_hl(0, "NeoTreeEndOfBuffer", { fg = "#0d0d0e", bg = "#0d0d0e" })
    --         vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "#7fa563", bold = true }) -- plus (green)
    --
    --         -- nvim-treesitter/nvim-treesitter-context: the theme has no group for it, so it would follow NormalFloat and
    --         -- vanish into the editor background. inactiveBg (#1c1c24) sits between bg and CursorLine (line #252530); the
    --         -- same value as CursorLine would merge the bar with the cursor line right below it.
    --         vim.api.nvim_set_hl(0, "TreesitterContext", { fg = "#cdcdcd", bg = "#1c1c24" })
    --
    --         -- unblevable/quick-scope
    --         vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = "#ff0000", underline = true, bold = true })
    --         vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = "aqua", underline = true, bold = true })
    --
    --         -- folke/flash.nvim (vague ships no Flash* groups either, see the gruvbox-material block).
    --         vim.api.nvim_set_hl(0, "FlashBackdrop", { fg = "#606079" }) -- comment: dim the rest, no italic
    --         vim.api.nvim_set_hl(0, "FlashMatch", { fg = "#cdcdcd", bg = "#405065" }) -- fg on search: quiet box per match
    --         vim.api.nvim_set_hl(0, "FlashCurrent", { fg = "#141415", bg = "#b4d4cf" }) -- bg on builtin: match under cursor
    --         vim.api.nvim_set_hl(
    --             0,
    --             "FlashLabel",
    --             { fg = "#141415", bg = "#f3be7c", bold = true, underline = true, italic = true }
    --         ) -- bg on warning, the brightest yellow of the palette: jump labels
    --
    --         -- undercurl: see the INFO note in the gruvbox-material block
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", { undercurl = true, sp = "#d8647e" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", { undercurl = true, sp = "#f3be7c" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineInfo", { undercurl = true, sp = "#7e98e8" })
    --         vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", { undercurl = true, sp = "#7fa563" })
    --
    --         vim.api.nvim_set_hl(0, "JavaFormatOk", { fg = "#808080", bold = true, default = true, italic = true })
    --         vim.api.nvim_set_hl(0, "JavaFormatBad", { fg = "#d8647e", bold = true, default = true, italic = true })
    --
    --         vim.api.nvim_set_hl(0, "GrayBold", { fg = "#808080", bold = true, default = true })
    --
    --         vim.api.nvim_set_hl(0, "SnacksPickerInputBorder", { fg = "#7fa563" })
    --         vim.api.nvim_set_hl(0, "SnacksTitle", { link = "SnacksPickerTitle", default = true })
    --
    --         vim.api.nvim_set_hl(0, "Visual", { fg = "NONE", bg = "#333738" }) -- visual, the theme's own Visual colour
    --     end,
    -- },
}