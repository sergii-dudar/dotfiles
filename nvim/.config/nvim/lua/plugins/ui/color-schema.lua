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
            vim.g.gruvbox_material_enable_italic = true
            vim.cmd.colorscheme("gruvbox-material")
        end,
    },
    -- {
    --     "sainnhe/everforest",
    --     lazy = false,
    --     optional = false,
    --     priority = 1000,
    --     config = function()
    --         -- Everforest is a green based color scheme; it's designed to be warm and soft in order to protect developers' eyes.
    --
    --         -- Optionally configure and load the colorscheme
    --         -- directly inside the plugin declaration.
    --
    --         vim.g.everforest_enable_italic = true
    --         vim.g.everforest_background = "hard" -- `'hard'`, `'medium'`, `'soft'`
    --         -- vim.cmd("set background=dark") -- dark,  light
    --         vim.cmd.colorscheme("everforest")
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
    --         vim.cmd.colorscheme("sonokai")
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