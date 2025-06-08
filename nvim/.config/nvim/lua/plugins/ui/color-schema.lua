return {
    -- ##############  FAVORITE ###################
    {
        "catppuccin/nvim",
        name = "catppuccin",
        priority = 1000,
        optional = false,
        lazy = false,
        opts = {
            custom_highlights = function()
                return {
                    Visual = { bg = "#373267" },
                }
            end,
        },
        --[[opts = {
          term_colors = true,
          transparent_background = true
      }]]
    },
    -- {
    --     "LazyVim/LazyVim",
    --     opts = {
    --         -- colorscheme = "catppuccin-mocha",
    --         -- colorscheme = "gruvbox",
    --     },
    -- },
    -- ############## MOVE EYE (Monokai, Gruvbox, Everforest) ###################
    {
        "sainnhe/everforest",
        lazy = false,
        optional = false,
        priority = 1000,
        config = function()
            -- Everforest is a green based color scheme; it's designed to be warm and soft in order to protect developers' eyes.

            -- Optionally configure and load the colorscheme
            -- directly inside the plugin declaration.

            vim.g.everforest_enable_italic = true
            vim.g.everforest_background = "hard" -- `'hard'`, `'medium'`, `'soft'`
            -- vim.cmd("set background=dark") -- dark,  light
            vim.cmd.colorscheme("everforest")
        end,
    },
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
    --     "ellisonleao/gruvbox.nvim",
    --     priority = 1000,
    --     optional = false,
    --     lazy = false,
    --     config = function()
    --         require("gruvbox").setup({
    --             contrast = "hard", -- can be "hard", "soft" or empty string
    --             -- overrides = {
    --             --     ["@lsp.type.method"] = { bg = "#000000" },
    --             --     ["@comment.lua"] = { bg = "#000000" },
    --             -- },
    --         })
    --
    --         -- vim.o.background = "dark" -- or "light" for light mod
    --         vim.cmd.colorscheme("gruvbox")
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
}