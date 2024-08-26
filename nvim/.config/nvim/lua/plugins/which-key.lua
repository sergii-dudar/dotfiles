return
{
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
    },
    keys = {
        {
            "<leader>?",
            function()
                require("which-key").show({ global = false })
            end,
            desc = "Buffer Local Keymaps (which-key)",
        },
    },
        opts = {
            defaults = {},
            spec = {
                {
                    mode = { "n", "v" },
                   { "<leader>h", group = "harpoon" },
                   { "<leader>n", group = "neo-tree" },
                   { "<leader>s", group = "session" },
                   { "<leader>z", group = "zen-mode" },


                   -- { "<leader><tab>", group = "tabs" },
                   -- { "<leader>c", group = "code" },
                   -- { "<leader>f", group = "file/find" },
                   -- { "<leader>g", group = "git" },
                   -- { "<leader>gh", group = "hunks" },
                   -- { "<leader>q", group = "quit/session" },
                   -- { "<leader>s", group = "search" },
                   -- { "<leader>u", group = "ui", icon = { icon = "󰙵 ", color = "cyan" } },
                   -- { "<leader>x", group = "diagnostics/quickfix", icon = { icon = "󱖫 ", color = "green" } },
                   -- { "[", group = "prev" },
                   -- { "]", group = "next" },
                   -- { "g", group = "goto" },
                   -- { "gs", group = "surround" },
                   -- { "z", group = "fold" },
                   -- {
                   --     "<leader>b",
                   --     group = "buffer",
                   --     expand = function()
                   --         return require("which-key.extras").expand.buf()
                   --     end,
                   -- },
                   -- {
                   --     "<leader>w",
                   --     group = "windows",
                   --     proxy = "<c-w>",
                   --     expand = function()
                   --         return require("which-key.extras").expand.win()
                   --     end,
                   -- },
                   -- -- better descriptions
                   -- { "gx", desc = "Open with system app" },
                },
            },
        },
    plugins = {
        marks = true, -- shows a list of your marks on ' and `
        registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
        -- the presets plugin, adds help for a bunch of default keybindings in Neovim
        -- No actual key bindings are created
        spelling = {
            enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
            suggestions = 20, -- how many suggestions should be shown in the list?
        },
        presets = {
            operators = true, -- adds help for operators like d, y, ...
            motions = true, -- adds help for motions
            text_objects = true, -- help for text objects triggered after entering an operator
            windows = true, -- default bindings on <c-w>
            nav = true, -- misc bindings to work with windows
            z = true, -- bindings for folds, spelling and others prefixed with z
            g = true, -- bindings for prefixed with g
        },
        operators = { gc = "Comments" },
        icons = {
            breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
            separator = "➜", -- symbol used between a key and it's label
            group = "+", -- symbol prepended to a group
        },
        popup_mappings = {
            scroll_down = "<c-d>", -- binding to scroll down inside the popup
            scroll_up = "<c-u>",   -- binding to scroll up inside the popup
        },
        window = {
            border = "none",          -- none, single, double, shadow
            position = "bottom",      -- bottom, top
            margin = { 1, 0, 1, 0 },  -- extra window margin [top, right, bottom, left]. When between 0 and 1, will be treated as a percentage of the screen size.
            padding = { 1, 2, 1, 2 }, -- extra window padding [top, right, bottom, left]
            winblend = 0,             -- value between 0-100 0 for fully opaque and 100 for fully transparent
            zindex = 1000,            -- positive value to position WhichKey above other floating windows.
        },
    }
}

--[[
return {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts_extend = { "spec" },
    opts = {
        defaults = {},
        spec = {
            {
                mode = { "n", "v" },
                { "<leader><tab>", group = "tabs" },
                { "<leader>c", group = "code" },
                { "<leader>f", group = "file/find" },
                { "<leader>g", group = "git" },
                { "<leader>gh", group = "hunks" },
                { "<leader>q", group = "quit/session" },
                { "<leader>s", group = "search" },
                { "<leader>u", group = "ui", icon = { icon = "󰙵 ", color = "cyan" } },
                { "<leader>x", group = "diagnostics/quickfix", icon = { icon = "󱖫 ", color = "green" } },
                { "[", group = "prev" },
                { "]", group = "next" },
                { "g", group = "goto" },
                { "gs", group = "surround" },
                { "z", group = "fold" },
                {
                    "<leader>b",
                    group = "buffer",
                    expand = function()
                        return require("which-key.extras").expand.buf()
                    end,
                },
                {
                    "<leader>w",
                    group = "windows",
                    proxy = "<c-w>",
                    expand = function()
                        return require("which-key.extras").expand.win()
                    end,
                },
                -- better descriptions
                { "gx", desc = "Open with system app" },
            },
        },
    },
    keys = {
        {
            "<leader>?",
            function()
                require("which-key").show({ global = false })
            end,
            desc = "Buffer Keymaps (which-key)",
        },
        {
            "<c-w><space>",
            function()
                require("which-key").show({ keys = "<c-w>", loop = true })
            end,
            desc = "Window Hydra Mode (which-key)",
        },
    },
    config = function(_, opts)
        local wk = require("which-key")
        wk.setup(opts)
        if not vim.tbl_isempty(opts.defaults) then
            LazyVim.warn("which-key: opts.defaults is deprecated. Please use opts.spec instead.")
            wk.register(opts.defaults)
        end
    end,
}]]