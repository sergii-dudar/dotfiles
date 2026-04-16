vim.filetype.add({
    extension = {
        ["http"] = "http",
    },
})
return {
    {
        "mistweaverco/kulala.nvim",
        ft = "http",
        -- stylua: ignore
        keys = {
            { "<leader>R", "", desc = "+Rest" },
            { "<leader>Rb", function() require('kulala').scratchpad() end, desc = "Open scratchpad" },
            { "<leader>Rr", function() require('kulala').replay() end, desc = "Replay the last request" },
            { "<leader>r", "", desc = "+Rest", ft = "http" },
            { "<leader>rr", function() require('kulala').run() end, desc = "Send the request", ft = "http" },
            { "<leader>rl", function() require('kulala').replay() end, desc = "Replay the last request", ft = "http" },

            -- already defined as global_keymaps by default
            -- https://neovim.getkulala.net/docs/getting-started/default-keymaps
            -- { "<leader>rc", function() require('kulala').copy() end, desc = "Copy as cURL", ft = "http" },
            -- { "<leader>rC", function() require('kulala').from_curl() end, desc = "Paste from curl", ft = "http" },
            -- { "<leader>re", function() require('kulala').set_selected_env() end, desc = "Set environment", ft = "http" },
            -- { "<leader>rg", function() require('kulala').download_graphql_schema() end, desc = "Download GraphQL schema", ft = "http", },
            -- { "<leader>ri", function() require('kulala').inspect() end, desc = "Inspect current request", ft = "http" },
            -- { "<leader>rn", function() require('kulala').jump_next() end, desc = "Jump to next request", ft = "http" },
            -- { "<leader>rp", function() require('kulala').jump_prev() end, desc = "Jump to previous request", ft = "http" },
            -- { "<leader>rq", function() require('kulala').close() end, desc = "Close window", ft = "http" },
            -- { "<leader>rS", function() require('kulala').show_stats() end, desc = "Show stats", ft = "http" },
            -- { "<leader>rt", function() require('kulala').toggle_view() end, desc = "Toggle headers/body", ft = "http" },
        },
        opts = {
            ui = {
                winbar = true,
                pickers = {
                    snacks = {
                        layout = require("plugins.snacks.configs.layouts").custom_default,
                    },
                },
                icons = {
                    inlay = {
                        loading = "󰔛",
                        done = "󰄲",
                        error = " ",
                    },
                    lualine = "󱜿",
                    textHighlight = "WarningMsg", -- highlight group for request elapsed time
                    loadingHighlight = "Normal",
                    doneHighlight = "String",
                    errorHighlight = "ErrorMsg",
                },
            },
            lsp = {
                on_attach = function(_, bufnr)
                    vim.schedule(function()
                        -- remove mapping to pick my default mapping in keymaps.lua
                        pcall(vim.keymap.del, "n", "K", { buffer = bufnr })
                    end)
                end,
            },
            global_keymaps = true,
            global_keymaps_prefix = "<leader>r",
        },
    },
    {
        "nvim-treesitter/nvim-treesitter",
        opts = {
            ensure_installed = { "http", "graphql" },
        },
    },
}
