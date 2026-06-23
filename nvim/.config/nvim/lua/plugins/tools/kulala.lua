-- TODO: load conditionally on http project
vim.filetype.add({
    extension = {
        ["http"] = "http",
    },
})
-- docs: https://github.com/mistweaverco/kulala.nvim/tree/main/doc
return {
    {
        "mistweaverco/kulala.nvim",
        -- tag = "v6.14.0",
        ft = { "http", "rest" },
        -- ft = { "http", "rest", "javascript", "lua" },
        -- stylua: ignore
        keys = {
            { "<leader>R", "", desc = "+Rest" },
            { "<leader>Rb", function() require('kulala').scratchpad() end, desc = "Open scratchpad (http)" },
            { "<leader>Rr", function() require('kulala').replay() end, desc = "Replay the last request (http)" },
            { "<leader>r", "", desc = "+Rest", ft = {"http", "json"} },
            { "<leader>rr", function() require('kulala').run() end, desc = "Send the request (http)", ft = "http" },
            { "<CR>", function() require('kulala').run() end, desc = "Send the request (http)", ft = "http" },
            { "<leader>rl", function() require('kulala').replay() end, desc = "Replay the last request (http)", ft = {"http", "json"} },

            { "<leader>rc", function() require('kulala').copy() end, desc = "Copy as cURL (http)", ft = "http" },
            { "<leader>rC", function() require('kulala').from_curl() end, desc = "Paste from curl (http)", ft = "http" },
            { "<leader>re", function() require('kulala').set_selected_env() end, desc = "Set environment (http)", ft = "http" },
            { "<leader>rg", function() require('kulala').download_graphql_schema() end, desc = "Download GraphQL schema (http)", ft = "http", },
            { "<leader>ri", function() require('kulala').inspect() end, desc = "Inspect current request (http)", ft = "http" },

            { "<leader>]", function() require('kulala').jump_next() end, desc = "Jump to next request (http)", ft = "http" },
            { "<leader>[", function() require('kulala').jump_prev() end, desc = "Jump to previous request (http)", ft = "http" },
            -- { "<leader>rq", function() require('kulala').close() end, desc = "Close window", ft = "http" },
            -- { "<leader>rS", function() require('kulala').show_stats() end, desc = "Show stats", ft = "http" },
            -- { "<leader>rt", function() require('kulala').toggle_view() end, desc = "Toggle headers/body", ft = "http" },
            --
            { "<leader>H", function() require("kulala.ui").show_headers() end, desc = "Show [H]eaders (http result)", ft = "http" },
            { "<leader>B", function() require("kulala.ui").show_body() end, desc = "Show [B]ody (http result)", ft = "http" },
            { "<leader>A", function() require("kulala.ui").show_headers_body() end, desc = "Show [A]ll (http result)", ft = "http" },
            { "<leader>V", function() require("kulala.ui").show_verbose() end, desc = "Show [V]erbose (http result)", ft = "http" },
            -- 
            -- ["Show script output"] = { "O", function() require("kulala.ui").show_script_output() end, },
            -- ["Show report"] = { "R", function() require("kulala.ui").show_report() end, },
            -- ["Show filter"] = { "F", function() require("kulala.ui").toggle_filter() end },
            -- 
            -- ["Send WS message"] = { "<S-CR>", function() require("kulala.cmd.websocket").send() end, mode = { "n", "v" }, },
            -- ["Interrupt requests"] = { "<C-c>", function() require("kulala.cmd.websocket").close() end, desc = "also: CLose WS connection" },
            -- 
            -- ["Next response"] = { "]", function() require("kulala.ui").show_next() end, },
            -- ["Previous response"] = { "[", function() require("kulala.ui").show_previous() end, },
            -- ["Jump to response"] = { "<CR>", function() require("kulala.ui").jump_to_response() end, desc = "also: Send WS message for WS connections" },
            -- 
            -- ["Clear responses history"] = { "X", function() require("kulala.ui").clear_responses_history() end, },
            -- 
            -- ["Show help"] = { "?", function() require("kulala.ui").show_help() end, },
            -- ["Show news"] = { "g?", function() require("kulala.ui").show_news() end, },
            -- 
            -- ["Toggle split/float"] = { "|", function() require("kulala.ui").toggle_display_mode() end, prefix = false, },
            -- ["Close"] = { "q", function() require("kulala.ui").close_kulala_buffer() end, },
        },
        opts = {
            debug = false,
            default_env = "uat",
            custom_dynamic_variables = {
                ["$cwd"] = function()
                    return vim.fn.getcwd()
                end,
                ["$env"] = function()
                    return require("kulala").get_selected_env()
                end,
            },
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
                enable = true,
                keymaps = false, -- disabled by default, as Kulala relies on default Neovim LSP keymaps
                ---filetypes to attach Kulala LSP to
                ---@type string[]
                filetypes = {
                    "http",
                    "rest",
                    -- "javascript",
                    -- "typescript",
                    -- "lua",
                },
                on_attach = function(_, bufnr)
                    vim.schedule(function()
                        -- remove mapping to pick my default mapping in keymaps.lua
                        pcall(vim.keymap.del, "n", "K", { buffer = bufnr })
                    end)
                end,
            },
            global_keymaps = false,
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
