if not global.is_all and not vim.fs.root(vim.fn.getcwd(), { "http-client.env.json" }) then
    return {}
end

vim.filetype.add({
    extension = {
        ["http"] = "http",
    },
})
-- docs: https://github.com/dont-be-evil-company/kulala.nvim/tree/main/doc
--
-- Setup from scratch (mistweaverco/kulala.* went private 2026-09; andycowan/* is the recovery fork):
--  1. Plugin: "andycowan/kulala.nvim" below. It clones + compiles the tree-sitter grammar itself
--     (needs git, cc, curl) and downloads kulala-core from andycowan/kulala-core releases.
--  Install (build kulala-core once, any target cross-compiles from one machine):
--        >> git clone https://github.com/andycowan/kulala-core ~/tools/tests/kulala-core && cd ~/tools/tests/kulala-core
--        >> VERSION=0.37.0-andycowan.1 bun install --frozen-lockfile
--        >> export KULALA_CORE_DATA_DIR=/tmp/kulala-core-build
--  Linux:
--        >> bun run build:linux-x64      # -> packages/core/dist/kulala-core-linux-x86_64 (linux-arm64, darwin-x64 too), if not match, check package.json to proper platform cmd
--  MacOS:
--        # Corporate Mac only (Zscaler): Bun's own https can't verify anything without the corporate CAs.
--        # Without this every build:* fails with "Could not resolve curl binary. Tried download to ...".
--        mkdir -p ~/tools/certs
--        security find-certificate -a -p /Library/Keychains/System.keychain > ~/tools/certs/corp-ca-bundle.pem
--        export NODE_EXTRA_CA_CERTS=~/tools/certs/corp-ca-bundle.pem
--        >> bun run build:darwin-arm64   # -> packages/core/dist/kulala-core-darwin-arm64
--  3. Point the plugin at your binary (per machine) instead of downloading:
--        opts.kulala_core = {
--            path = vim.fn.expand("~/tools/tests/kulala-core/packages/core/dist/kulala-core-<os>-<arch>"),
--        }
--     Without it macOS uses the downloaded release; Linux has no release, so path is required there.
--  4. Check: open a .http file, :checkhealth kulala, <CR> on a request.

local kulala_core_bin = require("utils.constants").is_macos
        and vim.fn.expand("~/tools/tests/kulala-core/packages/core/dist/kulala-core-darwin-arm64")
    or vim.fn.expand("~/tools/tests/kulala-core/packages/core/dist/kulala-core-linux-x86_64")

return {
    {
        -- "dont-be-evil-company/kulala.nvim",
        -- "sergii-dudar/kulala.nvim",
        "andycowan/kulala.nvim",
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
            { "<leader>rw", function() require("utils.kulala-wget-util").copy_as_wget({ insecure = true }) end, desc = "Copy as wget (http)", ft = "http" },
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
        config = function(_, opts)
            local core_util = require("utils.kulala-core-util")
            -- Use the OS curl (trusts the OS certificate store, e.g. the bank CA) instead of the static curl
            -- kulala-core embeds/caches, whose OpenSSL trust store rejects internal hosts.
            vim.env.KULALA_CURL_PATH = vim.env.KULALA_CURL_PATH or vim.fn.exepath("curl")
            -- kulala-core is (re)downloaded asynchronously; "ready" fires once the binary is installed.
            -- macOS 27+ SIGKILLs the shipped binary until it is re-signed ad-hoc.
            require("kulala.api").on("ready", function()
                core_util.ensure_signed()
            end)
            require("kulala").setup(opts)
            -- Also cover an already-installed binary whose signature a macOS update invalidated.
            core_util.ensure_signed()
        end,
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
                max_response_size = 1024 * 1024, -- 1 MiB
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
            kulala_core = {
                path = kulala_core_bin,
            },
        },
    },
    {
        "nvim-treesitter/nvim-treesitter",
        opts = {
            ensure_installed = { "http", "graphql" },
        },
    },
}
