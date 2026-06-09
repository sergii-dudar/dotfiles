return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            spec = {
                { "<leader>j", group = "+rust" },
                { "<leader>jc", group = "+rust code/compile" },
            },
        },
    },
    {
        "mrcjkb/rustaceanvim",
        -- stylua: ignore
        keys = {
            { "<leader>jcc", function() vim.cmd.RustLsp { "flyCheck", "run" } end, desc = "Rust Compile" },
        },
        opts = {},
    },
    -- rust code actions & lsp based extensions
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                ["rust-analyzer"] = {
                    -- stylua: ignore
                    keys = {
                        { "<leader>cc", function()
                            local action_names = require("utils.lang.lsp.lsp-rust").code_action_auto_resolve_match_names
                            require("utils.lsp-util").code_action.resolve_context(action_names)
                        end,  desc = "Context Apply First Code Action [rust-analyzer]" }
                    },
                },
            },
        },
    },
}
