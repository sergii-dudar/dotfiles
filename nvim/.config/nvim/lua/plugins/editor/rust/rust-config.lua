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
    -- rust keymaps, code actions & lsp based extensions.
    -- rust-analyzer is owned by rustaceanvim (not nvim-lspconfig), so buffer-local keymaps are
    -- registered through `server.on_attach`. The previous on_attach (from LazyVim's rust extra)
    -- is chained so its keymaps and `default_settings` are preserved.
    {
        "mrcjkb/rustaceanvim",
        -- stylua: ignore
        keys = {
            { "<leader>jcc", function() vim.cmd.RustLsp { "flyCheck", "run" } end, desc = "Rust Compile" },
        },
        opts = function(_, opts)
            opts.server = opts.server or {}
            local prev_on_attach = opts.server.on_attach
            opts.server.on_attach = function(client, bufnr)
                if prev_on_attach then
                    prev_on_attach(client, bufnr)
                end
                vim.keymap.set("n", "<leader>cc", function()
                    local action_names = require("utils.lang.rust.lsp-rust").code_action_auto_resolve_match_names
                    require("utils.lsp-util").code_action.resolve_context(action_names)
                end, { buffer = bufnr, desc = "Context Apply First Code Action [rust-analyzer]" })
            end
            return opts
        end,
    },
}
