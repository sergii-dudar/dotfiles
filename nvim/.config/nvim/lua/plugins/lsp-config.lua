return {
    {
        "williamboman/mason.nvim",
        lazy = false,
        config = function()
            require("mason").setup()
        end,
    },
    {
        "williamboman/mason-lspconfig.nvim",
        lazy = false,
        opts = {
            auto_install = true,
        },
    },
    {
        "neovim/nvim-lspconfig",
        lazy = false,
        config = function()
            local capabilities = require('cmp_nvim_lsp').default_capabilities()

            local lspconfig = require("lspconfig")
            lspconfig.tailwindcss.setup({
                capabilities = capabilities
            })
            lspconfig.tsserver.setup({
                capabilities = capabilities
            })
            lspconfig.solargraph.setup({
                capabilities = capabilities
            })
            lspconfig.html.setup({
                capabilities = capabilities
            })
            lspconfig.lua_ls.setup({
                capabilities = capabilities
            })

            --vim.keymap.set("n", "K", vim.lsp.buf.hover, {})
            vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, {})
            vim.keymap.set("n", "<leader>la", vim.lsp.buf.code_action, {})

            vim.keymap.set("n", "<leader>ld", vim.lsp.buf.definition, {})
            vim.keymap.set("n", "<leader>li", vim.lsp.buf.implementation, {})
            vim.keymap.set("n", "<leader>ll", vim.lsp.buf.references, {})
        end,
    },
}