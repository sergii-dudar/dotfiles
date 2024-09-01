local a = {
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
        config = function()
            require("mason-lspconfig").setup({
                ensure_installed = {
                    "lua_ls",
                    -- "bashls",
                    -- "gradle_ls",
                    -- "helm_ls",
                    -- "java_language_server",
                    -- --"jdtls"
                    -- "jsonls",
                    -- "sqlls",
                    -- "harper_ls",
                    -- "jedi_language_server",
                },
            })
        end,
    },
    {
        "neovim/nvim-lspconfig",
        lazy = false,
        config = function()
            -- :Mason
            -- :LspInfo
            -- :h vim.lsp.buf
            -- :e --refresh bugger
            --local capabilities = require("cmp_nvim_lsp").default_capabilities()


            -- Manual autocompletion ('omnifunc'): <C-X><C-O>



            local lspconfig = require("lspconfig")
            --  lspconfig.tailwindcss.setup({
            --      capabilities = capabilities,
            --  })
            --  lspconfig.tsserver.setup({
            --      capabilities = capabilities,
            --  })
            --  lspconfig.solargraph.setup({
            --      capabilities = capabilities,
            --  })
            --  lspconfig.html.setup({
            --      capabilities = capabilities,
            --  })
            lspconfig.lua_ls.setup({
                --capabilities = capabilities,
            })
            --  lspconfig.java_language_server.setup({
            --      capabilities = capabilities,
            --  })
            --  lspconfig.bashls.setup({
            --      capabilities = capabilities,
            --  })


            vim.keymap.set("n", "<leader>lq", vim.lsp.buf.hover, {})
            vim.keymap.set("n", "<leader>lr", vim.lsp.buf.rename, {})
            vim.keymap.set("n", "<leader>la", vim.lsp.buf.code_action, {})

            vim.keymap.set("n", "<leader>ld", vim.lsp.buf.definition, {})
            vim.keymap.set("n", "<leader>li", vim.lsp.buf.implementation, {})
            vim.keymap.set("n", "<leader>ll", vim.lsp.buf.references, {})
        end,
    },
}
return {
}
