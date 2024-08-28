return {
    {
        "jay-babu/mason-null-ls.nvim",
        event = { "BufReadPre", "BufNewFile" },
        enabled = false,
        dependencies = {
            "williamboman/mason.nvim",
            "nvimtools/none-ls.nvim",
        },
        config = function()
            require("mason-null-ls").setup({
                ensure_installed = {
                    "google_java_format",
                    "checkstyle",
                    "black",
                    "isort",
                    "shellcheck",
                },
            })
        end,
    },
    {
        "nvimtools/none-ls.nvim",
        enabled = false,
        config = function()
            local null_ls = require("null-ls")
            null_ls.setup({
                sources = {
                    -- lua
                    null_ls.builtins.formatting.stylua,
                    -- js
                    null_ls.builtins.formatting.prettier,
                    null_ls.builtins.diagnostics.eslint_d,
                    -- rub
                    null_ls.builtins.diagnostics.rubocop,
                    null_ls.builtins.formatting.rubocop,
                    -- python
                    null_ls.builtins.formatting.black,
                    null_ls.builtins.formatting.isort,
                    -- java
                    null_ls.builtins.formatting.google_java_format,
                    null_ls.builtins.diagnostics.checkstyle.with({
                        extra_args = { "-c", "/google_checks.xml" }, -- or "/sun_checks.xml" or path to self written rules
                    }),
                },
            })

            vim.keymap.set("n", "<leader>gf", vim.lsp.buf.format, {})
        end,
    },
}