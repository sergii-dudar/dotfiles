return {
    -- ###### Makefile / autotools #######
    -- autotools-language-server: LSP for Makefile, automake, autoconf
    -- gradle-language-server
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                autotools_ls = {},
                gradle_ls = {
                    cmd = { global.dotfiles_path("/bin/java/gradle-language-server-stdio") },
                },
                omnisharp = {
                    -- stylua: ignore
                    keys = {
                        { "gd", function() require("omnisharp_extended").lsp_definitions() end, desc = "Goto Definition", },
                        { "gr", function() require('omnisharp_extended').lsp_references() end, desc = "References", nowait = true },
                        { "gI", function() require('omnisharp_extended').lsp_implementation() end, desc = "Goto Implementation" },
                        { "gy", function() require('omnisharp_extended').lsp_type_definition() end, desc = "Goto T[y]pe Definition" },
                    },
                },
                yamlls = {
                    settings = {
                        yaml = {
                            format = {
                                printWidth = 300,
                            },
                            validate = true, -- temp disabled because of issues with schemas for application.yml [ Property ... is not allowed ]
                        },
                    },
                },
            },
            setup = {
                -- Keep gradle_ls out of mason-lspconfig automatic enablement.
                -- Mason currently rewrites gradle_ls.cmd back to its broken
                -- `gradle-language-server` wrapper, so this server must be
                -- configured and enabled manually with the stdio adapter above.
                -- Returning true tells LazyVim this one server is handled here;
                -- other LSP servers continue through the normal setup path.
                gradle_ls = function(_, opts)
                    vim.lsp.config("gradle_ls", opts)
                    vim.lsp.enable("gradle_ls")
                    return true
                end,
            },
        },
    },
    -- ###### openapi #######
    -- openapi-language-server is started conditionally from ftplugin/yaml.lua
    {
        -- "armsnyder/openapi-language-server",
        "sergii-dudar/openapi-language-server",
        build = "go install .",
        ft = { "yaml" },
        lazy = true,
    },
}
