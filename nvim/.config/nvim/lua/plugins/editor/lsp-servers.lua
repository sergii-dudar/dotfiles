return {
    -- ###### Makefile / autotools #######
    -- autotools-language-server: LSP for Makefile, automake, autoconf
    -- gradle-language-server
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                autotools_ls = {},
                -- gradle_ls = {},
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
