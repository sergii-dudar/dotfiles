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
