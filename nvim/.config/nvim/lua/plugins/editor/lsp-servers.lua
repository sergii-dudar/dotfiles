return {
    -- ###### Makefile / autotools #######
    -- autotools-language-server: LSP for Makefile, automake, autoconf
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                autotools_ls = {},
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
