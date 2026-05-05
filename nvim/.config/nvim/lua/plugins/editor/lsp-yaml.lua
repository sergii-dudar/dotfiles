return {
    "neovim/nvim-lspconfig",
    opts = {
        servers = {
            yamlls = {
                settings = {
                    yaml = {
                        format = {
                            printWidth = 300,
                        },
                    },
                },
            },
        },
    },
}
