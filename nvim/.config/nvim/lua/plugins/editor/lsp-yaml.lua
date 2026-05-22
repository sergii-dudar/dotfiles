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
                        validate = false, -- temp disabled because of issues with schemas for application.yml [ Property ... is not allowed ]
                    },
                },
            },
        },
    },
}
