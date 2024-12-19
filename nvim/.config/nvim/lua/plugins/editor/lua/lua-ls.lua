return {
    "neovim/nvim-lspconfig",
    opts = {
        servers = {
            lua_ls = {
                settings = {
                    Lua = {
                        diagnostics = {
                            enable = true,
                            globals = { "vim", "use", "awesome", "client", "root", "screen", "mouse" },
                            disable = { "lowercase-global" },
                        },
                        workspace = {
                            checkThirdParty = false,
                            library = {
                                "/usr/share/nvim/runtime/lua",
                                "/usr/share/nvim/runtime/lua/lsp",
                                "/usr/share/awesome/lib",
                            },
                        },
                        codeLens = {
                            enable = true,
                        },
                        completion = {
                            callSnippet = "Replace",
                        },
                        doc = {
                            privateName = { "^_" },
                        },
                        hint = {
                            enable = true,
                            setType = false,
                            paramType = true,
                            paramName = "Disable",
                            semicolon = "Disable",
                            arrayIndex = "Disable",
                        },
                    },
                },
            },
        },
    },
}
