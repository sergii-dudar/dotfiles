local lazy_path = vim.fn.stdpath("data") .. "/lazy"
local plenary_lua_path = lazy_path .. "/plenary.nvim/lua"

return {
    "neovim/nvim-lspconfig",
    opts = {
        servers = {
            lua_ls = {
                settings = {
                    Lua = {
                        diagnostics = {
                            enable = true,
                            globals = {
                                "vim",
                                "use",
                                "awesome",
                                "client",
                                "root",
                                "screen",
                                "mouse",
                                "describe",
                                "it",
                                "pending",
                                "before_each",
                                "after_each",
                                "clear",
                            },
                            disable = { "lowercase-global" },
                        },
                        workspace = {
                            checkThirdParty = false,
                            library = {
                                "/usr/share/nvim/runtime/lua",
                                "/usr/share/nvim/runtime/lua/lsp",
                                "/usr/share/awesome/lib",
                                plenary_lua_path,
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
