return {
    "A7Lavinraj/fyler.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    lazy = false,
    opts = {
        integrations = {
            icon = "nvim_web_devicons",
        },
        hooks = {
            on_delete = function(path)
                print("DELETED: " .. path) -- You can do anything whenever an item deleted
            end,
            on_rename = function(src, dst)
                -- Snacks.rename.on_rename_file(src, dst) -- LSP-integrated file renaming (for lsp supported it) - DEFAULT
                print("RENAMED: " .. src .. " > " .. dst) -- You can do anything whenever an item Renamed
            end,
        },
        views = {
            finder = {
                close_on_select = false,
                confirm_simple = false,
                default_explorer = false,
                delete_to_trash = false,
                icon = {
                    directory_collapsed = "",
                    directory_empty = "",
                    directory_expanded = "",
                },
                mappings = {
                    ["\\"] = "SelectVSplit",
                    ["|"] = "SelectSplit",
                    -- ["q"] = "CloseView",
                    -- ["<CR>"] = "Select",
                    -- ["<C-t>"] = "SelectTab",
                    -- ["|"] = "SelectVSplit",
                    -- ["-"] = "SelectSplit",
                    -- ["^"] = "GotoParent",
                    -- ["="] = "GotoCwd",
                    -- ["."] = "GotoNode",
                    -- ["#"] = "CollapseAll",
                    -- ["<BS>"] = "CollapseNode",
                },
                follow_current_file = true,
                win = {
                    border = vim.o.winborder == "" and "rounded" or vim.o.winborder, -- "bold""double","none","rounded","shadow","single","solid"
                    kind = "float",
                    kinds = {
                        -- float = {
                        --     height = "80%",
                        --     width = "60%",
                        --     top = "5%",
                        --     left = "25%",
                        -- },
                        float = {
                            height = "90%",
                            width = "60%",
                            top = "2%",
                            left = "15%",
                        },
                    },
                    win_opts = {
                        -- cursorline = false,
                        number = true,
                        relativenumber = true,
                        winhighlight = "Normal:FylerNormal,NormalNC:FylerNormalNC",
                    },
                },
            },
        },
    },
    -- stylua: ignore
    keys = {
        { "<leader>i", function() require("fyler").toggle() end, desc = "Toggle Fyler View" },
        { "<leader>-", function() require("fyler").toggle() end, desc = "Toggle Fyler View" }
    },
}
