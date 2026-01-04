local java_util = require("utils.java.java-common")
local java_refactor_util = require("utils.java.java-refactor-util")
function toggle_fyler()
    vim.cmd("Neotree close") -- close neotree, as in case moving files and opened neotree getting errors.
    require("fyler").toggle()
end
return {
    "A7Lavinraj/fyler.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    lazy = false,
    -- stylua: ignore
    keys = {
        { "<leader>i", toggle_fyler, desc = "Toggle Fyler View" },
        { "<leader>-", toggle_fyler, desc = "Toggle Fyler View" }
    },
    opts = {
        integrations = {
            icon = "nvim_web_devicons",
        },
        hooks = {
            on_delete = function(path)
                vim.notify("DELETED: " .. path, vim.log.levels.INFO) -- You can do anything whenever an item deleted
            end,
            on_rename = function(src, dst)
                if java_util.is_java_project() then
                    java_refactor_util.register_change(src, dst)
                    -- vim.notify("SUCCESS: " .. src .. " > " .. dst, vim.log.levels.INFO) -- You can do anything whenever an item Renamed
                else
                    Snacks.rename.on_rename_file(src, dst) -- LSP-integrated file renaming (for lsp supported it) - DEFAULT
                end
                -- vim.notify("RENAMED: " .. src .. " > " .. dst, vim.log.levels.INFO) -- You can do anything whenever an item Renamed
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
}