function toggle_fyler()
    vim.cmd("Neotree close") -- close neotree, as in case moving files and opened neotree getting errors.
    require("fyler").toggle()
end
return {
    "A7Lavinraj/fyler.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    branch = "stable", -- Use stable branch for production
    -- commit = "5ef9d97b9292cee169ef0cfb865996f15ee9577a",
    -- branch = "main",
    lazy = false,
    -- stylua: ignore
    keys = {
        { "<leader>i", toggle_fyler, desc = "Toggle Fyler View" },
        { "<leader>-", toggle_fyler, desc = "Toggle Fyler View" }
    },
    opts = {
        -- Event hooks for custom behavior (on_highlight, on_delete, on_rename)
        hooks = {
            on_rename = function(src, dst)
                require("modules.java.refactor.integrations").fyler_on_rename(src, dst)
            end,
        },
        -- Whether to skip confirmation for "simple" mutations. A simple mutation
        -- follows: copy <= 1 and create <= 5 and delete <= 0 and move <= 1
        auto_confirm_simple_mutation = false,
        -- Restricts cursor from moving outside editable region
        bound_cursor = true,
        -- Buffer-local options applied to the finder buffer
        -- (see: nvim_set_option_value)
        buf_opts = {},
        -- Follow current file
        follow_current_file = true,
        -- Table of extensions to enable (e.g., 'git', 'trash', 'watcher')
        extensions = {
            -- Enable git extension
            -- git = { enabled = true },
            -- Enable git extension with right aligned icons
            -- git = { enabled = true, inline = false },
            -- Enable trash extension
            -- trash = { enabled = true },
            -- Enable watcher extension
            -- watcher = { enabled = true },
        },
        -- External integrations (e.g., icon provider)
        integrations = {
            icon = "nvim_web_devicons",
        },
        -- Window-local options applied to the finder window
        -- (see: nvim_set_option_value)
        win_opts = {
            number = true,
            relativenumber = true,
        },
        -- Buffer kind to use globally.
        kind = "floating", -- floating, replace, split_left, split_left_most, split_above, split_above_all, split_right, split_right_most, split_below, split_below_all
        -- Per-kind preset overrides. Each preset can contain mappings,
        -- buf_opts, win_opts, and any window layout fields
        kind_presets = {
            floating = {
                -- Border style (see: :h winborder)
                border = "single",
                -- Size of buffer:
                -- - string with '%' for relative (e.g. '70%')
                -- - number for absolute
                height = "95%",
                mappings = {
                    n = {
                        ["<CR>"] = {
                            action = "select",
                            args = { close = true, pick = false },
                        },
                    },
                },
                width = "60%",
                -- top = "2%",
                -- left = "15%",
                -- Horizontal alignment: 'start' | 'center' | 'end'
                col = "center",
                -- Vertical alignment: 'start' | 'center' | 'end'
                row = "center",
            },
            replace = {
                mappings = {
                    n = {
                        ["<CR>"] = {
                            action = "select",
                            args = { close = true, pick = false },
                        },
                    },
                },
            },
            split_above = { height = "50%" },
            split_above_all = { height = "50%" },
            split_below = { height = "50%" },
            split_below_all = { height = "50%" },
            split_left = { width = "25%" },
            split_left_most = { width = "25%" },
            split_right = { width = "25%" },
            split_right_most = { width = "25%" },
        },
        -- Key mappings organized by mode (see: fyler.Mapping)
        mappings = {
            n = {
                ["-"] = {
                    action = "visit",
                    args = { parent = true },
                },
                ["."] = {
                    action = "visit",
                    args = { cursor = true },
                },
                ["<BS>"] = {
                    action = "shrink",
                    args = { parent = true },
                },
                ["<C-R>"] = {
                    action = "refresh",
                    args = { recursive = true, force = true },
                },
                ["<C-S>"] = { disabled = true },
                ["<C-T>"] = {
                    action = "select",
                    args = { tabedit = true },
                },
                ["<C-V>"] = {
                    action = "select",
                    args = { vsplit = true },
                },
                ["<CR>"] = {
                    action = "select",
                    args = { pick = true },
                },
                ["="] = {
                    action = "visit",
                },
                ["g."] = {
                    action = "toggle_ui",
                    args = { "hidden_items" },
                },
                ["gi"] = {
                    action = "toggle_ui",
                    args = { "indent_guides" },
                },
                ["q"] = {
                    action = "close",
                },
            },
        },
        -- UI configuration
        ui = {
            hidden_items = {
                -- Toggleable pre-defined switches (e.g. 'dotfiles' to hide files).
                switches = {},
                -- Toggleable patterns (Lua patterns matched against the full path).
                patterns = {},
                -- Always visible items matching these patterns, even if they would
                -- normally be hidden.
                always_visible = {},
                -- Always hide items matching these patterns, even if they would
                -- normally be visible.
                always_hidden = {},
            },
            -- Whether to draw indent guides at each depth level.
            indent_guides = true,
        },
        -- views = {
        --     finder = {
        --         close_on_select = false,
        --         -- confirm_simple = false,
        --         default_explorer = false,
        --         delete_to_trash = false,
        --         icon = {
        --             -- directory_collapsed = "",
        --             -- directory_empty = "",
        --             -- directory_expanded = "",
        --             directory_collapsed = "󰉋",
        --             directory_empty = "",
        --             directory_expanded = "󰝰",
        --         },
        --         mappings = {
        --             ["\\"] = "SelectVSplit",
        --             ["|"] = "SelectSplit",
        --             -- ["q"] = "CloseView",
        --             -- ["<CR>"] = "Select",
        --             -- ["<C-t>"] = "SelectTab",
        --             -- ["|"] = "SelectVSplit",
        --             -- ["-"] = "SelectSplit",
        --             -- ["^"] = "GotoParent",
        --             -- ["="] = "GotoCwd",
        --             -- ["."] = "GotoNode",
        --             -- ["#"] = "CollapseAll",
        --             -- ["<BS>"] = "CollapseNode",
        --         },
        --         follow_current_file = true,
        --         win = {
        --             border = vim.o.winborder == "" and "rounded" or vim.o.winborder, -- "bold""double","none","rounded","shadow","single","solid"
        --             kind = "float",
        --             kinds = {
        --                 -- float = {
        --                 --     height = "80%",
        --                 --     width = "60%",
        --                 --     top = "5%",
        --                 --     left = "25%",
        --                 -- },
        --                 float = {
        --                     height = "90%",
        --                     width = "60%",
        --                     top = "2%",
        --                     left = "15%",
        --                 },
        --             },
        --             win_opts = {
        --                 -- cursorline = false,
        --                 number = true,
        --                 relativenumber = true,
        --                 winhighlight = "Normal:FylerNormal,NormalNC:FylerNormalNC",
        --             },
        --         },
        --     },
        -- },
    },
}
