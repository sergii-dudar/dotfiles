return {
    "nvim-neo-tree/neo-tree.nvim",
    cond = require("utils.project-util").is_multifile_proj,
    keys = {
        -- same as default, just swapped [ <leader>E ] with [ <leader>e]
        { "<leader>E", "<leader>fe", desc = "Explorer NeoTree (Root Dir)", remap = true },
        { "<leader>e", "<leader>fE", desc = "Explorer NeoTree (cwd)", remap = true },
    },
    opts = {
        source_selector = {
            winbar = false,
            statusline = false,
        },
        window = {
            mappings = {
                --    e = 'none'
                --["/"] = "noop",
                --["<esc>"] = "noop",
            },
            --position = "float",
            popup = { -- settings that apply to float position only
                size = { height = "90%", width = "70%" },
                position = "50%", -- 50% means center it
            },
        },
        popup_border_style = "rounded", --'solid',
        filesystem = {
            filtered_items = {
                visible = false,
                hide_dotfiles = false,
                hide_gitignored = false,
                hide_by_name = {
                    ".neotreeignore",
                    ".ignore",
                    ".git",
                    ".idea",
                    ".settings",
                    ".project",
                    ".factorypath",
                    "target",
                    -- "bin",
                    -- ".rgignore"
                },
                hide_by_pattern = { -- uses glob style patterns
                    ".classpath.*",
                },
            },
            follow_current_file = {
                enabled = true, -- This will find and focus the file in the active buffer every time
                --               -- the current file is changed while the tree is open.
                leave_dirs_open = true, -- `false` closes auto expanded dirs, such as with `:Neotree reveal`
            },
            group_empty_dirs = true,
        },
        event_handlers = {
            -- add relativenumber to neo-tree buffer
            {
                event = "neo_tree_buffer_enter",
                handler = function()
                    vim.opt_local.relativenumber = true
                    vim.opt_local.number = true
                end,
            },
            -- automatically toggle file preview, not working if neotree opened, need manually toggle, but works great
            -- in case close\open tree that fully ok for my flow
            -- {
            --     event = 'after_render',
            --     handler = function ()
            --         local state = require('neo-tree.sources.manager').get_state('filesystem')
            --         if not require('neo-tree.sources.common.preview').is_active() then
            --             state.config = { use_float = false } -- or whatever your config is
            --             state.commands.toggle_preview(state)
            --         end
            --     end
            -- }
        },
        default_component_configs = {
            type = {
                enabled = false,
                width = 10, -- width of the column
                required_width = 100, -- min width of window required to show this column
            },
            last_modified = {
                enabled = true,
                required_width = 100,
            },
            file_size = {
                enabled = false,
            },
        },
    },
}