return {
    "nvim-neo-tree/neo-tree.nvim",
    cond = require("utils.neo-tree-util").is_enable_neo_tree,
    opts = {
        source_selector = {
            winbar = true,
            statusline = false,
        },
        window = {
            mappings = {
                --    e = 'none'
                ["/"] = "noop",
                ["<esc>"] = "noop",
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
                visible = true,
                hide_dotfiles = false,
                hide_gitignored = true,
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
