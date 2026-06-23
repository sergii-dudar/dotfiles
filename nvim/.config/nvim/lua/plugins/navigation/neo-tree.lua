local neotree_util = require("utils.neotree-util")

return {
    "nvim-neo-tree/neo-tree.nvim",
    -- cond = require("utils.project-util").is_multifile_proj,
    keys = {
        -- same as default, just swapped [ <leader>E ] with [ <leader>e]
        -- { "<leader>E", "<leader>fe", desc = "Explorer NeoTree (Root Dir)", remap = true },
        { "<leader>e", "<cmd>Neotree reveal show<cr>", desc = "Show current in NeoTree", remap = true },
        { "<leader>E", "<leader>fE", desc = "Explorer NeoTree (cwd)", remap = true },
    },
    opts = {
        source_selector = {
            winbar = false,
            statusline = false,
        },
        commands = {
            shared_copy = neotree_util.shared_copy,
            shared_copy_visual = neotree_util.shared_copy_visual,
            shared_paste = neotree_util.shared_paste,
        },
        window = {
            mappings = {
                --    e = 'none'
                ["/"] = "noop",
                ["<esc>"] = "noop",
                ["Y"] = "shared_copy",
                ["P"] = "shared_paste",
                ["c"] = {
                    function(state)
                        local node = state.tree:get_node()
                        local path = node:get_id()
                        vim.fn.setreg("+", path, "c")
                        vim.notify("Copied path: " .. path)
                    end,
                    desc = "[C]opy Path to [C]lipboard",
                },
                ["gc"] = {
                    function(state)
                        local node = state.tree:get_node()
                        local name = node.name
                        vim.fn.setreg("+", name, "c")
                        vim.notify("Copied name: " .. name)
                    end,
                    desc = "Copy file/dir name to clipboard",
                },
                ["gd"] = {
                    function(state)
                        state.group_empty_dirs = not state.group_empty_dirs
                        local manager = require("neo-tree.sources.manager")
                        manager.refresh(state.name)
                        vim.notify("group_empty_dirs: " .. tostring(state.group_empty_dirs))
                    end,
                    desc = "Toggle group empty dirs",
                },
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
                    ".DS_Store",
                    "target",
                    -- "bin", -- commented as currently now using gradle
                    -- ".rgignore"
                },
                hide_by_pattern = { -- uses glob style patterns
                    ".classpath.*",
                },
                -- always_show_by_pattern = {
                --     global.dotfiles_path("/bin"),
                -- },
            },
            follow_current_file = {
                enabled = false, -- This will find and focus the file in the active buffer every time
                --               -- the current file is changed while the tree is open.
                leave_dirs_open = true, -- `false` closes auto expanded dirs, such as with `:Neotree reveal`
            },
            group_empty_dirs = true,
            -- Neo-tree does not need to be manually refreshed.
            -- manuall refresh if need: require("neo-tree.command").execute({ action = "refresh" })
            use_libuv_file_watcher = true,
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
            {
                event = "neo_tree_popup_input_ready",
                --- Keep rename inputs open when leaving insert mode with Esc, and go to normal mode.
                ---@param args { bufnr: integer }
                handler = function(args)
                    vim.keymap.set("i", "<esc>", vim.cmd.stopinsert, { noremap = true, buffer = args.bufnr })
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
