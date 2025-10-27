return {
    {
        "stevearc/oil.nvim",
        keys = {
            {
                "<leader>-",
                function()
                    require("oil").toggle_float()
                end,
                desc = "Oil Toggle Float",
            },
        },
        opts = {
            default_file_explorer = true,
            -- Send deleted files to the trash instead of permanently deleting them (:help oil-trash)
            delete_to_trash = true,
            -- Skip the confirmation popup for simple operations (:help oil.skip_confirm_for_simple_edits)
            skip_confirm_for_simple_edits = false,
            float = {
                -- Padding around the floating window
                padding = 5,
                -- max_width and max_height can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
                max_width = 0.8,
                max_height = 0,
                border = "rounded",
            },
            columns = {
                "icon",
                -- "permissions",
                -- "size",
                -- "mtime",
            },
            view_options = {
                -- Show files and directories that start with "."
                show_hidden = true,
                -- Sort file names with numbers in a more intuitive order for humans.
                -- Can be "fast", true, or false. "fast" will turn it off for large directories.
                natural_order = true,
                -- This function defines what is considered a "hidden" file
                is_always_hidden = function(name, _)
                    return name == ".." or name == ".git"
                end,
            },
            keymaps = {
                ["<C-c>"] = false,
                ["q"] = "actions.close",
                ["<C-l>"] = "actions.select",
                ["<C-h>"] = "actions.parent",
                ["<Right>"] = "actions.select",
                ["<Left>"] = "actions.parent",
            },
            -- default keymaps
            --[[ keymaps = {
                ["g?"] = { "actions.show_help", mode = "n" },
                ["<CR>"] = "actions.select",
                ["<C-s>"] = { "actions.select", opts = { vertical = true } },
                ["<C-h>"] = { "actions.select", opts = { horizontal = true } },
                ["<C-t>"] = { "actions.select", opts = { tab = true } },
                ["<C-p>"] = "actions.preview",
                ["<C-c>"] = { "actions.close", mode = "n" },
                ["<C-l>"] = "actions.refresh",
                ["-"] = { "actions.parent", mode = "n" },
                ["_"] = { "actions.open_cwd", mode = "n" },
                ["`"] = { "actions.cd", mode = "n" },
                ["~"] = { "actions.cd", opts = { scope = "tab" }, mode = "n" },
                ["gs"] = { "actions.change_sort", mode = "n" },
                ["gx"] = "actions.open_external",
                ["g."] = { "actions.toggle_hidden", mode = "n" },
                ["g\\"] = { "actions.toggle_trash", mode = "n" },
            }, ]]
        },
        -- dependencies = { { "nvim-mini/mini.icons", opts = {} } },
        dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if you prefer nvim-web-devicons
        -- Lazy loading is not recommended because it is very tricky to make it work correctly in all situations.
        lazy = false,
    },
}
