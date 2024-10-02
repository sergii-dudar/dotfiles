return {
    {
        "nvim-telescope/telescope.nvim",
        keys = {
            { "<leader>fl", "<cmd>Telescope notify<cr>", desc = "Find Logs" },
        },
        opts = function(_, opts)
            opts.defaults.path_display = { "smart" }

            local actions = require("telescope.actions")

            opts.defaults.mappings.i["<esc>"] = actions.close
            opts.defaults.mappings.i["<C-k>"] = actions.move_selection_previous
            opts.defaults.mappings.i["<C-j>"] = actions.move_selection_next

            opts.defaults.vimgrep_arguments = {
                "rg",
                "--color=never",
                "--no-heading",
                "--with-filename",
                "--line-number",
                "--column",
                "--smart-case",
                "--fixed-strings"
            }

            -- Telescope find_files find_command=fd,--glob prompt_prefix=🔍

            -- opts.defaults.file_ignore_patterns = {}

            --opts.defaults.mappings.i["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist
            --opts.defaults.mappings.i["<C-q>"] = actions.send_selection_to_qflist + actions.open_qflist
            -- table.insert(opts.sources, { name = 'emoji' })
        end
    }
}