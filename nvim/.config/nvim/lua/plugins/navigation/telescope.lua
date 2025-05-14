local telescope_path_display_opts = { "filename_first" }
local telescope_rg_opts = {
    path_display = telescope_path_display_opts,
}

return {
    {
        "nvim-telescope/telescope.nvim",
        dependencies = {
            --"nvim-telescope/telescope-file-browser.nvim",

            "nvim-telescope/telescope-live-grep-args.nvim",
            "blanktiger/telescope-rg.nvim",
            -- rg --vimgrep search_text -g *.ext -g !*.exe externals/users/
            -- rg --files -g '!*.hcl' -g '*.sh' -g '-kafka' externals/users/

            -- live grep with pick dir where to search before
            "smilovanovic/telescope-search-dir-picker.nvim",
            -- "consumer_group" -g *.hcl externals/users/
        },
        keys = {
            --{ "<leader>fl", "<cmd>Telescope notify<cr>", desc = "Find Logs" }, -> <leader>n by snack.nvim
            {
                "<leader>sii",
                function()
                    require("search_dir_picker").search_dir()
                end,
                desc = "[S]earch [i]n [d]ir",
            },
            {
                "<leader>sia",
                function()
                    require("telescope").extensions.ripgrep.ripgrep_text(telescope_rg_opts)
                end,
                desc = "[S]earch text [i]n [a]rgs",
            },
            {
                "<leader>sil",
                function()
                    require("telescope").extensions.live_grep_args.live_grep_args()
                end,
                desc = "Telescope live grep [a]rgs",
            },
            {
                "<leader>fi",
                function()
                    require("telescope").extensions.ripgrep.ripgrep_files(telescope_rg_opts)
                end,
                desc = "Telescope [f]iles [i]n args",
            },
            { "<leader>.", LazyVim.pick("files", { root = true }), desc = "Find Files (cwd)" },
            { "<leader>/", LazyVim.pick("live_grep", { root = true }), desc = "Grep (cwd)" },
        },
        opts = function(_, opts)
            --opts.defaults.path_display = { "smart" }
            opts.defaults.path_display = telescope_path_display_opts

            local telescope = require("telescope")
            local actions = require("telescope.actions")
            --local lga_actions = require("telescope-live-grep-args.actions")

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
                "--fixed-strings",
                --"--word-regexp"
            }

            -- Telescope find_files find_command=fd,--glob prompt_prefix=🔍

            -- opts.defaults.file_ignore_patterns = {}

            --opts.defaults.mappings.i["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist
            --opts.defaults.mappings.i["<C-q>"] = actions.send_selection_to_qflist + actions.open_qflist
            -- table.insert(opts.sources, { name = 'emoji' })

            --telescope.load_extension("file_browser")
            telescope.load_extension("live_grep_args")
            telescope.load_extension("search_dir_picker")
            telescope.load_extension("ripgrep")

            --local telescope_util = require("utils.telescope-util")
            --vim.api.nvim_set_keymap('n', '<leader>sg', telescope_util.grep_with_dynamic_options, { noremap = true, silent = true })
            --vim.api.nvim_set_keymap('n', '<leader>fg', "<cmd>lua require('utils.telescope-util').grep_with_dynamic_options()<CR>", { noremap = true, silent = true })
        end,
    },
}