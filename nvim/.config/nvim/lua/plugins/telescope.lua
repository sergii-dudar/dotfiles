return {
    {
        "nvim-telescope/telescope-ui-select.nvim",
    },
    {
        "nvim-telescope/telescope-file-browser.nvim",
        dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
    },
    {
        "nvim-telescope/telescope.nvim",
        tag = "0.1.5",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = function()
            require("telescope").setup({
                defaults = {
                    vimgrep_arguments = {
                        'rg',
                        '--hidden',
                        '--glob', '!.git/',
                        '--vimgrep'
                    }
                },
                pickers = {
                    find_files = {
                        hidden = true,
                    },
                },
                extensions = {
                    ["ui-select"] = {
                        require("telescope.themes").get_dropdown({}),
                    },
                },
            })
            local builtin = require("telescope.builtin")
            vim.keymap.set("n", "<leader>ff", builtin.find_files, {})
            vim.keymap.set("n", "<leader>gf", builtin.git_files, {})

            vim.keymap.set("n", "<leader>fg", builtin.live_grep, {})
            vim.keymap.set("n", "<leader>fo", builtin.oldfiles, {})
            vim.keymap.set("n", "<leader>fc", builtin.colorscheme, {})
            vim.keymap.set("n", "<leader>fd", ":Telescope file_browser<CR>", {})
            vim.keymap.set("n", "<leader>fk", ":Telescope keymaps<CR>", {})
            vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
            vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

            require("telescope").load_extension("ui-select")

            -- vim.keymap.set('n', '<leader>pws', function()
            --     local word = vim.fn.expand("<cword>")
            --     builtin.grep_string({ search = word })
            -- end)
            -- vim.keymap.set('n', '<leader>pWs', function()
            --     local word = vim.fn.expand("<cWORD>")
            --     builtin.grep_string({ search = word })
            -- end)
            -- vim.keymap.set('n', '<leader>ps', function()
            --     builtin.grep_string({ search = vim.fn.input("Grep > ") })
            -- end)
        end,
    },
}