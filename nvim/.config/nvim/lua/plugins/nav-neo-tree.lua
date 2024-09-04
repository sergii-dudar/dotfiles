return {
	"nvim-neo-tree/neo-tree.nvim",
	branch = "v3.x",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-tree/nvim-web-devicons",
		"MunifTanjim/nui.nvim",
	},
	config = function()
		vim.keymap.set("n", "<leader>nn", ":Neotree filesystem reveal left<CR>", { desc = "Neotree open"})
		vim.keymap.set("n", "<leader>nt", ":Neotree toggle<CR>", { desc = "Neotree toggle view"})
		--vim.keymap.set("n", "<leader>nb", ":Neotree buffers reveal float<CR>", { desc = "Neotree open buffers"})

		vim.keymap.set("n", "<leader>ng", ":Neotree git_status<CR>", { desc = "Neotree open git"})
		vim.keymap.set("n", "<leader>nb", ":Neotree buffers<CR>", { desc = "Neotree open buffers"})

        require("neo-tree").setup({
            source_selector = {
                winbar = true,
                statusline = false
            },
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
                group_empty_dirs = true
            }
        })
	end,
}