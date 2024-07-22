-------------------------------------------------
-- COLORSCHEMES
-------------------------------------------------

-- Uncomment just ONE of the following colorschemes!
-- local ok, _ = pcall(vim.cmd, 'colorscheme base16-dracula')
-- local ok, _ = pcall(vim.cmd, 'colorscheme base16-gruvbox-dark-medium')
-- local ok, _ = pcall(vim.cmd, 'colorscheme base16-monokai')
-- local ok, _ = pcall(vim.cmd, 'colorscheme base16-nord')
-- local ok, _ = pcall(vim.cmd, 'colorscheme base16-oceanicnext')
-- local ok, _ = pcall(vim.cmd, "colorscheme base16-onedark")
-- local ok, _ = pcall(vim.cmd, 'colorscheme palenight')
-- local ok, _ = pcall(vim.cmd, 'colorscheme base16-solarized-dark')
-- local ok, _ = pcall(vim.cmd, 'colorscheme base16-solarized-light')
-- local ok, _ = pcall(vim.cmd, 'colorscheme base16-tomorrow-night')

-- Highlight the region on yank
vim.api.nvim_create_autocmd("TextYankPost", {
	group = num_au,
	callback = function()
		vim.highlight.on_yank({ higroup = "Visual", timeout = 300 })
        -- vim.api.nvim_set_hl(0, 'YankHighlight', { bg = '#FFD700', fg = '#000000' })
	end,
})
