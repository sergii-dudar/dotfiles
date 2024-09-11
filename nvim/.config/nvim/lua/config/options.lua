-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

vim.opt.tabstop = 1
vim.opt.softtabstop = 1
vim.opt.shiftwidth = 1
vim.opt.expandtab = true

vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.background = "light"
vim.g.have_nerd_font = true

vim.opt.swapfile = false
-- vim.opt.showtabline = 2

-- Enable syntax highlighting
--vim.cmd('syntax on')

-- Set various options
vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.incsearch = true
vim.opt.swapfile = false
vim.opt.number = true
vim.wo.number = true
vim.opt.relativenumber = true
vim.opt.clipboard = 'unnamedplus'
vim.opt.clipboard = ''
vim.opt.showmode = false
vim.opt.termguicolors = true
vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.smartindent = true


-- Enable mouse mode, useful for resizing splits, debugging etc
vim.opt.mouse = 'a'


local augroup = vim.api.nvim_create_augroup
local myCustomGroup = augroup('myCustomStartupGroup', {})

-- by some reasons not firing from `autocmds`
vim.api.nvim_create_autocmd("UiEnter", {
    desc = "Open Neotree automatically",
    group = myCustomGroup,
    --pattern = { "*" },
    callback = function()

        -- restore current proj session
        require("persistence").load()

        if vim.fn.argc() == 0 then
            vim.cmd("Neotree filesystem reveal left")
        end
    end,
})