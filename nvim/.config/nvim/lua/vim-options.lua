vim.opt.tabstop = 1
vim.opt.softtabstop = 1
vim.opt.shiftwidth = 1
vim.opt.expandtab = true

vim.g.mapleader = " "
vim.g.background = "light"

vim.opt.swapfile = false
-- vim.opt.showtabline = 2

-- Navigate vim panes better
vim.keymap.set('n', '<c-k>', ':wincmd k<CR>')
vim.keymap.set('n', '<c-j>', ':wincmd j<CR>')
vim.keymap.set('n', '<c-h>', ':wincmd h<CR>')
vim.keymap.set('n', '<c-l>', ':wincmd l<CR>')

--vim.keymap.set('n', '<leader>h', ':nohlsearch<CR>')
vim.wo.number = true

vim.api.nvim_set_keymap('', '<Up>', '<Nop>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('', '<Down>', '<Nop>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('', '<Left>', '<Nop>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('', '<Right>', '<Nop>', { noremap = true, silent = true })

--syntax on
--set hlsearch
--set ignorecase
--set incsearch
--set noswapfile
--set number
--set number relativenumber
--set clipboard=unnamedplus

-- Enable syntax highlighting
--vim.cmd('syntax on')

-- Set various options
vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.incsearch = true
vim.opt.swapfile = false
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.clipboard = 'unnamedplus'
vim.opt.clipboard = ''
vim.opt.showmode = false
vim.opt.termguicolors = true
vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.smartindent = true

--vnoremap . :norm! .<CR>
vim.api.nvim_set_keymap('v', '.', ':norm! .<CR>', { noremap = true, silent = true })

--nnoremap <SPACE> <Nop>

vim.api.nvim_set_keymap('n', 'J', '6jzz', { noremap = true, silent = true }) -- Mapping J to 6jzz
vim.api.nvim_set_keymap('n', 'K', '6kzz', { noremap = true, silent = true }) -- Mapping K to 6kzz

--vertically navigation
vim.api.nvim_set_keymap('n', '<C-u>', '<C-u>zz', { noremap = true, silent = true }) -- Mapping <C-u> to <C-u>zz
vim.api.nvim_set_keymap('n', '<C-d>', '<C-d>zz', { noremap = true, silent = true }) -- Mapping <C-d> to <C-d>zz

--find
vim.api.nvim_set_keymap('n', 'n', 'nzz', { noremap = true, silent = true }) -- Mapping n to nzz
vim.api.nvim_set_keymap('n', 'N', 'Nzz', { noremap = true, silent = true }) -- Mapping N to Nzz

--vim.keymap.set('n', '<leader>a', 'ggVG')
vim.api.nvim_set_keymap('n', '<leader>\\', ':vsplit | wincmd l<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>|', ':split | wincmd j<CR>', { noremap = true, silent = true })