vim.cmd("set expandtab")
vim.cmd("set tabstop=2")
vim.cmd("set softtabstop=2")
vim.cmd("set shiftwidth=2")

vim.g.mapleader = " "
vim.g.background = "light"

vim.opt.swapfile = false
-- vim.opt.showtabline = 2

-- Navigate vim panes better
vim.keymap.set('n', '<c-k>', ':wincmd k<CR>')
vim.keymap.set('n', '<c-j>', ':wincmd j<CR>')
vim.keymap.set('n', '<c-h>', ':wincmd h<CR>')
vim.keymap.set('n', '<c-l>', ':wincmd l<CR>')

vim.keymap.set('n', '<leader>h', ':nohlsearch<CR>')
vim.wo.number = true

vim.api.nvim_set_keymap('', '<Up>', '<Nop>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('', '<Down>', '<Nop>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('', '<Left>', '<Nop>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('', '<Right>', '<Nop>', { noremap = true, silent = true })

--[[noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>]]

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
vim.opt.showmode = false


--vnoremap . :norm! .<CR>
vim.api.nvim_set_keymap('v', '.', ':norm! .<CR>', { noremap = true, silent = true })

--nnoremap <C-c> "+y
--vnoremap <C-c> "+y
--nnoremap <C-p> "+p
--vnoremap <C-p> "+p

--inoremap jj <esc>
--inoremap JJ <esc>

--cnoremap jj <C-c>
--cnoremap JJ <C-c>

--nnoremap <SPACE> <Nop>

--noremap J 6jzz
--noremap K 6kzz
vim.api.nvim_set_keymap('n', 'J', '6jzz', { noremap = true, silent = true }) -- Mapping J to 6jzz
vim.api.nvim_set_keymap('n', 'K', '6kzz', { noremap = true, silent = true }) -- Mapping K to 6kzz

--vertically navigation
--noremap <C-u> <C-u>zz
--noremap <C-d> <C-d>zz
vim.api.nvim_set_keymap('n', '<C-u>', '<C-u>zz', { noremap = true, silent = true }) -- Mapping <C-u> to <C-u>zz
vim.api.nvim_set_keymap('n', '<C-d>', '<C-d>zz', { noremap = true, silent = true }) -- Mapping <C-d> to <C-d>zz

--find
--noremap n nzz
--noremap N Nzz
vim.api.nvim_set_keymap('n', 'n', 'nzz', { noremap = true, silent = true }) -- Mapping n to nzz
vim.api.nvim_set_keymap('n', 'N', 'Nzz', { noremap = true, silent = true }) -- Mapping N to Nzz


vim.keymap.set('n', '<leader>a', 'ggVG')
