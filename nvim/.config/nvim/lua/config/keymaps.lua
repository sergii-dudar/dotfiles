-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

--Clear highlights on search when pressing <Esc> in normal mode
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Select all
vim.keymap.set("n", "<C-a>", "gg<S-v>G")

vim.keymap.set("n", "<leader>qq", "<cmd>q<cr>", { desc = "Quit" })
vim.keymap.set("n", "<leader>qQ", "<cmd>qa<cr>", { desc = "Quit All" })
--vim.keymap.set("n", "<leader>qW", "<cmd>wa<cr>", { desc = "Save All" })
--vim.keymap.set("n", "<leader>qW", "<cmd>wqa<cr>", { desc = "Save all Quit" })

--vim.keymap.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save File" })
vim.keymap.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>wa<cr><esc>", { desc = "Save All Files" })

-- Exit Vim's terminal mode
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

vim.api.nvim_set_keymap('n', 'J', '6jzz', { noremap = true, silent = true }) -- Mapping J to 6jzz
vim.api.nvim_set_keymap('n', 'K', '6kzz', { noremap = true, silent = true }) -- Mapping K to 6kzz

--vertically navigation with keep cursor on center
vim.api.nvim_set_keymap('n', '<C-u>', '<C-u>zz', { noremap = true, silent = true }) -- Mapping <C-u> to <C-u>zz
vim.api.nvim_set_keymap('n', '<C-d>', '<C-d>zz', { noremap = true, silent = true }) -- Mapping <C-d> to <C-d>zz

--find
vim.api.nvim_set_keymap('n', 'n', 'nzz', { noremap = true, silent = true }) -- Mapping n to nzz
vim.api.nvim_set_keymap('n', 'N', 'Nzz', { noremap = true, silent = true }) -- Mapping N to Nzz

--vim.keymap.set('n', '<leader>a', 'ggVG')
vim.api.nvim_set_keymap('n', '<leader>\\', ':vsplit | wincmd l<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>|', ':split | wincmd j<CR>', { noremap = true, silent = true })

--move selected block
--vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
--vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

--yank(copy) to system clipboard
vim.keymap.set({ "n", "v" }, "<leader>y", "\"+yy")
vim.keymap.set("n", "<leader>Y", "\"+y")

--start replacing word under cursonr
vim.keymap.set("n", "<leader>r", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
    { desc = "Start replacing word under cursor" })