-- Navigate vim panes better

-- vim.keymap.set('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
-- vim.keymap.set('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
-- vim.keymap.set('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
-- vim.keymap.set('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

vim.keymap.set('n', '<c-k>', ':wincmd k<CR>')
vim.keymap.set('n', '<c-j>', ':wincmd j<CR>')
vim.keymap.set('n', '<c-h>', ':wincmd h<CR>')
vim.keymap.set('n', '<c-l>', ':wincmd l<CR>')

--vim.keymap.set('n', '<leader>hn', ':nohlsearch<CR>')
--Clear highlights on search when pressing <Esc> in normal mode
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

vim.keymap.set('n', '<leader>ch', ':checkhealth<CR>')

vim.api.nvim_set_keymap('', '<Up>', '<Nop>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('', '<Down>', '<Nop>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('', '<Left>', '<Nop>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('', '<Right>', '<Nop>', { noremap = true, silent = true })

--vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
--vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

--vnoremap . :norm! .<CR>
vim.api.nvim_set_keymap('v', '.', ':norm! .<CR>', { noremap = true, silent = true })

--nnoremap <SPACE> <Nop>

--vim.api.nvim_set_keymap('n', 'J', '6jzz', { noremap = true, silent = true }) -- Mapping J to 6jzz
--vim.api.nvim_set_keymap('n', 'K', '6kzz', { noremap = true, silent = true }) -- Mapping K to 6kzz

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
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

--stay curson in plase during joining lines
vim.keymap.set("n", "J", "mzJ`z")

--yank(copy) to system clipboard
vim.keymap.set({ "n", "v" }, "<leader>y", "\"+yy")
vim.keymap.set("n", "<leader>Y", "\"+y")

-- Diagnostic keymaps
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

--start replacing word under cursonr
vim.keymap.set("n", "<leader>r", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
    { desc = "Start replacing word under cursor" })
vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true, desc = "Make file executable" })

--cool projects navigations: see
vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")
