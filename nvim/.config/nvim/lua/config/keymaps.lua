-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local map = LazyVim.safe_keymap_set

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
vim.api.nvim_set_keymap('n', '<leader>\\', ':vsplit | wincmd l<CR>', { noremap = true, silent = true, desc = "VSplit" })
vim.api.nvim_set_keymap('n', '<leader>|', ':split | wincmd j<CR>', { noremap = true, silent = true, desc = "HSplit" })

-- standard vim quickfix (ad default replaced to trouble.nvim)
map("n", "<leader>xL", "<cmd>lopen<cr>", { desc = "Location List" })
map("n", "<leader>xQ", "<cmd>copen<cr>", { desc = "Quickfix List" })

--move selected block
--vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
--vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

--yank(copy) to system clipboard
--vim.keymap.set({ "n", "v" }, "<leader>y", "\"+yy")
--vim.keymap.set("n", "<leader>Y", "\"+y")

--start replacing word under cursonr
--vim.keymap.set("n", "<leader>r", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
--    { desc = "Start replacing word under cursor" })

-- Example usage for jdtls
vim.opt.path:append("**")

map("v", "<leader>xp", function()
    local helpers = require("utils.common-util")
    local stack_trace = helpers.get_visual_selection()
    helpers.show_stack_trace_qflist(stack_trace)
end, { desc = "Parse trace to quick fix list" })

map("n", "<leader>xp", function()
    local helpers = require("utils.common-util")
    local stack_trace = helpers.get_current_buffer_text()
    helpers.show_stack_trace_qflist(stack_trace)
end, { desc = "Parse trace to quick fix list" })

map("n", "<leader>fs", function()
    local helpers = require("utils.common-util")

    local jdtls_client_id = helpers.get_client_id_by_name("jdtls")
    if jdtls_client_id then
        local current_buf_id = vim.api.nvim_get_current_buf()
        if not vim.lsp.buf_is_attached(current_buf_id, jdtls_client_id) then
            vim.lsp.buf_attach_client(current_buf_id, jdtls_client_id)

            LazyVim.info("jdtls client found by ID:" .. jdtls_client_id)
            LazyVim.info("attaching jdtls to current buffer by ID:" .. current_buf_id)
        end
    end

    local fileName = helpers.get_file_with_no_ext()
    -- LazyVim.info("fileName:" .. fileName)

    require("telescope.builtin").lsp_dynamic_workspace_symbols({
        symbols = LazyVim.config.get_kind_filter(),
        default_text = fileName
    })
end, { desc = "Find word under curser in lsp dynamic_workspace_symbols" })

-- debug purposes
_G.log_table = function (table)
    require("utils.common-util").log_table(table);
end