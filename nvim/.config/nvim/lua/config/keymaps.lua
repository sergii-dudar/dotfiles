-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local map = LazyVim.safe_keymap_set

--Clear highlights on search when pressing <Esc> in normal mode
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Quick diff conflict resolution
vim.keymap.set("n", "<leader>d1", ":diffget 1<CR>") -- get from left [LOCAL]
vim.keymap.set("n", "<leader>d2", ":diffget 2<CR>") -- get from right [REMORE]
vim.keymap.set("n", "<leader>d3", ":diffget 3<CR>") -- get from right [REMORE]
-- vim.keymap.set("n", "<leader>dl", ":diffget 3<CR>") -- get from right [REMORE]

-- tabs
--[[ map("n", "<leader><tab>n", function()
    vim.cmd.tabnew()
    local name = vim.fn.input("Tab name: ")
    vim.cmd("BufferLineTabRename " .. name)
end, { desc = "New Tab Named" })

map("n", "<leader><tab>r", function()
    local name = vim.fn.input("Tab name: ")
    local current_tab_num = vim.fn.tabpagenr()
    vim.cmd("BufferLineTabRename " .. current_tab_num .. " " .. name)
end, { desc = "Rename Current Tab" }) ]]

-- Select all
vim.keymap.set("n", "<C-a>", "gg<S-v>G")

vim.keymap.set("n", "<leader>qq", "<cmd>q<cr>", { desc = "Quit" })
vim.keymap.set("n", "<leader>qQ", "<cmd>qa<cr>", { desc = "Quit All" })
--vim.keymap.set("n", "<leader>qW", "<cmd>wa<cr>", { desc = "Save All" })
--vim.keymap.set("n", "<leader>qW", "<cmd>wqa<cr>", { desc = "Save all Quit" })

-- vim.api.nvim_set_keymap("n", "", "<C-o>", { noremap = true, silent = true, desc = "Go To Previour Position" })
-- vim.api.nvim_set_keymap("n", "", "<C-i>", { noremap = true, silent = true, desc = "Go To Next Position" })

--[[if vim.fn.has("mac") == 1 then
    -- Move Lines (default lazyvim is alt <A-...> for linux\windows)
    map("n", "<M-j>", "<cmd>execute 'move .+' . v:count1<cr>==", { desc = "Move Down" })
    map("n", "<M-k>", "<cmd>execute 'move .-' . (v:count1 + 1)<cr>==", { desc = "Move Up" })
    map("i", "<M-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Down" })
    map("i", "<M-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Up" })
    map("v", "<M-j>", ":<C-u>execute \"'<,'>move '>+\" . v:count1<cr>gv=gv", { desc = "Move Down" })
    map("v", "<M-k>", ":<C-u>execute \"'<,'>move '<-\" . (v:count1 + 1)<cr>gv=gv", { desc = "Move Up" })

    map({ "i", "x", "n", "s" }, "<D-s>", "<cmd>w<cr><esc>", { desc = "Save File" })
end]]

--vim.keymap.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save File" })
vim.keymap.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>wa<cr><esc>", { desc = "Save All Files" })

-- Exit Vim's terminal mode
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

vim.keymap.set({ "n", "x" }, "K", "8kzz", { noremap = true, silent = true }) -- Mapping K to 6kzz
vim.keymap.set({ "n", "x" }, "J", "8jzz", { noremap = true, silent = true }) -- Mapping J to 6jzz
vim.keymap.set({ "n", "x" }, "H", "^", { noremap = true, silent = true }) -- Mapping K to 6kzz
vim.keymap.set({ "n", "x" }, "L", "$", { noremap = true, silent = true }) -- Mapping J to 6jzz

-- vim.keymap.set({ "n", "x" }, "H", "12h", { noremap = true, silent = true })
-- vim.keymap.set({ "n", "x" }, "L", "12l", { noremap = true, silent = true })

-- map("n", "<M-S-h>", "<cmd>bprevious<cr>", { desc = "Prev Buffer" })
-- map("n", "<M-S-l>", "<cmd>bnext<cr>", { desc = "Next Buffer" })

vim.api.nvim_set_keymap("n", "<leader>J", ":join<CR>", { noremap = true, silent = true, desc = "Join with next line" })
--vim.api.nvim_set_keymap('n', '<leader>J', ':<C-u>join!<CR>', { noremap = true, silent = true })
--vim.api.nvim_set_keymap('n', '<Leader>J', 'J:s/\\s\\+//<CR>', { noremap = true, silent = true })

--vertically navigation with keep cursor on center
vim.api.nvim_set_keymap("n", "<C-u>", "<C-u>zz", { noremap = true, silent = true }) -- Mapping <C-u> to <C-u>zz
vim.api.nvim_set_keymap("n", "<C-d>", "<C-d>zz", { noremap = true, silent = true }) -- Mapping <C-d> to <C-d>zz

--find
vim.api.nvim_set_keymap("n", "n", "nzz", { noremap = true, silent = true }) -- Mapping n to nzz
vim.api.nvim_set_keymap("n", "N", "Nzz", { noremap = true, silent = true }) -- Mapping N to Nzz

--vim.keymap.set('n', '<leader>a', 'ggVG')
vim.api.nvim_set_keymap("n", "<leader>\\", ":vsplit | wincmd l<CR>", { noremap = true, silent = true, desc = "VSplit" })
vim.api.nvim_set_keymap("n", "<leader>|", ":split | wincmd j<CR>", { noremap = true, silent = true, desc = "HSplit" })

-- standard vim quickfix (ad default replaced to trouble.nvim)
map("n", "<leader>xL", "<cmd>lopen<cr>", { desc = "Location List" })
map("n", "<leader>xQ", "<cmd>copen<cr>", { desc = "Quickfix List" })

-- map("n", "<S-h>", "^", { desc = "Prev Buffer" })
-- map("n", "<S-l>", "$", { desc = "Next Buffer" })

-- Snacks
Snacks.toggle.zen():map("<leader>zz")

--move selected block
--vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
--vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

--yank(copy) to system clipboard
--vim.keymap.set({ "n", "v" }, "<leader>y", "\"+yy")
--vim.keymap.set("n", "<leader>Y", "\"+y")

--start replacing word under cursonr
--vim.keymap.set("n", "<leader>r", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
--    { desc = "Start replacing word under cursor" })

-- Function to create a visual mode substitution with prompt on every replace
-- help to replace intellij - replace qualified name with import

-- stylua: ignore start
vim.api.nvim_set_keymap( "v", "<leader>rr", '"hy:%s/<C-r>h//gc<Left><Left><Left>', { noremap = true, silent = false, desc = "Replace with prompt" })

-- run lua in runtime
map("n", "<space>rs", "<cmd>source %<CR>", { desc = "Run lua current file" })
-- map("n", "<space>rl", ":.lua<CR>", { desc = "Run lua current line" })
-- map("v", "<space>rl", ":lua<CR>", { desc = "Run lua selected code" })
map("n", "<space>rl", function() Snacks.debug.run() end, { desc = "Run current lua buffer (file)" })
map("v", "<space>rl", function() Snacks.debug.run() end, { desc = "Run lua selected code" })

-- mouse horisontal scrolling
vim.api.nvim_set_keymap("n", "<S-ScrollWheelUp>", "5zh", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<S-ScrollWheelDown>", "5zl", { noremap = true, silent = true })

-- maven compile
vim.api.nvim_set_keymap("n", "<leader><F9>", ":MavenCompile<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader><F10>", ":MavenCleanCompile<CR>", { noremap = true, silent = true })

vim.opt.path:append("**")

map("v", "<leader>xp", function() require("utils.java.java-trace").parse_selected_trace_to_qflist() end, { desc = "Parse trace to quick fix list" })
map("n", "<leader>xp", function() require("utils.java.java-trace").parse_buffer_trace_to_qflist() end, { desc = "Parse trace to quick fix list" })
-- stylua: ignore end

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
        default_text = fileName,
    })
end, { desc = "Find word under curser in lsp dynamic_workspace_symbols" })

-- -- debug purposes
-- _G.log_table = function(table)
--     require("utils.common-util").log_table(table)
-- end

--local buff_utils = require("utils.buffer-util")
--_G.get_goto_preview_buffers = function()
--    log_table(buff_utils.get_active_ls_buffers())
--end
