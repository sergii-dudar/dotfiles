-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local map = LazyVim.safe_keymap_set

--Clear highlights on search when pressing <Esc> in normal mode
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Quick diff conflict resolution
-- vim.keymap.set("n", "<leader>d1", ":diffget 1<CR>", { desc = "get from left [LOCAL]" }) -- get from left [LOCAL]
-- vim.keymap.set("n", "<leader>d2", ":diffget 2<CR>", { desc = "get from right [REMORE]" }) -- get from right [REMORE]
-- vim.keymap.set("n", "<leader>d3", ":diffget 3<CR>", { desc = "get from right [REMORE]" }) -- get from right [REMORE]
-- vim.keymap.set("n", "<leader>dl", ":diffget 3<CR>") -- get from right [REMORE]

-- disable start recording of macros (I'm not using it a lot, but often conflicting with another plugins on `q`, that quite anoying)
vim.keymap.set("n", "q", "<Nop>", { noremap = true, silent = true })

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
-- vim.keymap.set("n", "<C-a>", "gg<S-v>G")
-- vim.keymap.set("n", "<leader>A", "gg<S-v>G")
vim.keymap.set("n", "<C-A>", "gg<S-v>G")

vim.keymap.set("n", "<leader>qq", "<cmd>q<cr>", { desc = "Quit" })
vim.keymap.set("n", "<leader>qQ", "<cmd>qa<cr>", { desc = "Quit All" })
--vim.keymap.set("n", "<leader>qW", "<cmd>wa<cr>", { desc = "Save All" })
--vim.keymap.set("n", "<leader>qW", "<cmd>wqa<cr>", { desc = "Save all Quit" })

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

-- replace currently selected text with default register without yanking it
vim.keymap.set({ "v" }, "<leader>p", '"_dP', { desc = "Replace selected from default register", silent = true })

-- paste from 0 register (where always live last yanked text, which is immune to being overwritten by deletions.)
-- INFO: using `c` (change) in exchange is the key.
vim.keymap.set({ "i" }, "<C-r>", "<C-r>0", { noremap = true, silent = true })

-- move cursor in insert mode, convenient in case need move in brackets or parentheses without leaving insert mode
vim.keymap.set("i", "<C-l>", "<Right>", { noremap = true })
vim.keymap.set("i", "<C-h>", "<Left>", { noremap = true })

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

-- Window resize with bigger step (20)
map("n", "<C-W>]", "<cmd>resize +20<cr>", { desc = "Increase height", noremap = true })
map("n", "<C-W>[", "<cmd>resize -20<cr>", { desc = "Decrease height", noremap = true })
map("n", "<C-W>.", "<cmd>vertical resize +20<cr>", { desc = "Increase width", noremap = true })
map("n", "<C-W>,", "<cmd>vertical resize -20<cr>", { desc = "Decrease width", noremap = true })

-- standard vim quickfix (ad default replaced to trouble.nvim)
map("n", "<leader>xL", "<cmd>lopen<cr>", { desc = "Location List" })
map("n", "<leader>xQ", "<cmd>copen<cr>", { desc = "Quickfix List" })

map("n", "<leader>??", function()
    local path = vim.fn.glob("$HOME/.config/nvim/cheat_sheet/vim_cheat_sheet.md")
    vim.cmd("sp " .. path)
    vim.schedule(function()
        local bufnr = vim.api.nvim_get_current_buf()
        vim.keymap.set("n", "q", function()
            vim.api.nvim_buf_delete(bufnr, { force = true })
        end, { buffer = bufnr, desc = "Close" })
    end)
end, { desc = "Personal Vim Cheat Sheet" })

-- map("n", "<S-h>", "^", { desc = "Prev Buffer" })
-- map("n", "<S-l>", "$", { desc = "Next Buffer" })

-- Snacks
Snacks.toggle.zen():map("<leader>zz")

vim.keymap.set("n", "<leader>N", function()
    Snacks.scratch({ name = "CWD Notes", ft = "txt" })
end, { desc = "CWD Scratch Notes", noremap = true, silent = true }) -- Mapping J to 6jzz

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
vim.api.nvim_set_keymap("v", "<leader>R", [["hy:%s/\V<C-r>=escape(@h, '/\')<CR>//gc<Left><Left><Left>]], { noremap = true, silent = false, desc = "Replace with prompt" })
-- vim.api.nvim_set_keymap("v", "<leader>R", '"hy:%s/<C-r>h//gc<Left><Left><Left>', { noremap = true, silent = false, desc = "Replace with prompt" })
-- vim.api.nvim_set_keymap("v", "<leader>a/", [["hy/\V<C-r>=escape(@h, '/\')<CR><CR>]], { noremap = true, silent = false, desc = "Search selection literally" })

-- run lua in runtime (in all buffers!)
-- map("n", "<space>rs", "<cmd>source %<CR>", { desc = "Run lua current file" })
-- -- -- map("n", "<space>rl", ":.lua<CR>", { desc = "Run lua current line" })
-- -- -- map("v", "<space>rl", ":lua<CR>", { desc = "Run lua selected code" })
-- map("n", "<space>rV", function() Snacks.debug.run() end, { desc = "Run current lua buffer (file)" })
-- map("v", "<space>rl", function() Snacks.debug.run() end, { desc = "Run lua selected code" })

-- mouse horisontal scrolling
vim.api.nvim_set_keymap("n", "<S-ScrollWheelUp>", "5zh", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<S-ScrollWheelDown>", "5zl", { noremap = true, silent = true })
-- stylua: ignore end

-- Open a search prompt prefilled with the clipboard, escaping "/" and "\" so e.g.
-- "folke/noice.nvim" becomes "folke\/noice.nvim" and searches literally for the slash.
-- (Terminal intercepts <C-v> as paste, so we prefill instead of mapping inside the prompt.)
vim.keymap.set("n", "g/", function()
    local text = vim.fn.escape((vim.fn.getreg("+"):gsub("\n", "")), "/\\")
    vim.fn.feedkeys("/" .. text, "n")
end, { desc = "Search clipboard (slash-escaped)" })

------------------------------------------------------------
-------------- convert Java toString() to JSON -------------

vim.keymap.set("v", "<leader>Cjj", function()
    -- Exit visual mode to set '< '> marks
    local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
    vim.api.nvim_feedkeys(esc, "x", false)
    require("utils.java.java-tostring-parser").convert_selection(false)
end, { desc = "Convert Java toString to JSON (replace)" })
vim.keymap.set("v", "<leader>Cjy", function()
    local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
    vim.api.nvim_feedkeys(esc, "x", false)
    require("utils.java.java-tostring-parser").convert_selection(true)
end, { desc = "Convert Java toString to JSON (clipboard)" })
vim.keymap.set("n", "<leader>Cjn", function()
    require("utils.json.normalize").normalize_buffer()
end, { desc = "Convert json to normalized" })
vim.keymap.set("v", "<leader>Cjn", function()
    local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
    vim.api.nvim_feedkeys(esc, "x", false)
    require("utils.json.normalize").normalize_selection()
end, { desc = "Convert selected json to normalized" })
