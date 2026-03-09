-- Markdown concealment settings
--
-- THE ISSUE: Long concealed URLs reserve space, creating empty gaps
-- When wrap=true: Creates empty wrapped lines
-- When wrap=false: Creates horizontal empty space
--
-- ROOT CAUSE: Vim's concealment ALWAYS reserves space for hidden text.
-- This is hardcoded behavior that cannot be changed.
--
-- YOUR OPTIONS:
-- 1. Disable concealment: conceallevel = 0 (see full URLs, no gaps)
-- 2. Disable wrapping: wrap = false (long URLs scroll horizontally, no wrapped gaps)
-- 3. Enable concealment + wrapping: Accept the empty gaps
-- 4. Use :MarkdownPreview to view in browser

-- vim.opt_local.conceallevel = 3 -- Enable concealment
-- vim.opt_local.concealcursor = "nvic" -- Show full link when cursor on line
-- vim.opt_local.wrap = false -- Disable wrapping to avoid empty wrapped lines
-- vim.opt_local.linebreak = false -- no soft-wrap at words
-- vim.opt_local.showbreak = "↪ " -- optional marker when scrolling horizontally

-- If you prefer wrapping despite the gaps:
-- vim.opt_local.wrap = true
-- vim.opt_local.linebreak = true

-- qadzek/link.vim
vim.keymap.set("n", "<leader>jj", ":LinkJump<CR>", { buffer = true, desc = "Jump to Link (link.vim)" })
vim.keymap.set("n", "<leader>jo", ":LinkOpen<CR>", { buffer = true, desc = "Open Link (link.vim)" })