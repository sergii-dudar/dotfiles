-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

local augroup = vim.api.nvim_create_augroup
local customBuffer = augroup("custom_buffer", { clear = true })
local myCustomGroup = augroup("myCustomGroup", {})
local yank_group = augroup("HighlightYank", {})

local autocmd = vim.api.nvim_create_autocmd

-- start terminal in insert mode
autocmd("TermOpen", {
  desc = "Auto enter insert mode when opening a terminal",
  group = customBuffer,
  pattern = "*",
  callback = function()
    -- Wait briefly just in case we immediately switch out of the buffer (e.g. Neotest)
    vim.defer_fn(function()
      if vim.api.nvim_buf_get_option(0, "buftype") == "terminal" then
        vim.cmd([[startinsert]])
      end
    end, 100)
  end,
})

-- highlight yanks
autocmd("TextYankPost", {
  group = yank_group,
  pattern = "*",
  callback = function()
    vim.highlight.on_yank({ timeout = 300 })
  end,
})

--autocmd("BufWritePre", {
--    pattern = "*",
--    callback = function()
--        vim.lsp.buf.format({ async = false })
--    end,
--})

--print("UiEnter1")
--vim.api.nvim_create_autocmd("VimEnter", {
--    callback = function()
--        -- Open Neotree automatically
--        require("neo-tree.command").execute({ toggle = true, dir = vim.loop.cwd() })
--    end
--})
