vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.shiftwidth = 8 --8, need create custom indentexpr to handle correctly where 8, and where 4
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.breakindent = true

-- disabled autoformat to work with dwm
vim.b.autoformat = false

-- vim.api.nvim_create_autocmd({ "FileType" }, {
--   pattern = { "c" },
--   callback = function()
--     vim.b.autoformat = false
--   end,
-- })
