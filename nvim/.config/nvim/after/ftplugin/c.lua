-- this config as for c, h, c++ etc

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.shiftwidth = 8 --8, need create custom indentexpr to handle correctly where 8, and where 4
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.breakindent = true

local current_file = vim.fn.expand("%:p")
local list_util = require("utils.list-util")
local ignore_dirs = {
    "dwm",
    --"myforks/my%-dwm",
    "dotfiles/suckless",
    -- Add more directories as needed
}
if list_util.any_match(current_file, ignore_dirs) then
    -- disabled autoformat to work with dwm
    vim.b.autoformat = false
    vim.diagnostic.enable(false)
end

-- vim.api.nvim_create_autocmd({ "FileType" }, {
--   pattern = { "c" },
--   callback = function()
--     vim.b.autoformat = false
--   end,
-- })
