-- https://github.com/folke/snacks.nvim/blob/main/docs/debug.md
_G.dd = function(...)
    -- Show a notification with a pretty printed dump of the object(s) with lua treesitter highlighting and the location of the caller
    Snacks.debug.inspect(...)
end
_G.bt = function()
    -- Show a notification with a pretty backtrace
    Snacks.debug.backtrace()
end
vim.print = _G.dd
require("utils.global-util")

-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")

-- Snacks.debug.inspect("Testing", 123, nil, { dsf = "sdfsd" })
