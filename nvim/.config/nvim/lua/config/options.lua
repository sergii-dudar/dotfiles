-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

local augroup = vim.api.nvim_create_augroup
local myCustomGroup = augroup('myCustomStartupGroup', {})

-- by some reasons not firing from `autocmds`
vim.api.nvim_create_autocmd("UiEnter", {
    desc = "Open Neotree automatically",
    group = myCustomGroup,
    --pattern = { "*" },
    callback = function()

        -- restore current proj session
        require("persistence").load()

        if vim.fn.argc() == 0 then
            vim.cmd("Neotree filesystem reveal left")
        end
    end,
})