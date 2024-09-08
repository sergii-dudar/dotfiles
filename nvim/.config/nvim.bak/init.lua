local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

--require("core.vim-options")
--require("core.keymaps")
--require("core.autocmds")

require("core")
require("lazy").setup({
    --{ import = "plugins" },
    { import = "plugins.code" },
    { import = "plugins.git" },
    { import = "plugins.lsp" },
    { import = "plugins.navigation" },
    { import = "plugins.ui" }
})
