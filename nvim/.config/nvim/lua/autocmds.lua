-- Autocommands
vim.api.nvim_create_augroup("custom_buffer", { clear = true })

-- start terminal in insert mode
vim.api.nvim_create_autocmd("TermOpen", {
    desc = "Auto enter insert mode when opening a terminal",
    group = "custom_buffer",
    pattern = "*",
    callback = function()
        -- Wait briefly just in case we immediately switch out of the buffer (e.g. Neotest)
        vim.defer_fn(function()
            if vim.api.nvim_buf_get_option(0, 'buftype') == 'terminal' then
                vim.cmd([[startinsert]])
            end
        end, 100)
    end,
})

-- highlight yanks
vim.api.nvim_create_autocmd("TextYankPost", {
    group = "custom_buffer",
    pattern = "*",
    callback = function() vim.highlight.on_yank { timeout = 300 } end
})

-- auto indent on save
--vim.api.nvim_create_autocmd("BufWritePre", {
--    pattern = "*",
--    command = "normal! gg=G",
--})

vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    callback = function()
        vim.lsp.buf.format({ async = false })
    end,
})
