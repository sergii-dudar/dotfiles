-- Autocommands
local augroup = vim.api.nvim_create_augroup
local customBuffer = augroup("custom_buffer", { clear = true })
local myCustomGroup = augroup('myCustomGroup', {})
local yank_group = augroup('HighlightYank', {})

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

-- auto indent on save
--autocmd("BufWritePre", {
--    pattern = "*",
--    command = "normal! gg=G",
--})

autocmd("BufWritePre", {
    pattern = "*",
    callback = function()
        vim.lsp.buf.format({ async = false })
    end,
})

--autocmd("VimEnter", {
--    pattern = "*",
--    group = "custom_buffer",
--    callback = function()
--        vim.cmd("Neotree filesystem reveal left")
--    end,
--})

autocmd("UiEnter", {
    desc = "Open Neotree automatically",
    group = myCustomGroup,
    callback = function()
        if vim.fn.argc() == 0 then
            --vim.cmd "Neotree toggle"
            vim.cmd("Neotree filesystem reveal left")
        end
    end,
})

autocmd('LspAttach', {
    group = myCustomGroup,
    callback = function(e)
        local opts = { buffer = e.buf }
        vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
        vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
        vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end, opts)
        vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, opts)
        vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end, opts)
        vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end, opts)
        vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, opts)
        vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)
        vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
        vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
    end
})