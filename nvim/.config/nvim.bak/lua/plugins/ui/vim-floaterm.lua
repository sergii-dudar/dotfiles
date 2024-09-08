return {
    "voldikss/vim-floaterm",
    enabled = false, -- not figured out yet if will use it
    config = function()
        vim.keymap.set("n", "<leader>tf", "<Cmd>FloatermNew<CR>", {})
        vim.keymap.set("n", "<leader>tt", "<Cmd>FloatermToggle<CR>", {})
        vim.keymap.set("n", "<leader>th", "<Cmd>FloatermHide<CR>", {})
    end
}