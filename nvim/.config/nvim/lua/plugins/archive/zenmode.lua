return {
    "folke/zen-mode.nvim",
    enabled = false,
    config = function()
        vim.keymap.set("n", "<leader>zz", function()
            require("zen-mode").setup({
                window = {
                    width = 0.90,
                    options = {},
                },
            })
            require("zen-mode").toggle()
            vim.wo.wrap = false
            vim.wo.number = true
            vim.wo.rnu = true
        end, { desc = "toggle zen-mode" })
    end,
}
