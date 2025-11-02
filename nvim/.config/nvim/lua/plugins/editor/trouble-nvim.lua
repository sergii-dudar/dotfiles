return {
    "folke/trouble.nvim",
    opts = {
        -- preview = {
        --     type = "float",
        --     relative = "editor",
        --     border = "rounded",
        --     title = "Preview",
        --     title_pos = "center",
        --     position = { 0, -2 },
        --     size = { width = 0.3, height = 0.3 },
        --     zindex = 200,
        -- },
        preview = {
            type = "split",
            relative = "win",
            position = "right",
            size = 0.4,
        },
        modes = {
            lsp = {
                --win = { position = "right" },
                win = { position = "bottom" },
            },
            symbols = {
                --win = { position = "right" },
                win = { position = "bottom" },
            },
        },
        auto_close = true, -- auto close when there are no items
        auto_preview = true, -- automatically open preview when on an item
        auto_refresh = true, -- auto refresh when open
        auto_jump = true, -- auto jump to the item when there's only one
        focus = true, -- Focus the window when opened
    },
    keys = {
        { "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix List (Trouble)" },
        { "<leader>xl", "<cmd>Trouble loclist toggle<cr>", desc = "Location List (Trouble)" },
        { "<leader>xL", "<cmd>lopen<cr>", { desc = "Location List" } },
        { "<leader>xQ", "<cmd>copen<cr>", { desc = "Quickfix List" } },
    },
}
