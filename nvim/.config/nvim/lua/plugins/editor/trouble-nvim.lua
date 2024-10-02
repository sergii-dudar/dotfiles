return {
    "folke/trouble.nvim",
    opts = {
        --preview = {
        --    type = "float",
        --    relative = "editor",
        --    border = "rounded",
        --    title = "Preview",
        --    title_pos = "center",
        --    position = { 0, -2 },
        --    size = { width = 0.3, height = 0.3 },
        --    zindex = 200,
        --},
        preview = {
            type = "split",
            relative = "win",
            position = "right",
            size = 0.3,
        },
        modes = {
            lsp = {
                --win = { position = "right" },
                win = { position = "bottom" },
            },
            symbols = {
                --win = { position = "right" },
                win = { position = "bottom" },
            }
        }
    },
    keys = {
        { "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix List (Trouble)" },
        { "<leader>xl", "<cmd>Trouble loclist toggle<cr>", desc = "Location List (Trouble)" },
        { "<leader>xL", "<cmd>lopen<cr>", { desc = "Location List" } },
        { "<leader>xQ", "<cmd>copen<cr>", { desc = "Quickfix List" } }
    }
}