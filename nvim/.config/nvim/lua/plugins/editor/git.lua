return {
    {
        "lewis6991/gitsigns.nvim",
        keys = {
            { "<leader>gt", ":Gitsigns toggle_current_line_blame<CR>", desc = "Toggle Line Blame" },
        },
    },
    -- https://github.com/chojs23/ec
    -- very perspective with separated TUI tool to resolve conflics, can be runned in shell directly by `ec`
    {
        "chojs23/ec",
        keys = {
            { "<leader>gr", ":Ec<CR>", desc = "Open ec (easy-conflict)" },
        },
    },
    -- https://github.com/StackInTheWild/headhunter.nvim
    -- another simple nice plugin to resolve merge conflicts.
    --[[ {
        "StackInTheWild/headhunter.nvim",
        config = function()
            require("headhunter").setup()
        end,
    }, ]]
    -- https://github.com/spacedentist/resolve.nvim
    -- simple nice plugin to resolve merge conflicts.
    --[[ {
        "spacedentist/resolve.nvim",
        event = { "BufReadPre", "BufNewFile" },
        opts = {},
    }, ]]
    -- {
    --     "sindrets/diffview.nvim",
    --     version = "*",
    --     config = true,
    -- },

    -- {
    --     "akinsho/git-conflict.nvim",
    --     version = "*",
    --     config = true,
    -- },
}
