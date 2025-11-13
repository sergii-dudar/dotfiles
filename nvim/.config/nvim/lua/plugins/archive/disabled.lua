return {
    -- disable trouble
    --{ "folke/trouble.nvim", enabled = false },
    --{ "lukas-reineke/indent-blankline.nvim", enabled = false }
    --{ "folke/ts-comments.nvim", enabled = false }
    {
        "akinsho/toggleterm.nvim",
        --opts = {--[[ things you want to change go here]]}
        enabled = false,
        config = function()
            require("toggleterm").setup()
        end,
    },
}
