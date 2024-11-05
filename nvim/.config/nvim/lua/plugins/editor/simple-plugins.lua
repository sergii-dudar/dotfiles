return {
    { "mg979/vim-visual-multi" },
    {
        "norcalli/nvim-colorizer.lua",
        config = function()
            require("colorizer").setup()
        end,
    },
    {
        "akinsho/toggleterm.nvim",
        --opts = {--[[ things you want to change go here]]}
        enabled = false,
        config = function()
            require("toggleterm").setup()
        end,
    },
}
