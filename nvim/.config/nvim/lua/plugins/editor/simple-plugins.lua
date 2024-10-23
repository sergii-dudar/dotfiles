return {
    { "mg979/vim-visual-multi" },
    {
        "norcalli/nvim-colorizer.lua",
        cmd = { "ColorizerToggle" },
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
