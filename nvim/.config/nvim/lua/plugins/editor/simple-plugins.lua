return {
    --{ "mg979/vim-visual-multi" },
    {
        -- "norcalli/nvim-colorizer.lua",
        "catgoose/nvim-colorizer.lua", -- fork with support of AARRGGBB and another color formats
        config = function()
            require("colorizer").setup({
                user_default_options = {
                    RRGGBBAA = true, -- #RRGGBBAA hex codes
                    AARRGGBB = true, -- 0xAARRGGBB hex codes
                },
            })
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
    {
        "folke/flash.nvim",
        event = "VeryLazy",
        opts = {
            modes = {
                char = {
                    -- jump_labels = true, # f,F,t,T...
                    -- jump = {
                    --     autojump = true,
                    -- },
                },
            },
            search = {
                enabled = true,
            },
            -- jump = {
            --     -- automatically jump when there is only one match
            --     autojump = true,
            -- },
        },
    },
}
