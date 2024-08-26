return {
    {
        "catppuccin/nvim",
        lazy = false,
        name = "catppuccin",
        priority = 1000,
        config = function()
            -- require("catppuccin").setup({
            --     transparent_background = true
            -- })
            vim.cmd.colorscheme "catppuccin-mocha"
            -- vim.cmd.colorscheme "catppuccin-macchiato"
            -- vim.cmd.colorscheme "catppuccin-frappe"
            -- vim.cmd.colorscheme "catppuccin-latte"
        end
    }
}

--return {
--    'sainnhe/gruvbox-material',
--    lazy = false,
--    priority = 1000,
--    config = function()
--        -- Optionally configure and load the colorscheme
--        -- directly inside the plugin declaration.
--        vim.g.gruvbox_material_enable_italic = true
--        vim.cmd.colorscheme('gruvbox-material')
--    end
--}

--return {
--  "folke/tokyonight.nvim",
--  lazy = false,
--  priority = 1000,
--  opts = {},
--  config = function ()
--       -- vim.cmd.colorscheme "tokyonight"
--       -- vim.cmd.colorscheme "tokyonight-night"
--       -- vim.cmd.colorscheme "tokyonight-storm"
--       vim.cmd.colorscheme "tokyonight-moon"
--  end
--}