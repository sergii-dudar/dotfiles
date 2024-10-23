return {
    {
        "catppuccin/nvim",
        name = "catppuccin",
        priority = 1000,
        optional = false,
        lazy = false,
        opts = {
            custom_highlights = function()
                return {
                    Visual = { bg = "#373267" },
                }
            end,
        },
        --[[opts = {
          term_colors = true,
          transparent_background = true
      }]]
    },
    --[[{
      'rose-pine/neovim',
      name = 'rose-pine',
      priority = 1000,
      optional = false,
      lazy = false,
      opts = { highlight_groups = { Visual = { bg = '#373267' } } },
  },]]
    --[[{
      "rebelot/kanagawa.nvim",
      priority = 1000
  },]]
    {
        "LazyVim/LazyVim",
        opts = {
            colorscheme = "catppuccin-mocha",
        },
    },
    --{
    --  "sainnhe/sonokai",
    --  priority = 1000, -- Ensure it loads first
    --}
    --{ "ellisonleao/gruvbox.nvim", priority = 1000}
}
