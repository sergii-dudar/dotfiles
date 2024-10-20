return {
  {
      "catppuccin/nvim",
      --[[opts = {
          term_colors = true,
          transparent_background = true
      }]]
  },
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