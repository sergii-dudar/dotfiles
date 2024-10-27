return {
    'L3MON4D3/LuaSnip',
    config = function()
        require("luasnip.loaders.from_vscode").lazy_load({paths = "~/.config/nvim/snippets"})
        require("plugins.luasnip.snippets.java").setup()
    end,
}
