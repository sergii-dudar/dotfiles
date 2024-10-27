return {
    'L3MON4D3/LuaSnip',
    -- disabled for now, as I have found there not best integration with lsp cmp
    enabled = false,
    config = function()
        require("luasnip.loaders.from_vscode").lazy_load({paths = "~/.config/nvim/snippets"})
        require("plugins.luasnip.snippets.java").setup()
    end,
}
