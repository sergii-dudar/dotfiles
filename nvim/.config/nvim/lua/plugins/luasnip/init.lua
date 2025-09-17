local register_snippets = function()
    local languages = {
        -- "lua",
        "javascript",
        "yaml",
        "java",
        "rust",
        "bash",
        "markdown",
    }

    for _, language in ipairs(languages) do
        local module_path = string.format("plugins.luasnip.snippets.%s", language)

        local ok, snip = pcall(require, module_path)

        if ok then
            snip.setup()
        end
    end
end

return {
    "L3MON4D3/LuaSnip",
    dependencies = {
        "s1n7ax/nvim-snips",
        "nvim-treesitter/nvim-treesitter",
        "s1n7ax/nvim-ts-utils",
    },
    config = function()
        require("luasnip.loaders.from_vscode").lazy_load({ paths = "~/.config/nvim/snippets" })
        register_snippets()
    end,
}
