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
    -- stylua: ignore
    config = function()
        require("luasnip.loaders.from_vscode").lazy_load({ paths = "~/.config/nvim/snippets" })
        register_snippets()

        local ls = require("luasnip")

        -- vim.keymap.set({"i"}, "<C-K>", function() ls.expand() end, {silent = true})
        -- vim.keymap.set({"i", "s"}, "<C-L>", function() ls.jump( 1) end, {silent = true})
        -- vim.keymap.set({"i", "s"}, "<C-J>", function() ls.jump(-1) end, {silent = true})

        vim.keymap.set({ "i", "s" }, "<C-E>", function()
            if ls.choice_active() then
                ls.change_choice(1)
            end
        end, { silent = true })

        vim.keymap.set({ "i", "s" }, "<C-R>", function()
            if ls.choice_active() then
                require("luasnip.extras.select_choice")()
            end
        end, { silent = true })
    end,
}
