return {
    {
        "hrsh7th/cmp-nvim-lsp",
        enabled = false,
    },
    {
        "L3MON4D3/LuaSnip",
        dependencies = {
            "saadparwaiz1/cmp_luasnip",
            "rafamadriz/friendly-snippets",
        },
        enabled = false,
    },
    {
        "hrsh7th/nvim-cmp",
        enabled = false,
        config = function()
            local cmp = require("cmp")
            require("luasnip.loaders.from_vscode").lazy_load()

            local cmp_select = { behavior = cmp.SelectBehavior.Select }
            cmp.setup({
                snippet = {
                    expand = function(args)
                        require("luasnip").lsp_expand(args.body)
                    end,
                },
                window = {
                    completion = cmp.config.window.bordered(),
                    documentation = cmp.config.window.bordered(),
                },
                mapping = cmp.mapping.preset.insert({
                    -- ["<C-n>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
                    -- ["<C-p>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
                    ["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
                    ["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
                    ["<C-y>"] = cmp.mapping.confirm({ select = true }),

                    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-f>"] = cmp.mapping.scroll_docs(4),
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<C-e>"] = cmp.mapping.abort(),
                    ["<CR>"] = cmp.mapping.confirm({ select = true }),
                    -- ["<C-j>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
                    -- ["<C-k>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
                    -- ['<Tab>'] = function(fallback)
                    --     if cmp.visible() then
                    --         cmp.confirm()
                    --     else
                    --         fallback() -- If you use vim-endwise, this fallback will behave the same as vim-endwise.
                    --     end
                    -- end,
                    -- ['<S-Tab>'] = function(fallback)
                    --     if cmp.visible() then
                    --         cmp.select_next_item()
                    --     else
                    --         fallback()
                    --     end
                    -- end
                }),
                sources = cmp.config.sources({
                    -- { name = "nvm_lua" },
                    -- { name = "nvim_lsp" },
                    { name = "luasnip" }, -- For luasnip users.
                }, {
                    { name = "buffer" },
                }),
            })
        end,
    },
}