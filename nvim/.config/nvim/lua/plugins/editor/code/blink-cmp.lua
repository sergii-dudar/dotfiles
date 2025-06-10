return {
    {
        "saghen/blink.cmp",
        enabled = true,
        dependencies = {
            --"hrsh7th/cmp-cmdline",
            "nvim-tree/nvim-web-devicons",
        },
        opts = {
            keymap = {
                preset = "enter",
                -- ["<Tab>"] = {
                --     LazyVim.cmp.map({ "snippet_forward", "ai_accept" }),
                --     "fallback",
                -- },
                ["<C-k>"] = { "select_prev", "fallback" },
                ["<C-j>"] = { "select_next", "fallback" },
                ["<Space>"] = { "accept", "fallback" },
            },
            fuzzy = {
                implementation = "rust", -- prefer_rust_with_warning(default)|prefer_rust|rust|lua
            },
            completion = {
                menu = {
                    auto_show = true,
                    scrollbar = false,
                    border = "rounded",
                    draw = {
                        columns = {
                            { "kind_icon" },
                            { "label", "label_description", gap = 1 },
                            { "kind" },
                            { "source_name" },
                            -- { "source_id" },
                        },
                    },
                },

                documentation = {
                    auto_show = true,
                    window = {
                        border = "rounded",
                    },
                },
                ghost_text = {
                    enabled = true,
                    show_with_menu = true,
                },
            },
            -- Experimental signature help support
            signature = {
                enabled = true,
                window = { border = "rounded" },
            },
            cmdline = {
                keymap = {
                    -- recommended, as the default keymap will only show and select the next item
                    -- ["<Tab>"] = { "show", "accept" },
                    -- ["<Tab>"] = { "accept" },
                    ["<CR>"] = { "accept_and_enter", "fallback" },
                    ["<C-k>"] = { "select_prev", "fallback" },
                    ["<C-j>"] = { "select_next", "fallback" },
                    ["<Space>"] = { "accept", "fallback" },
                    -- ["<Space>"] = {
                    --     "accept",
                    --     function(cmp)
                    --         vim.cmd("normal! i ")
                    --     end,
                    --     "fallback",
                    -- },
                },
                enabled = true,
                -- keymap = { preset = "inherit" },
                -- completion = { menu = { auto_show = true } },
            },
            sources = {
                providers = {
                    path = {
                        opts = {
                            show_hidden_files_by_default = true,
                        },
                    },
                },
                -- default = { "lsp", "path", "snippets", "buffer" },
            },
            -- snippets = { preset = 'default' | 'luasnip' | 'mini_snippets' },
            snippets = { preset = "luasnip" },
        },
    },
}
