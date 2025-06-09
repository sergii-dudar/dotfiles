return {
    {
        "saghen/blink.cmp",
        enabled = true,
        dependencies = {
            --"hrsh7th/cmp-cmdline",
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
            },
        },
    },
}
