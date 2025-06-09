return {
    {
        "saghen/blink.cmp",
        -- Disabled for now and for now not very stable, switched back to nvim-cmp for now
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
            completion = {
                menu = {
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
            },
            signature = { window = { border = "rounded" } },
            cmdline = {
                enabled = true,
                -- keymap = { preset = "inherit" },
                -- completion = { menu = { auto_show = true } },
            },
            -- sources = {
            --     -- adding any nvim-cmp sources here will enable them
            --     -- with blink.compat
            --     compat = {},
            --     default = { "lsp", "path", "snippets", "buffer" },
            --     --cmdline = { "cmdline" },
            --     -- cmdline = function()
            --     --     local type = vim.fn.getcmdtype()
            --     --     -- Search forward and backward
            --     --     if type == "/" or type == "?" then
            --     --         return { "buffer" }
            --     --     end
            --     --     -- Commands
            --     --     if type == ":" then
            --     --         return { "cmdline" }
            --     --     end
            --     --     return {}
            --     -- end,
            -- },
        },

        -- {
        --     -- 'default' for mappings similar to built-in completion
        --     -- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
        --     -- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
        --     -- see the "default configuration" section below for full documentation on how to define
        --     -- your own keymap.
        --     keymap = { preset = "default" },
        --
        --     appearance = {
        --         -- Sets the fallback highlight groups to nvim-cmp's highlight groups
        --         -- Useful for when your theme doesn't support blink.cmp
        --         -- will be removed in a future release
        --         use_nvim_cmp_as_default = true,
        --         -- Set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
        --         -- Adjusts spacing to ensure icons are aligned
        --         nerd_font_variant = "mono",
        --     },
        --
        --     -- default list of enabled providers defined so that you can extend it
        --     -- elsewhere in your config, without redefining it, via `opts_extend`
        --     sources = {
        --         default = { "lsp", "path", "snippets", "buffer" },
        --         -- optionally disable cmdline completions
        --         -- cmdline = {},
        --     },
        --
        --     -- experimental signature help support
        --     -- signature = { enabled = true }
        -- },
    },
}