return {
    {
        "folke/noice.nvim",
        dependencies = {
            -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
            "MunifTanjim/nui.nvim",
            -- OPTIONAL:
            --   `nvim-notify` is only needed, if you want to use the notification view.
            --   If not available, we use `mini` as the fallback
            "rcarriga/nvim-notify",
        },
        opts = {
            presets = {
                bottom_search = false, -- use a classic bottom cmdline for search
                command_palette = false, -- position the cmdline and popupmenu together
                long_message_to_split = true, -- long messages will be sent to a split
                inc_rename = false, -- enables an input dialog for inc-rename.nvim
                lsp_doc_border = true, -- add a border to hover docs and signature help
            },
            lsp = {
                hover = {
                    silent = true, -- set to true to not show a message if hover is not available
                },
                signature = {
                    auto_open = {
                        enabled = false, -- in favor of blink.cmp
                    },
                },
            },
            notify = {
                enabled = false,
            },
            -- not needed in case blink.cmp
            -- lsp = {
            --     hover = {
            --         -- I'm using self customized config, see lsp.lua
            --         enabled = false,
            --     },
            --     --[[override = {
            --         ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
            --         ["vim.lsp.util.stylize_markdown"] = true,
            --         ["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
            --     },]]
            -- },
        },
    },
}