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
                bottom_search = false,
                command_palette = false,
                long_message_to_split = true,
                lsp_doc_border = true,
            },
            lsp = {
                hover = {
                    -- I'm using self customized config, see lsp.lua
                    enabled = false
                },
                --[[override = {
                    ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                    ["vim.lsp.util.stylize_markdown"] = true,
                    ["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
                },]]
            }
        }
    }
}