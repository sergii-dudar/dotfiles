return {
    "akinsho/bufferline.nvim",
    keys = {
        { "<Tab>", "<Cmd>BufferLineCycleNext<CR>", desc = "Next tab" },
        { "<S-Tab>", "<Cmd>BufferLineCyclePrev<CR>", desc = "Prev tab" },
    },
    opts = {
        options = {
            separator_style = 'slant', --'thick'
            show_buffer_close_icons = false,
            show_close_icon = false,
            always_show_bufferline = true,
            show_tab_indicators = false,
            --show_duplicate_prefix = false
        }
    }
}