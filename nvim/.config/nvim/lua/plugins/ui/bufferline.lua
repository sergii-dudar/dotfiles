return {
    "akinsho/bufferline.nvim",
    keys = {
        { "<Tab>", "<Cmd>BufferLineCycleNext<CR>", desc = "Next tab" },
        { "<S-Tab>", "<Cmd>BufferLineCyclePrev<CR>", desc = "Prev tab" },
    },
    opts = {
        options = {
            separator_style = "thick", --thin, thick, slant
            show_buffer_close_icons = false,
            show_close_icon = false,
            always_show_bufferline = true,
            show_tab_indicators = false,
            auto_toggle_bufferline = true,
            tab_size = 8,
            indicator = {
                -- icon = "",
                icon = "󰜴",
                style = "icon", -- 'icon' | 'underline' | 'none'
            },

            -- name_formatter = function(buf) -- buf contains:
            --     return buf.name .. "  "
            --     -- name                | str        | the basename of the active file
            --     -- path                | str        | the full path of the active file
            --     -- bufnr               | int        | the number of the active buffer
            --     -- buffers (tabs only) | table(int) | the numbers of the buffers in the tab
            --     -- tabnr (tabs only)   | int        | the "handle" of the tab, can be converted to its ordinal number using: `vim.api.nvim_tabpage_get_number(buf.tabnr)`
            -- end,
            --duplicates_across_groups = false,
            --show_duplicate_prefix = false
            --move_wraps_at_ends = true,
        },
    },
}
