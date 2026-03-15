return {
    { "tiagovla/scope.nvim", config = true }, -- relying on stevearc/resession.nvim
    {
        "akinsho/bufferline.nvim",
        keys = {
            { "<Tab>", "<Cmd>BufferLineCycleNext<CR>", desc = "Next tab" },
            { "<S-Tab>", "<Cmd>BufferLineCyclePrev<CR>", desc = "Prev tab" },
            { "<S-h>", false },
            { "<S-l>", false },
        },
        cond = require("utils.project-util").is_multifile_proj,
        opts = {
            highlights = {
                tab_selected = {
                    fg = "#000000",
                    bg = "#676868",
                },
                tab_separator_selected = {
                    bg = "#676868",
                },
                separator = {
                    fg = "#676868",
                    bg = "#0f1011",
                },
            },
            options = {
                -- mode = "tabs", -- buffers|tabs: set to "tabs" to only show tabpages instead
                separator_style = { "|", "|" }, -- { "|", "|" }, -- "thick", --thin, thick, slant
                show_buffer_close_icons = false,
                show_close_icon = true,
                always_show_bufferline = true,
                show_tab_indicators = true,
                auto_toggle_bufferline = true,
                tab_size = 8,
                indicator = {
                    -- icon = "",
                    -- icon = "󰜴",
                    style = "none", -- 'icon' | 'underline' | 'none'
                },

                get_element_icon = function(element)
                    if element.path and element.path:find("^jdt://jarentry/") then
                        return "󰏗", "Special"
                    end
                end,

                name_formatter = function(buf)
                    if buf.path and buf.path:find("^jdt://jarentry/") then
                        local entry = buf.path:match("jdt://jarentry/(.-)%?")
                        if entry then
                            return "jdt:" .. vim.fn.fnamemodify(entry, ":t")
                        end
                    end
                    return buf.name
                end,
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
    },
}
