local constants = require("utils.constants")
return {
    "folke/trouble.nvim",
    opts = {
        -- preview = {
        --     type = "float",
        --     relative = "editor",
        --     border = "rounded",
        --     title = "Preview",
        --     title_pos = "center",
        --     position = { 0, -2 },
        --     size = { width = 0.3, height = 0.3 },
        --     zindex = 200,
        -- },
        preview = {
            type = "split",
            relative = "win",
            position = "right",
            size = 0.4,
        },
        modes = {
            lsp = {
                --win = { position = "right" },
                win = { position = "bottom" },
            },
            symbols = {
                --win = { position = "right" },
                win = { position = "bottom" },
            },
            qflist = {
                -- groups = {
                --     { "filename", format = "{file_icon} {filename} {count}" },
                -- },
                -- sort = { "severity", "filename", "pos", "message" },
                sort = { { by = "none" } },
                -- format = "{severity_icon|item.type:DiagnosticSignWarn} {text:ts} {pos}",
            },
            diagnostics = {
                groups = {
                    { "directory" },
                    { "filename", format = "{file_icon} {basename} {count}" },
                },
                -- sort = { "severity", "filename", "pos", "message" },
                sort = { { by = "none" } },
            },
            maven_test_diagnostics = {
                mode = "diagnostics",
                filter = function(items)
                    return vim.tbl_filter(function(item)
                        -- return item.severity == vim.diagnostic.severity.ERROR
                        return item.item.source == constants.java.maven_diagnostics_test_source
                    end, items)
                end,
            },
        },
        auto_close = true, -- auto close when there are no items
        auto_open = false, -- auto open when there are items
        auto_preview = true, -- automatically open preview when on an item
        auto_refresh = true, -- auto refresh when open
        auto_jump = false, -- auto jump to the item when there's only one
        focus = true, -- Focus the window when opened
    },
    keys = {
        { "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix List (Trouble)" },
        { "<leader>xl", "<cmd>Trouble loclist toggle<cr>", desc = "Location List (Trouble)" },
        { "<leader>xL", "<cmd>lopen<cr>", { desc = "Location List" } },
        { "<leader>xQ", "<cmd>copen<cr>", { desc = "Quickfix List" } },
    },
}
