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
        -- preview = {
        --     type = "split",
        --     relative = "win",
        --     position = "right",
        --     size = 0.4,
        -- },
        preview = {
            type = "main",
            -- when a buffer is not yet loaded, the preview window will be created
            -- in a scratch buffer with only syntax highlighting enabled.
            -- Set to false, if you want the preview to always be a real loaded buffer.
            scratch = true,
        },
        -- Custom sorters (referenced by name in a mode's `sort`).
        sorters = {
            -- Order strictly by the trace ordinal the java-trace parser stamps on each
            -- quickfix item (`nr`). Trouble's table.sort is NOT stable, so an empty/
            -- equal-key sort scrambles equal items — an explicit numeric key is the only
            -- reliable way to keep stack frames in their exact 1..N order.
            trace_order = function(item)
                return (item.item and item.item.nr) or 0
            end,
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
            -- Flat, exactly-ordered view of the quickfix list, used by the Java
            -- stack-trace parser (utils.java.java-trace). Reads the same `qf.qflist`
            -- source but deliberately defines NO `groups` (so frames are not grouped
            -- by file) and sorts by `trace_order` (the per-item `nr` ordinal) so frames
            -- stay in exact trace order 1..N. The frame text carries the ordinal +
            -- class.method, and the topmost frame is prefixed with a star by the parser.
            java_trace = {
                desc = "Java Stack Trace",
                source = "qf.qflist",
                sort = { "trace_order" },
                format = "{text:ts} {pos}",
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
            maven_compile_diagnostics = {
                mode = "diagnostics",
                filter = function(items)
                    return vim.tbl_filter(function(item)
                        return item.item.source == constants.java.maven_diagnostics_compile_source
                    end, items)
                end,
            },
            junit_diagnostics = {
                mode = "diagnostics",
                filter = function(items)
                    return vim.tbl_filter(function(item)
                        return item.item.source == constants.java.junit
                    end, items)
                end,
            },
            cargo_test_diagnostics = {
                mode = "diagnostics",
                filter = function(items)
                    return vim.tbl_filter(function(item)
                        return item.item.source == constants.rust.cargo_test
                    end, items)
                end,
            },
            go_test_diagnostics = {
                mode = "diagnostics",
                filter = function(items)
                    return vim.tbl_filter(function(item)
                        return item.item.source == constants.go.go_test
                    end, items)
                end,
            },
            busted_test_diagnostics = {
                mode = "diagnostics",
                filter = function(items)
                    return vim.tbl_filter(function(item)
                        return item.item.source == constants.lua.busted
                    end, items)
                end,
            },
            bashunit_test_diagnostics = {
                mode = "diagnostics",
                filter = function(items)
                    return vim.tbl_filter(function(item)
                        return item.item.source == constants.bash.bashunit
                    end, items)
                end,
            },
            pytest_test_diagnostics = {
                mode = "diagnostics",
                filter = function(items)
                    return vim.tbl_filter(function(item)
                        return item.item.source == constants.python.pytest
                    end, items)
                end,
            },
            jest_test_diagnostics = {
                mode = "diagnostics",
                filter = function(items)
                    return vim.tbl_filter(function(item)
                        return item.item.source == constants.js.jest
                    end, items)
                end,
            },
            dotnet_test_diagnostics = {
                mode = "diagnostics",
                filter = function(items)
                    return vim.tbl_filter(function(item)
                        return item.item.source == constants.cs.dotnet_test
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
