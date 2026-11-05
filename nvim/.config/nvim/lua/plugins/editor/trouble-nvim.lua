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
                -- sort = { { by = "none" } },
                -- Keep the quickfix list's own order. An EMPTY sort is the only real no-op:
                -- `{ by = "none" }` still runs table.sort with a nil key for every item, and
                -- that sort is not stable, so longer lists (e.g. a parsed stack trace opened
                -- via `Trouble qflist`) came out shuffled.
                sort = {},
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
            -- Same data as `java_trace`, but grouped by file. Because items are sorted
            -- by `trace_order` BEFORE the tree is built, group nodes are created in the
            -- order their first (lowest-ordinal) frame appears — so groups stay in trace
            -- order and frames within a group keep their 1..N order too. Selected via
            -- `require("utils.java.java-trace").trace_layout = "grouped"` (or the toggle).
            java_trace_grouped = {
                desc = "Java Stack Trace (grouped)",
                source = "qf.qflist",
                groups = {
                    { "filename", format = "{file_icon} {filename} {count}" },
                },
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
    -- `preview.type = "main"` draws the preview in a full-size floating window laid over the
    -- main window (`winhighlight=Normal:Normal`, so it is indistinguishable from the window
    -- below). The auto-preview is debounced by 100 ms (`throttle.preview`) and never checks
    -- that Trouble is still focused: `j` + `<CR>` (or `o`) inside that window jumps first and
    -- then drops the float on top of the window just jumped into. Neovim draws the cursor of a
    -- window hidden behind a float as an underline, the float never scrolls and `<C-o>` does
    -- not remove it — the "stuck `_` cursor" (reproduced 2026-11-04, trouble bd67efe; same
    -- race when leaving Trouble with a window command within 100 ms of moving). The throttle
    -- captures `View.preview` on mount, so wrapping it here makes every preview path
    -- (auto-preview, `p`, `P`) a no-op unless the Trouble window is the current window.
    config = function(_, opts)
        require("trouble").setup(opts)
        local View = require("trouble.view")
        local preview = View.preview
        ---@diagnostic disable-next-line: duplicate-set-field
        function View:preview(item)
            if not (self.win and self.win.win and vim.api.nvim_get_current_win() == self.win.win) then
                return
            end
            return preview(self, item)
        end
    end,
    keys = {
        { "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix List (Trouble)" },
        { "<leader>xl", "<cmd>Trouble loclist toggle<cr>", desc = "Location List (Trouble)" },
        { "<leader>xL", "<cmd>lopen<cr>", { desc = "Location List" } },
        { "<leader>xQ", "<cmd>copen<cr>", { desc = "Quickfix List" } },
    },
}
