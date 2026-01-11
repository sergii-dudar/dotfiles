return {
    -- { "nvim-mini/mini.nvim", version = "*" },
    -- {
    --     "nvim-mini/mini.move",
    --     event = "VeryLazy",
    --     opts = {},
    -- },
    {
        "nvim-mini/mini.surround",
        opts = {
            mappings = {
                add = "gsa", -- Add surrounding in Normal and Visual modes
                delete = "gsd", -- Delete surrounding
                find = "gsf", -- Find surrounding (to the right)
                find_left = "gsF", -- Find surrounding (to the left)
                highlight = "gsh", -- Highlight surrounding
                replace = "gsr", -- Replace surrounding
                update_n_lines = "gsn", -- Update `n_lines`
            },
        },
    },
    {
        "nvim-mini/mini.cmdline",
        opts = {
            autocomplete = {
                enable = false,
            },
            -- show command's target range in a floating window
            autopeek = {
                enable = true,
                -- Number of lines to show above and below range lines
                n_context = 1,
            },
        },
    },
    {
        "nvim-mini/mini.splitjoin",
        opts = {
            mappings = {
                toggle = "gS",
                split = "",
                join = "",
            },
            -- Detection options: where split/join should be done
            detect = {
                -- Array of Lua patterns to detect region with arguments.
                -- Default: { '%b()', '%b[]', '%b{}' }
                brackets = nil,

                -- String Lua pattern defining argument separator
                separator = ",",

                -- Array of Lua patterns for sub-regions to exclude separators from.
                -- Enables correct detection in presence of nested brackets and quotes.
                -- Default: { '%b()', '%b[]', '%b{}', '%b""', "%b''" }
                exclude_regions = nil,
            },
        },
    },
}
