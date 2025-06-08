return {
    {
        "folke/snacks.nvim",
        -- lazy = false,
        -- optional = false,
        -- priority = 1000,
        opts = function(_, opts)
            vim.api.nvim_set_hl(0, "SnacksPickerPathHidden", { link = "SnacksPickerPathNormal" })

            return vim.tbl_deep_extend("force", opts or {}, {
                picker = {
                    hidden = true, -- Include hidden files in grep
                    ignored = true, -- Exclude git-ignored files
                    layout = {
                        cycle = false,
                    },
                    sources = {
                        explorer = {},
                        -- files = {
                        --     -- cmd = "fd", -- "fd"| "rg"| "find" command to use. Leave empty to auto-detect
                        --     hidden = true, -- Show hidden files
                        --     ignored = true, -- Exclude git-ignored files
                        --     -- exclude = { "node_modules/*", "*.pyc", "*.log" }, -- Exclude patterns
                        --     -- preview = false, -- Enable file preview in picker
                        -- },
                    },
                },
                explorer = {
                    -- win = {
                    --     icons = {
                    --         ui = {
                    --             live = "󰐰 111",
                    --             hidden = "",
                    --             ignored = "",
                    --             follow = "f",
                    --             selected = "● ",
                    --             unselected = "○ ",
                    --             -- selected = " ",
                    --         },
                    --     },
                    -- },
                    hidden = true, -- Show hidden files󰔊
                    ignored = true, -- Show git-ignored files
                    replace_netrw = true,
                    auto_close = false, -- Keep explorer open
                },
                image = {
                    force = true,
                    enabled = true,
                    debug = { request = false, convert = false, placement = false },
                    math = { enabled = true },
                    doc = {
                        enabled = true,
                        inline = true,
                        float = true,
                        max_width = 80,
                        max_height = 40,
                    },
                    -- doc = { enabled = true, inline = true, float = false },
                },
                scroll = { enabled = false },
                -- bigfile = { enabled = true },
                -- dashboard = { enabled = true },
                -- indent = { enabled = true },
                -- input = { enabled = true },
                -- notifier = { enabled = true },
                -- quickfile = { enabled = true },
                statuscolumn = { enabled = true },
                words = { enabled = true },
            })
        end,
        keys = {
            -- {
            --     "<leader>.",
            --     function()
            --         Snacks.scratch()
            --     end,
            --     desc = "Toggle Scratch Buffer",
            -- },
            -- {
            --     "<leader>S",
            --     function()
            --         Snacks.scratch.select()
            --     end,
            --     desc = "Select Scratch Buffer",
            -- },
            {
                "<leader>dps",
                function()
                    Snacks.profiler.scratch()
                end,
                desc = "Profiler Scratch Buffer",
            },
        },
    },
}