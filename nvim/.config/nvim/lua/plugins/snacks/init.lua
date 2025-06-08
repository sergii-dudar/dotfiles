return {
    {
        "folke/snacks.nvim",
        opts = function(_, opts)
            vim.schedule(function()
                vim.api.nvim_set_hl(0, "SnacksPickerPathHidden", { link = "SnacksPickerPathNormal" })
            end)

            return vim.tbl_deep_extend("force", opts or {}, {
                picker = {
                    layout = {
                        cycle = false,
                    },
                    hidden = true, -- Include hidden files in grep
                    ignored = true, -- Exclude git-ignored files
                    sources = {
                        explorer = {},
                        -- files = {
                        --     -- cmd = "fd", -- "fd"| "rg"| "find" command to use. Leave empty to auto-detect
                        --     hidden = true, -- Show hidden files
                        --     ignored = true, -- Exclude git-ignored files
                        --     -- exclude = { "node_modules/*", "*.pyc", "*.log" }, -- Exclude patterns
                        --     -- preview = false, -- Enable file preview in picker
                        -- },
                        grep = {
                            layout = {
                                reverse = true,
                                layout = {
                                    box = "horizontal",
                                    backdrop = false,
                                    width = 0.8,
                                    height = 0.9,
                                    border = "none",
                                    {
                                        box = "vertical",
                                        { win = "list", title = " Results ", title_pos = "center", border = "rounded" },
                                        {
                                            win = "input",
                                            height = 1,
                                            border = "rounded",
                                            title = "{title} {live} {flags}",
                                            title_pos = "center",
                                        },
                                    },
                                    {
                                        win = "preview",
                                        title = "{preview:Preview}",
                                        width = 0.45,
                                        border = "rounded",
                                        title_pos = "center",
                                    },
                                },
                            },
                        },
                    },
                    toggles = {
                        hidden = "󱞞",
                        ignored = "",
                        modified = "",
                    },
                },
                explorer = {
                    hidden = true, -- Show hidden files
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