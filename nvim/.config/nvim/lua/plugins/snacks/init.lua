return {
    {
        "folke/snacks.nvim",
        opts = function(_, opts)
            -- vim.schedule(function()
            --     vim.api.nvim_set_hl(0, "SnacksPickerPathHidden", { link = "SnacksPickerPathNormal" })
            -- end)
            Snacks.util.set_hl({
                PathHidden = "",
            }, { prefix = "SnacksPicker", default = true })

            local layouts = require("plugins.snacks.configs.layouts")
            return vim.tbl_deep_extend("force", opts or {}, {
                picker = {
                    -- layout = {
                    --     cycle = false,
                    -- },
                    layout = layouts.custom_default,
                    hidden = true, -- Include hidden files in grep
                    ignored = true, -- Exclude git-ignored files
                    formatters = {
                        file = {
                            filename_first = true, -- display filename before the file path
                            filename_only = false, -- only show the filename
                            truncate = 250, -- truncate the file path to (roughly) this length
                        },
                    },
                    sources = {
                        explorer = {
                            layout = layouts.custom_explorer,
                            auto_close = false,
                        },
                        files = {
                            -- cmd = "fd", -- "fd"| "rg"| "find" command to use. Leave empty to auto-detect
                            cmd = "fd",
                            hidden = true, -- Show hidden files
                            ignored = true, -- Exclude git-ignored files
                            -- exclude = { "node_modules/*", "*.pyc", "*.log" }, -- Exclude patterns
                            -- preview = false, -- Enable file preview in picker
                        },
                        grep = {
                            hidden = true, -- Show hidden files
                            ignored = true, -- Exclude git-ignored files
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
            { "<leader>.", LazyVim.pick("files", { root = false }), desc = "Find Files (cwd)" },
            { "<leader>/", LazyVim.pick("live_grep", { root = false }), desc = "Grep (cwd)" },
            { "<leader>m", LazyVim.pick("oldfiles", { cwd = vim.uv.cwd() }), desc = "Recent (cwd)" },
            { "<leader><space>", LazyVim.pick("oldfiles", { cwd = vim.uv.cwd() }), desc = "Recent (cwd)" },
            --{ "<leader>fR", function() Snacks.picker.recent({ filter = { cwd = true }}) end, desc = "Recent (cwd)" },
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