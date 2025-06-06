return {
    {
        "folke/snacks.nvim",
        -- opts = {
        --     picker = {
        --         sources = {
        --             files = {
        --                 hidden = true, -- Show hidden files
        --                 ignored = false, -- Exclude git-ignored files
        --                 exclude = { "node_modules/*", "*.pyc", "*.log" }, -- Exclude patterns
        --                 preview = true, -- Enable file preview in picker
        --             },
        --             rg = {
        --                 hidden = true, -- Search hidden files
        --                 ignored = false, -- Exclude git-ignored files
        --                 exclude = { "node_modules/*", "__pycache__/*" }, -- Exclude patterns
        --             },
        --             buffers = {
        --                 hidden = true, -- Show hidden files in buffer list
        --                 ignored = true, -- Include git-ignored files
        --                 show_all_buffers = true, -- Include unlisted buffers
        --             },
        --             git_files = {
        --                 hidden = true, -- Show hidden git-tracked files
        --                 ignored = false, -- Only git-tracked files
        --             },
        --             grep = {
        --                 hidden = true, -- Include hidden files in grep
        --                 ignored = false, -- Exclude git-ignored files
        --             },
        --         },
        --     },
        --     explorer = {
        --         hidden = true, -- Show hidden files
        --         ignored = true, -- Show git-ignored files
        --         replace_netrw = true, -- Replace netrw
        --         layout = {
        --             preset = "float", -- Floating window for explorer
        --             width = 0.8, -- 80% of window width
        --             height = 0.8, -- 80% of window height
        --         },
        --         auto_close = false, -- Keep explorer open
        --     },
        -- },
        opts = function(_, opts)
            return vim.tbl_deep_extend("force", opts or {}, {
                picker = {
                    hidden = true, -- Include hidden files in grep
                    ignored = true, -- Exclude git-ignored files
                },
                explorer = {
                    replace_netrw = true,
                },
                image = {
                    force = true,
                    enabled = true,
                    debug = { request = false, convert = false, placement = false },
                    math = { enabled = true },
                    doc = { enabled = true, inline = true, float = true },
                    -- doc = { enabled = true, inline = true, float = false },
                },
            })
        end,
    },
}

-- return {
--     "folke/snacks.nvim",
--     opts = {
--         explorer = {
--
--             replace_netrw = true,
--
--             enabled = true,
--             hidden = true, -- Show hidden files in the explorer
--             ignored = true,
--             files = {
--                 hidden = true, -- Show hidden files in the explorer
--                 ignored = true,
--             },
--             -- your explorer configuration comes here
--             -- or leave it empty to use the default settings
--             -- refer to the configuration section below
--         },
--         picker = {
--             enabled = true,
--             hidden = true, -- Show hidden files in the explorer
--             ignored = true,
--             files = {
--                 hidden = true, -- Show hidden files in the explorer
--                 ignored = true,
--             },
--             sources = {
--                 hidden = true, -- Show hidden files in the explorer
--                 ignored = true,
--                 explorer = {
--                     replace_netrw = true,
--                     -- your explreplace_netrworer picker configuration comes here
--                     -- or leave it empty to use the default settings
--                     ignored = true,
--                     hidden = true,
--                 },
--                 files = {
--                     ignored = true,
--                     hidden = true,
--                 },
--             },
--         },
--
--         scroll = { enabled = false },
--         -- bigfile = { enabled = true },
--         -- dashboard = { enabled = true },
--         -- indent = { enabled = true },
--         -- input = { enabled = true },
--         -- notifier = { enabled = true },
--         -- quickfile = { enabled = true },
--         statuscolumn = { enabled = true },
--
--         -- words = { enabled = true },
--
--         -- :lua Snacks.picker.cliphist(opts?)
--         -- picker = {
--         --     -- preset = "ivy",
--         --     -- layout = { position = "bottom" },
--         --
--         --     enabled = true,
--         --     hidden = true,
--         --     ignored = true,
--         --     buffers = {
--         --         hidden = true,
--         --     },
--         --     explorer = {
--         --         hidden = true,
--         --         include = { "exclude", "ignored", "hidden" },
--         --     },
--         --     files = {
--         --         hidden = true,
--         --         ignored = true,
--         --     },
--         --     grep = {
--         --         hidden = true,
--         --         ignored = true,
--         --     },
--         --     sources = {
--         --         files = {
--         --             hidden = true,
--         --             ignored = true,
--         --         },
--         --     },
--         -- },
--         --
--         -- image = {
--         --     -- your image configuration comes here
--         --     -- or leave it empty to use the default settings
--         --     -- refer to the configuration section below
--         -- },
--         --
--         -- image = {
--         --     force = true,
--         --     enabled = true,
--         --     debug = { request = false, convert = false, placement = false },
--         --     math = { enabled = true },
--         --     doc = { enabled = true, inline = true, float = true },
--         --     -- doc = { enabled = true, inline = true, float = false },
--         -- },
--     },
--     {
--         "snacks.nvim",
--         keys = {
--             {
--                 "<leader>.",
--                 function()
--                     Snacks.scratch()
--                 end,
--                 desc = "Toggle Scratch Buffer",
--             },
--             {
--                 "<leader>S",
--                 function()
--                     Snacks.scratch.select()
--                 end,
--                 desc = "Select Scratch Buffer",
--             },
--             {
--                 "<leader>dps",
--                 function()
--                     Snacks.profiler.scratch()
--                 end,
--                 desc = "Profiler Scratch Buffer",
--             },
--         },
--     },
-- }
