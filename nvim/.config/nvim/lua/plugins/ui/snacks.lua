return {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    opts = {
        scroll = { enabled = false },
        -- bigfile = { enabled = true },
        -- dashboard = { enabled = true },
        -- indent = { enabled = true },
        -- input = { enabled = true },
        -- notifier = { enabled = true },
        -- quickfile = { enabled = true },
        statuscolumn = { enabled = true },
        -- words = { enabled = true },

        -- explorer = {
        --     -- your explorer configuration comes here
        --     -- or leave it empty to use the default settings
        --     -- refer to the configuration section below
        -- },
        -- picker = {
        --     sources = {
        --         explorer = {
        --             -- your explorer picker configuration comes here
        --             -- or leave it empty to use the default settings
        --         },
        --     },
        -- },
        --
        -- image = {
        --     -- your image configuration comes here
        --     -- or leave it empty to use the default settings
        --     -- refer to the configuration section below
        -- },
        image = {
            force = true,
            enabled = true,
            debug = { request = false, convert = false, placement = false },
            math = { enabled = true },
            doc = { enabled = true, inline = true, float = true },
            -- doc = { enabled = true, inline = true, float = false },
        },
    },
    {
        "snacks.nvim",
        keys = {
            -- { "<leader>.",  function() Snacks.scratch() end, desc = "Toggle Scratch Buffer" },
            -- { "<leader>S",  function() Snacks.scratch.select() end, desc = "Select Scratch Buffer" },
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
