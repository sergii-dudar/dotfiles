return {
    {
        "rcasia/neotest-java",
        ft = "java",
        dependencies = {
            "mfussenegger/nvim-jdtls",
            "mfussenegger/nvim-dap", -- for the debugger
            "rcarriga/nvim-dap-ui", -- recommended
            "theHamsta/nvim-dap-virtual-text", -- recommended
        },
    },
    {
        "nvim-neotest/neotest",
        dependencies = {
            "nvim-neotest/nvim-nio",
            "nvim-lua/plenary.nvim",
            "antoinemadec/FixCursorHold.nvim",
            "nvim-treesitter/nvim-treesitter",
        },
        config = function()
            require("neotest").setup({
                adapters = {
                    require("neotest-java")({
                        -- config here
                    }),
                },
            })
        end,
    },
}

-- stylua: ignore
--[[ keys = {
    { "<leader>tt", function() require("neotest").run.run(vim.fn.expand("%")) end, mode = "n", desc = "Run Test File", },
    { "<leader>tr", function() require("neotest").run.run() end, mode = "n", desc = "Run Test Nearest", },
    { "<leader>tD", function() require("neotest").run.run({ strategy = "dap" }) end, mode = "n", desc = "Debug Test File", },
    { "<leader>td", function() require("neotest").run.run({ vim.fn.expand("%"), strategy = "dap" }) end, mode = "n", desc = "Debug Test Nearest", },
    { "<leader>tl", function() require("neotest").run.run_last() end, mode = "n", desc = "Run Test Last", },
    { "<leader>tL", function() require("neotest").run.run_last({ strategy = "dap" }) end, mode = "n", desc = "Debug Test Last", },
}, ]]
