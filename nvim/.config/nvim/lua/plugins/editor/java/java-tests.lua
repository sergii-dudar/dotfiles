return {
    {
        -- "rcasia/neotest-java",
        "sergii-dudar/neotest-java",
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
        opts = {
            adapters = {
                ["neotest-java"] = {},
            },
            -- default:
            -- quickfix = {
            --     enabled = true,
            --     open = false,
            -- },
            strategies = {
                -- used to attach window
                integrated = {
                    height = 70, -- 40
                    width = 260, -- 120
                },
            },
            quickfix = {
                enabled = true,
                open = false, -- need to override folke qflist open that in resuld opening two qfilist and diagnostics
            },
            floating = {
                max_height = 0.7,
                max_width = 0.95,
                border = "rounded",
            },
            output = {
                enabled = true,
                open_on_run = false,
                open_win = function()
                    -- Create a bottom split (horizontal) taking ~40% of the screen height
                    vim.cmd("botright 12split") -- '12' is approximate lines; adjust as needed (e.g., 'botright 20split')
                    require("neotest").run.attach({
                        enter = true,
                        open_win = function()
                            vim.cmd("botright 12split")
                        end,
                    })
                end,
            },
            consumers = {},
            -- output_panel = {
            --     enabled = true,
            --     open = "botright split | resize 15",
            -- },
        },
        --[[ config = function()
            vim.notify("In my te test")
            require("neotest").setup({
                adapters = {
                    require("neotest-java")({
                        -- config here
                    }),
                },
            })
        end, ]]
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