-- :lua print(vim.fn.stdpath("log"))
-- :lua print(vim.fn.stdpath("data"))

-- https://github.com/rcasia/neotest-java/pull/153#issuecomment-2395210490
return {
    {
        "rcasia/neotest-java",
        -- "sergii-dudar/neotest-java",
        -- commit = "8fefa6d853c2f37126f3a7406c76e46824859b65",
        --[[ branch = "main",
        commit = "f6e357f630fc21111d92553fb0ceacfdba1157b3", ]]
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
        keys = {
            -- {"<leader>t", "", desc = "+test"},
            -- { "<leader>ta", function() require("neotest").run.attach() end, desc = "Attach to Test (Neotest)" },
            -- { "<leader>tt", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run File (Neotest)" },
            -- { "<leader>tT", function() require("neotest").run.run(vim.uv.cwd()) end, desc = "Run All Test Files (Neotest)" },
            -- { "<leader>tr", function() require("neotest").run.run() end, desc = "Run Nearest (Neotest)" },
            -- { "<leader>tl", function() require("neotest").run.run_last() end, desc = "Run Last (Neotest)" },
            -- stylua: ignore
            { "<leader>tR", function() require("neotest").run.run({strategy = "dap"}) end, desc = "Debug Nearest" },
            {
                "<leader>ts",
                function()
                    local before = vim.api.nvim_get_current_win()
                    require("neotest").summary.toggle()
                    local after = vim.api.nvim_get_current_win()
                    if before == after then
                        vim.cmd("wincmd w")
                    end
                end,
                desc = "Toggle Summary (Neotest)",
            },
            -- { "<leader>to", function() require("neotest").output.open({ enter = true, auto_close = true }) end, desc = "Show Output (Neotest)" },
            -- { "<leader>tO", function() require("neotest").output_panel.toggle() end, desc = "Toggle Output Panel (Neotest)" },
            -- { "<leader>tS", function() require("neotest").run.stop() end, desc = "Stop (Neotest)" },
            -- { "<leader>tw", function() require("neotest").watch.toggle(vim.fn.expand("%")) end, desc = "Toggle Watch (Neotest)" },
        },
        opts = {
            adapters = {
                ["neotest-java"] = {
                    log_level = vim.log.levels.DEBUG,
                    junit_jar = vim.fn.glob("$HOME/tools/java-extensions/junit/junit-platform-console-standalone.jar"),
                    jvm_args = {
                        string.format("-javaagent:%s/tools/java-extensions/jmockit/jmockit.jar", os.getenv("HOME")),
                    },
                    -- incremental_build = true,
                },
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