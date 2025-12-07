return {
    {
        "rcasia/neotest-java",
        ft = "java",
        dependencies = {
            "mfussenegger/nvim-jdtls",
            "mfussenegger/nvim-dap",
            "rcarriga/nvim-dap-ui",
            "theHamsta/nvim-dap-virtual-text",
        },
        -- stylua: ignore
        --[[ keys = {
            { "<leader>tt", function() require("neotest").run.run(vim.fn.expand("%")) end, mode = "n", desc = "Run Test File", },
            { "<leader>tr", function() require("neotest").run.run() end, mode = "n", desc = "Run Test Nearest", },
            { "<leader>tD", function() require("neotest").run.run({ strategy = "dap" }) end, mode = "n", desc = "Debug Test File", },
            { "<leader>td", function() require("neotest").run.run({ vim.fn.expand("%"), strategy = "dap" }) end, mode = "n", desc = "Debug Test Nearest", },
            { "<leader>tl", function() require("neotest").run.run_last() end, mode = "n", desc = "Run Test Last", },
            { "<leader>tL", function() require("neotest").run.run_last({ strategy = "dap" }) end, mode = "n", desc = "Debug Test Last", },
        }, ]]
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
                ["neotest-java"] = {
                    -- root_dir = function()
                    --     -- Force Neotest to recognize Ant by treating it like Maven
                    --     local cwd = vim.fn.getcwd()
                    --     if vim.fn.filereadable(cwd .. "/build.xml") == 1 then
                    --         return cwd
                    --     end
                    --     return require("jdtls.setup").find_root({ "pom.xml", "build.gradle", ".git" }) or cwd
                    -- end,
                    -- ignore_wrapper = true,
                    -- force_test_class = true,
                    -- incremental_build = true,
                    -- extra_args = { "-Dtest=true -Ddomain=test -Djunit.jupiter.execution.parallel.enabled=false" },
                },
            },
        },
    },
}

-- return {
--     {
--         "rcasia/neotest-java",
--         ft = "java",
--         dependencies = {
--             "mfussenegger/nvim-jdtls",
--             "mfussenegger/nvim-dap", -- for the debugger
--             "rcarriga/nvim-dap-ui", -- recommended
--             "theHamsta/nvim-dap-virtual-text", -- recommended
--         },
--         --[[ init = function()
--             -- override the default keymaps.
--             -- needed until neotest-java is integrated in LazyVim
--             local keys = require("lazyvim.plugins.lsp.keymaps").get()
--             -- run test file
--             keys[#keys + 1] = {"<leader>tt", function() require("neotest").run.run(vim.fn.expand("%")) end, mode = "n" }
--             -- run nearest test
--             keys[#keys + 1] = {"<leader>tr", function() require("neotest").run.run() end, mode = "n" }
--             -- debug test file
--             keys[#keys + 1] = {"<leader>tD", function() require("neotest").run.run({ strategy = "dap" }) end, mode = "n" }
--             -- debug nearest test
--             keys[#keys + 1] = {"<leader>td", function() require("neotest").run.run({ vim.fn.expand("%"), strategy = "dap" }) end, mode = "n" }
--         end, ]]
--     },
--     {
--         "nvim-neotest/neotest",
--         dependencies = {
--             "nvim-neotest/nvim-nio",
--             "nvim-lua/plenary.nvim",
--             "antoinemadec/FixCursorHold.nvim",
--             "nvim-treesitter/nvim-treesitter",
--         },
--         -- stylua: ignore
--         keys = {
--             { "<leader>tt", function() require("neotest").run.run(vim.fn.expand("%")) end, mode = "n", desc = "Run Test File", },
--             { "<leader>tr", function() require("neotest").run.run() end, mode = "n", desc = "Run Test Nearest", },
--             { "<leader>tD", function() require("neotest").run.run({ strategy = "dap" }) end, mode = "n", desc = "Debug Test File", },
--             { "<leader>td", function() require("neotest").run.run({ vim.fn.expand("%"), strategy = "dap" }) end, mode = "n", desc = "Debug Test Nearest", },
--             { "<leader>tl", function() require("neotest").run.run_last() end, mode = "n", desc = "Run Test Last", },
--             { "<leader>tL", function() require("neotest").run.run_last({ strategy = "dap" }) end, mode = "n", desc = "Debug Test Last", },
--         },
--         opts = {
--             adapters = {
--                 ["neotest-java"] = {
--                     -- junit_jar = nil, -- default: stdpath("data") .. /nvim/neotest-java/junit-platform-console-standalone-[version].jar
--                     junit_jar = "/home/serhii/tools/java-extensions/junit/junit-platform-console-standalone-6.0.1.jar",
--                     -- config here
--                 },
--             },
--         },
--     },
-- }
