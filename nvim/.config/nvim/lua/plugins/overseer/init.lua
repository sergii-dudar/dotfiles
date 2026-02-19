_G.task = {}
task.test_type = {
    --- @enum task.test_type
    ALL_TESTS = 0,
    FILE_TESTS = 1,
    CURRENT_TEST = 2,
    CURRENT_PARAMETRIZED_NUM_TEST = 3,
}
task.run_type = {
    --- @enum task.run_type
    RUN = 0,
    TEST = 1,
}

return {
    {
        "stevearc/overseer.nvim",
        cmd = {
            "OverseerOpen",
            "OverseerClose",
            "OverseerToggle",
            "OverseerRun",
            "OverseerTaskAction",
        },
        opts = {
            -- output = {
            --     use_terminal = true,
            --     preserve_output = false,
            -- },
            task_list = {
                max_width = 5,
                min_width = 5,
                max_height = 0.33,
                min_height = 0.33,
                render = function(task)
                    local render = require("overseer.render")
                    return {
                        render.status_and_name(task),
                    }
                end,
            },
        },
        -- stylua: ignore
        keys = {
            -- code runners
            { "<leader>rr", function() require("plugins.overseer.overseer-util").run_current() end, desc = "Run Current", },
            { "<leader>rd", function() require("plugins.overseer.overseer-util").debug_current() end, desc = "Debug Current", },
            { "<leader>rl", function() require("plugins.overseer.overseer-util").restart_last() end, desc = "Re-Run Last" },
            { "<leader>rr", function() Snacks.debug.run() end, desc = "Run Selected Lua", mode = "v", },
            { "<leader>ro", "<cmd>OverseerToggle<cr>", desc = "Task list" },
            { "<leader>rt", "<cmd>OverseerTaskAction<cr>", desc = "Task action" },
            -- test runners
            { "<leader>rtr", function() require("plugins.overseer.overseer-util").run_test({ type = task.test_type.CURRENT_TEST }) end, desc = "Run Current Test", },
            { "<leader>rtd", function() require("plugins.overseer.overseer-util").run_test({ type = task.test_type.CURRENT_TEST, is_debug = true }) end, desc = "Debug Current Test", },
            { "<leader>rtf", function() require("plugins.overseer.overseer-util").run_test({ type = task.test_type.FILE_TESTS }) end, desc = "Run File Tests", },
            { "<leader>rta", function() require("plugins.overseer.overseer-util").run_test({ type = task.test_type.ALL_TESTS }) end, desc = "Run All Tests", },
            { "<leader>rtp", function()
                Snacks.input({ prompt = "Test Number" }, function(value)
                    require("plugins.overseer.tasks.lang-runner-resolver").resolve(vim.bo.filetype).set_parametrized_test_num(value)
                    require("plugins.overseer.overseer-util").run_test({ type = task.test_type.CURRENT_PARAMETRIZED_NUM_TEST })
                end)
            end, desc = "Run Current Parametrized Single Test", },
            { "<leader>rtP", function()
                Snacks.input({ prompt = "Test Number" }, function(value)
                    require("plugins.overseer.tasks.lang-runner-resolver").resolve(vim.bo.filetype).set_parametrized_test_num(value)
                    require("plugins.overseer.overseer-util").run_test({
                        type = task.test_type.CURRENT_PARAMETRIZED_NUM_TEST,
                        is_debug = true,
                    })
                end)
            end, desc = "Debug Current Parametrized Single Test", },
        },
        config = function(_, opts)
            local overseer = require("overseer")
            overseer.setup(opts)
            overseer.enable_dap()
            local compile_current = require("plugins.overseer.tasks.compile_current")
            local run_current = require("plugins.overseer.tasks.run_current")
            local debug_current = require("plugins.overseer.tasks.debug_current")
            local run_tests = require("plugins.overseer.tasks.run_tests")
            overseer.register_template(compile_current.build_taks())
            overseer.register_template(run_current.build_taks())
            overseer.register_template(debug_current.build_taks())
            overseer.register_template(run_tests.build_taks())

            -- Map 'q' to close overseer task output windows
            vim.api.nvim_create_autocmd("FileType", {
                -- pattern = { "OverseerList", "OverseerOutput" },
                pattern = { "OverseerOutput" },
                callback = function(event)
                    vim.keymap.set("n", "q", "<cmd>OverseerClose<cr>", { buffer = event.buf, silent = true })
                end,
            })
        end,
    },
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            spec = {
                { "<leader>r", group = "overseer runner" },
            },
        },
    },
}
