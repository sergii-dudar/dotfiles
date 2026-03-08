local constants = require("utils.constants")
local nio_util = require("utils.nio-util")

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

---@param type task.test_type|integer
---@param is_debug boolean|nil
---@return function
local build_run_test = function(type, is_debug)
    return function()
        nio_util.run(function()
            require("plugins.overseer.overseer-util").run_test({ test_type = type, is_debug = is_debug })
        end)
    end
end

return {
    {
        "stevearc/overseer.nvim",
        dependencies = { "nvim-neotest/nvim-nio" },
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
                max_width = 7,
                min_width = 7,
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
            { "<leader>rs", function() require("plugins.overseer.overseer-util").stop_all() end, desc = "Stop All" },
            -- test runners
            { "<leader>ts", function() require("plugins.overseer.overseer-util").stop_all() end, desc = "Stop All" },
            -- { "<leader>tr", build_run_test(task.test_type.CURRENT_TEST), desc = "Run Current Test", },
            { "<leader>tt", build_run_test(task.test_type.CURRENT_TEST), desc = "Run Current Test", },
            { "<leader>td", build_run_test(task.test_type.CURRENT_TEST, true), desc = "Debug Current Test", },
            { "<leader>tf", build_run_test(task.test_type.FILE_TESTS), desc = "Run File Tests", },
            { "<leader>ta", build_run_test(task.test_type.ALL_TESTS), desc = "Run All Tests", },
            { "<leader>tp", build_run_test(task.test_type.CURRENT_PARAMETRIZED_NUM_TEST), desc = "Run Current Parametrized Single Test", },
            { "<leader>tP", build_run_test(task.test_type.CURRENT_PARAMETRIZED_NUM_TEST, true), desc = "Debug Current Parametrized Single Test", },
            { "<leader>tl", function() require("plugins.overseer.overseer-util").restart_last() end, desc = "Re-Run Last" },
            { "<leader>to", function() require("plugins.overseer.test-report").show_test_output() end, desc = "Toggle Test Output" },
            { "<leader>tO", function() require("plugins.overseer.test-report").hide_test_output() end, desc = "Hide Test Output" },
            { "<leader>tL", function() require("plugins.overseer.test-report").load_existing() end, desc = "Load Last Test Report" },
            { "<leader>txx", "<cmd>Trouble junit_diagnostics toggle<cr>", desc = "Tests junit diagnostics trouble" },
            { "<leader>txd", function()
                Snacks.picker.diagnostics({
                     -- severity = vim.diagnostic.severity.ERROR,
                     filter = {
                        filter = function(item, filter)
                            return item.item.source == constants.java.junit
                        end,
                     },
                })
            end, desc = "Tests junit diagnostics picker" },
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
            overseer.register_template(run_tests.build_debug_taks())

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
                { "<leader>r", group = "+code runner" },
                { "<leader>t", group = "+tests runner" },
            },
        },
    },
}
