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
            { "<leader>rr", function() require("plugins.overseer.overseer-util").run_current() end, desc = "Run Current", },
            { "<leader>rd", function() require("plugins.overseer.overseer-util").debug_current() end, desc = "Debug Current", },
            { "<leader>rl", function() require("plugins.overseer.overseer-util").restart_last() end, desc = "Re-Run Last" },
            { "<leader>rr", function() Snacks.debug.run() end, desc = "Run Selected Lua", mode = "v", },
            { "<leader>ro", "<cmd>OverseerToggle<cr>", desc = "Task list" },
            { "<leader>rt", "<cmd>OverseerTaskAction<cr>", desc = "Task action" },
        },
        config = function(_, opts)
            local overseer = require("overseer")
            overseer.setup(opts)
            overseer.enable_dap()
            local compile_current = require("plugins.overseer.tasks.compile_current")
            local run_current = require("plugins.overseer.tasks.run_current")
            local debug_current = require("plugins.overseer.tasks.debug_current")
            overseer.register_template(compile_current.build_taks())
            overseer.register_template(run_current.build_taks())
            overseer.register_template(debug_current.build_taks())
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
