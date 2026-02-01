function restart_last()
    local overseer = require("overseer")
    local task_list = require("overseer.task_list")
    local tasks = overseer.list_tasks({
        status = {
            overseer.STATUS.SUCCESS,
            overseer.STATUS.FAILURE,
            overseer.STATUS.CANCELED,
        },
        sort = task_list.sort_finished_recently,
    })
    if vim.tbl_isempty(tasks) then
        vim.notify("No tasks found", vim.log.levels.WARN)
    else
        local most_recent = tasks[1]
        overseer.run_action(most_recent, "restart")
    end
end

function run_task(task_name)
    local overseer = require("overseer")
    overseer.run_task({ name = task_name }, function(task)
        if task then
            task:start()
            overseer.open()
        end
    end)
end

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
            -- dap = false,
            -- task_list = {
            --     bindings = {
            --         ["<C-h>"] = false,
            --         ["<C-j>"] = false,
            --         ["<C-k>"] = false,
            --         ["<C-l>"] = false,
            --     },
            -- },
            -- form = {
            --     win_opts = {
            --         winblend = 0,
            --     },
            -- },
            -- confirm = {
            --     win_opts = {
            --         winblend = 0,
            --     },
            -- },
            -- task_win = {
            --     win_opts = {
            --         winblend = 0,
            --     },
            -- },
        },
        keys = {
            -- { "<leader>rt", "<cmd>OverseerTaskAction<cr>",  desc = "Task action" },
            -- { "<leader>rw", "<cmd>OverseerToggle<cr>",      desc = "Task list" },
            {
                "<leader>rr",
                function()
                    if vim.bo.filetype == "lua" then
                        Snacks.debug.run()
                    else
                        run_task("RUN_CURRENT")
                    end
                end,
                desc = "Run Current",
            },
            {
                "<leader>rd",
                function()
                    run_task("DEBUG_CURRENT")
                end,
                desc = "Debug Current",
            },
            { "<leader>rl", restart_last, desc = "Re-Run Last" },
            {
                "<leader>rr",
                function()
                    Snacks.debug.run()
                end,
                desc = "Run Selected Lua",
                mode = "v",
            },
        },
        -- stylua: ignore
        config = function(_, opts)
            require("overseer").setup(opts)
            local run_current = require("plugins.overseer.tasks.run_current")
            local debug_current = require("plugins.overseer.tasks.debug_current")
            -- local build_current = require("plugins.overseer.tasks.build_current")
            require("overseer").register_template(run_current)
            require("overseer").register_template(debug_current)
            -- require("overseer").register_template(build_current)
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
    {
        "mfussenegger/nvim-dap",
        optional = true,
        opts = function()
            require("overseer").enable_dap()
        end,
    },
    -- {
    --     "folke/edgy.nvim",
    --     optional = true,
    --     opts = function(_, opts)
    --         opts.right = opts.right or {}
    --         table.insert(opts.right, {
    --             title = "Overseer",
    --             ft = "OverseerList",
    --             open = function()
    --                 require("overseer").open()
    --             end,
    --         })
    --     end,
    -- },
    -- {
    --     "nvim-neotest/neotest",
    --     optional = true,
    --     opts = function(_, opts)
    --         opts = opts or {}
    --         opts.consumers = opts.consumers or {}
    --         opts.consumers.overseer = require("neotest.consumers.overseer")
    --     end,
    -- },
}

-- return {
--     "stevearc/overseer.nvim",
-- event = "VeryLazy",
-- enabled = false,
-- opts = {
--     -- task_list = {
--     --     min_height = 16,
--     -- },
-- },

-- config = function(_, opts)
--     require("overseer").setup(opts)
--     require("overseer").register_template({
--         name = "Maven",
--         params = function()
--             return {
--                 subcommand = {
--                     desc = "Maven subcommand",
--                     type = "list",
--                     delimiter = " ",
--                     subtype = {
--                         type = "enum",
--                         choices = {
--                             "clean",
--                             "compile",
--                             "package",
--                             "install",
--                             "test",
--                             "verify",
--                             "deploy",
--                             "dependency:tree",
--                             "-DskipTests",
--                             "-Dmaven.test.skip=true",
--                         },
--                     },
--                 },
--             }
--         end,
--         builder = function(params)
--             local maven = require("utils.java.maven-util")
--             local settings = maven.get_maven_settings()
--             local file = vim.fn.expand("%")
--             local cmd = { "mvn" }
--             vim.list_extend(cmd, params.subcommand)
--             if settings then
--                 table.insert(cmd, "-s")
--                 table.insert(cmd, settings)
--             end
--             if maven.is_pom_file(file) then
--                 table.insert(cmd, "-f")
--                 table.insert(cmd, file)
--             end
--             return {
--                 cmd = cmd,
--             }
--         end,
--         condition = {
--             filetype = { "java", "xml" },
--             callback = function(param)
--                 if param.filetype == "xml" then
--                     local maven = require("utils.java.maven-util")
--                     return maven.is_pom_file(vim.fn.expand("%"))
--                 end
--                 return true
--             end,
--         },
--     })
-- end,
-- }

-- vim.api.nvim_set_keymap("n", "<F6>", "<cmd>CompilerOpen<cr>", { noremap = true, silent = true })
--
-- -- Redo last selected option
-- vim.api.nvim_set_keymap(
--     "n",
--     "<S-F6>",
--     "<cmd>CompilerStop<cr>" -- (Optional, to dispose all tasks before redo)
--         .. "<cmd>CompilerRedo<cr>",
--     { noremap = true, silent = true }
-- )
--
-- -- Toggle compiler results
-- vim.api.nvim_set_keymap("n", "<S-F7>", "<cmd>CompilerToggleResults<cr>", { noremap = true, silent = true })
--
