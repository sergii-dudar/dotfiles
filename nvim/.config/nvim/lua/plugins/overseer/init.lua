-- local last_run_info = {}
-- local write_run_info = function(runtype)
--     last_run_info.filetype = vim.bo.filetype
--     last_run_info.runtype = runtype
-- end

local restart_last = function()
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

        -- TEST:
        vim.defer_fn(function()
            local dap = require("dap")
            dap.run({
                type = "java",
                request = "attach",
                name = "Attach to Overseer (port 5005)",
                hostName = "127.0.0.1",
                port = 5005,
            })
            vim.cmd("Neotree close")
        end, 200) -- 1.5s; increase if your JVM is slow to start
    end
end

---@class task.Options
---@field task_name string
---@field is_open_output? boolean
---@field output_direction? "float"|"tab"|"vertical"|"horizontal"|"dock"
---@field on_finish? function

---@param opts task.Options
local run_task = function(opts)
    local overseer = require("overseer")

    -- Get ALL tasks (without any filters) and dispose them to clear the list
    local all_tasks = overseer.list_tasks()
    for _, task in ipairs(all_tasks) do
        task:dispose(true) -- true = force kill (for running tasks)
    end

    overseer.run_task({ name = opts.task_name }, function(task)
        if task then
            task:start()
            if opts.is_open_output then
                -- overseer.open({ enter = false })

                local direction = opts.output_direction or "horizontal"
                -- Use task:open_output() to show just the output without task list
                -- Options: "float", "tab", "vertical", "horizontal"
                -- Or use "dock" to show output with task list
                if direction == "dock" then
                    overseer.open({ enter = false })
                else
                    task:open_output(direction)
                end
            end
            if opts.on_finish then
                task:subscribe("on_complete", function(t, status)
                    opts.on_finish()
                end)
                -- task:subscribe("on_exit", function(t, status)
                --     vim.notify("on_exit")
                -- end)
            end
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
            {
                "<leader>rr",
                function()
                    if vim.bo.filetype == "lua" then
                        Snacks.debug.run()
                    else
                        run_task({
                            task_name = "RUN_CURRENT",
                            is_open_output = true,
                            output_direction = "horizontal", -- Shows only output, no task list
                        })
                    end
                    -- write_run_info("run")
                end,
                desc = "Run Current",
            },
            {
                "<leader>rd",
                function()
                    -- 1. Stop any existing debug session
                    local dap = require("dap")
                    dap.close()

                    -- 2. Kill the current overseer task (the running JVM)
                    local overseer = require("overseer")
                    local tasks = overseer.list_tasks({ status = overseer.STATUS.RUNNING })
                    for _, task in ipairs(tasks) do
                        if task.name == "Java Debug" then
                            task:dispose(true) -- true = force kill
                        end
                    end

                    local dapui = require("dapui")
                    dapui.close({})

                    -- 3. Re-launch via Overseer (non-blocking)
                    run_task({
                        task_name = "DEBUG_CURRENT",
                        on_finish = function()
                            local dap = require("dap")
                            dap.close()

                            -- 2. Kill the current overseer task (the running JVM)
                            local overseer = require("overseer")
                            local tasks = overseer.list_tasks({ status = overseer.STATUS.RUNNING })
                            for _, task in ipairs(tasks) do
                                if task.name == "Java Debug" then
                                    task:dispose(true) -- true = force kill
                                end
                            end

                            local dapui = require("dapui")
                            dapui.close({})
                        end,
                    })

                    -- 4. After a short delay, re-attach DAP
                    -- The delay gives the JVM a moment to start and open the port.
                    vim.defer_fn(function()
                        dap.run({
                            type = "java",
                            request = "attach",
                            name = "Attach to Overseer (port 5005)",
                            hostName = "127.0.0.1",
                            port = 5005,
                        })
                        vim.cmd("Neotree close")
                    end, 200) -- 1.5s; increase if your JVM is slow to start
                end,
                desc = "Debug Current",
            },
            { "<leader>rl", restart_last, desc = "Re-Run Last" },
            --[[ {
                "<leader>rd",
                function()
                    if vim.bo.filetype == "java" then
                        require("utils.java.jdtls-config-dap-util").run_current_main_class()
                    end
                    write_run_info("dap")
                end,
                desc = "Run Java Code Debug",
                noremap = true,
                silent = false,
            },
            {
                "<leader>rl",
                function()
                    if last_run_info.filetype == "java" and last_run_info.runtype == "dap" then
                        require("utils.java.jdtls-config-dap-util").rerun_last()
                    else
                        restart_last()
                    end
                end,
                desc = "Re-Run Last",
                noremap = true,
                silent = false,
            }, ]]
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
    --                 require("overseer").open({ enter = false })
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
