return {
    {
        "mfussenegger/nvim-dap",
        -- stylua: ignore
        keys = {
            { "<F1>", function() require("dap").toggle_breakpoint() end, desc = "Toggle Breakpoint", },
            { "<F2>", function() require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: ")) end, desc = "Breakpoint Condition", },
            { "<F4>", function() require("dapui").eval() end, desc = "Eval", mode = { "n", "x" }, },
            ---
            { "<F6>", function() require("dap").step_out() end, desc = "Step Out", },
            { "<F7>", function() require("dap").step_into() end, desc = "Step Into", },
            { "<F8>", function() require("dap").step_over() end, desc = "Step Over", },
            ---
            { "<F9>", function() require("dap").continue() end, desc = "Run/Continue", },
            { "<F10>", function() require("dap").run_to_cursor() end, desc = "Run to Cursor", },
            { "<F12>", function() require("dap").terminate() require("dapui").toggle({}) end, desc = "Toggle DapUI", },
            -- { "<F12>", function() pcall(require("dap").terminate) pcall(require("dapui").close, {}) end, desc = "Terminate", },
        },
        opts = {
            -- defaults = {
            --     fallback = {
            --         terminal_win_cmd = "tabnew",
            --         focus_terminal = true,
            --     },
            -- },
        },
    },
    {
        "rcarriga/nvim-dap-ui",
        dependencies = { "nvim-neotest/nvim-nio" },
        opts = {
            -- stylua: ignore
            layouts = {
                {
                    elements = {
                        { id = "scopes", size = 0.25, },
                        { id = "breakpoints", size = 0.25, },
                        { id = "stacks", size = 0.25, },
                        { id = "watches", size = 0.25, },
                    },
                    position = "left",
                    size = 40,
                },
                {
                    elements = {
                        { id = "repl", size = 0.2, },
                        { id = "console", size = 0.8, },
                    },
                    position = "bottom",
                    size = 12,
                },
            },
        },
        -- stylua: ignore
        keys = {
            { "<leader>du", function() require("dapui").toggle({ reset = true }) end, desc = "Dap UI" },
            -- { "<leader>du", function() require("dapui").toggle({ }) end, desc = "Dap UI" },
            { "<leader>de", function()
                require("dapui").eval(nil, { enter = true })
                vim.schedule(function()
                    vim.wo.wrap = true
                    vim.wo.linebreak = true
                    vim.wo.breakindent = true
                end)
            end, desc = "Eval", mode = {"n", "x"} },
            { "<leader>dwf", function() require("utils.dap-util").eval_to_file() end, desc = "Eval to File" },
            { "<leader>dwf", function() require("utils.dap-util").selection_eval_to_file() end, desc = "Eval Selection to File", mode = "x" },
            { "<leader>dww", function() require("utils.dap-util").selection_to_file() end, desc = "Selection to File", mode = "x" },
            { "<leader>do", function() require("utils.dap-util").show_logs() end, desc = "Dap log output" },
        },
        config = function(_, opts)
            local dap = require("dap")
            local dapui = require("dapui")
            local dap_util = require("utils.dap-util")
            dapui.setup(opts)

            -- Track whether Neotree was open before the debug session so we
            -- can restore it only when appropriate (avoid surprise reveal).
            local neotree_was_open = false
            local function is_neotree_open()
                for _, win in ipairs(vim.api.nvim_list_wins()) do
                    local buf = vim.api.nvim_win_get_buf(win)
                    if vim.bo[buf].filetype == "neo-tree" then
                        return true
                    end
                end
                return false
            end

            dap.listeners.after.event_initialized["dapui_config"] = function()
                neotree_was_open = is_neotree_open()
                if neotree_was_open then
                    pcall(vim.cmd, "Neotree close")
                end
                dap_util.reset()
                dapui.open({ reset = true })
            end
            dap.listeners.before.event_terminated["dapui_config"] = function()
                pcall(dapui.close, {})
                if neotree_was_open then
                    vim.schedule(function()
                        local bufname = vim.api.nvim_buf_get_name(0)
                        if bufname and bufname ~= "" and vim.fn.filereadable(bufname) == 1 then
                            pcall(vim.cmd, "Neotree filesystem reveal left action=show")
                        else
                            pcall(vim.cmd, "Neotree filesystem left action=show")
                        end
                    end)
                end
            end
            dap.listeners.before.event_exited["dapui_config"] = function()
                pcall(dapui.close, {})
            end
        end,
    },
    {
        "theHamsta/nvim-dap-virtual-text",
        opts = {
            enabled = false, -- enable this plugin (the default)
            enabled_commands = true, -- create commands DapVirtualTextEnable, DapVirtualTextDisable, DapVirtualTextToggle, (DapVirtualTextForceRefresh for refreshing when debug adapter did not notify its termination)
            highlight_changed_variables = true, -- highlight changed values with NvimDapVirtualTextChanged, else always NvimDapVirtualText
            highlight_new_as_changed = true, -- highlight new variables in the same way as changed variables (if highlight_changed_variables)
            show_stop_reason = true, -- show stop reason when stopped for exceptions
            commented = false, -- prefix virtual text with comment string
            only_first_definition = true, -- only show virtual text at first definition (if there are multiple)
            all_references = true, -- show virtual text on all all references of the variable (not only definitions)
            --clear_on_continue = false,             -- clear virtual text on "continue" (might cause flickering when stepping)
            virt_text_win_col = 120,
        },
    },
    -- {
    --     "Weissle/persistent-breakpoints.nvim",
    --     config = function()
    --         require("persistent-breakpoints").setup({
    --             load_breakpoints_event = { "BufReadPost" },
    --         })
    --     end,
    -- },
    -- debugging nvim lua plugins config ( real game changer for customizing nvim config )
    --[[ Quickstart:
        - Launch the server in the debuggee using <leader>dl
        - Open another Neovim instance with the source file
        - Place breakpoint with <leader>db
        - Connect using the DAP client with <leader>dc
        - Run your script/plugin in the debuggee ]]
    -- {
    --     "mfussenegger/nvim-dap",
    --     dependencies = {
    --         "jbyuki/one-small-step-for-vimkind",
    --     },
    --     lazy = false,
    --     config = function()
    --         local dap = require("dap")
    --         dap.configurations.lua = {
    --             {
    --                 type = "nlua",
    --                 request = "attach",
    --                 name = "Attach to running Neovim instance",
    --             },
    --         }
    --
    --         dap.adapters.nlua = function(callback, config)
    --             callback({ type = "server", host = config.host or "127.0.0.1", port = config.port or 8086 })
    --         end
    --
    --         vim.keymap.set("n", "<leader>dl", function()
    --             require("osv").launch({ port = 8086 })
    --         end, { noremap = true })
    --     end,
    -- },
}
