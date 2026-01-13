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
            { "<F12>", function() require("dap").terminate() require("dapui").toggle({}) end, desc = "Terminate", },
        },
    },
    {
        "theHamsta/nvim-dap-virtual-text",
        opts = {
            enabled = true, -- enable this plugin (the default)
            enabled_commands = true, -- create commands DapVirtualTextEnable, DapVirtualTextDisable, DapVirtualTextToggle, (DapVirtualTextForceRefresh for refreshing when debug adapter did not notify its termination)
            highlight_changed_variables = true, -- highlight changed values with NvimDapVirtualTextChanged, else always NvimDapVirtualText
            highlight_new_as_changed = true, -- highlight new variables in the same way as changed variables (if highlight_changed_variables)
            show_stop_reason = true, -- show stop reason when stopped for exceptions
            commented = false, -- prefix virtual text with comment string
            only_first_definition = true, -- only show virtual text at first definition (if there are multiple)
            all_references = true, -- show virtual text on all all references of the variable (not only definitions)
            --clear_on_continue = false,             -- clear virtual text on "continue" (might cause flickering when stepping)
            virt_text_win_col = 80,
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