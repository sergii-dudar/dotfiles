return {
    "theHamsta/nvim-dap-virtual-text",
    opts = {
        enabled = true, -- enable this plugin (the default)
        enabled_commands = true, -- create commands DapVirtualTextEnable, DapVirtualTextDisable, DapVirtualTextToggle, (DapVirtualTextForceRefresh for refreshing when debug adapter did not notify its termination)
        highlight_changed_variables = true, -- highlight changed values with NvimDapVirtualTextChanged, else always NvimDapVirtualText
        highlight_new_as_changed = false, -- highlight new variables in the same way as changed variables (if highlight_changed_variables)
        show_stop_reason = true, -- show stop reason when stopped for exceptions
        commented = false, -- prefix virtual text with comment string
        only_first_definition = true, -- only show virtual text at first definition (if there are multiple)
        all_references = false, -- show virtual text on all all references of the variable (not only definitions)
        --clear_on_continue = false,             -- clear virtual text on "continue" (might cause flickering when stepping)
    },
    {
        "mfussenegger/nvim-dap",
        keys = {
            {
                "<F1>",
                function()
                    require("dap").toggle_breakpoint()
                end,
                desc = "Toggle Breakpoint",
            },
            {
                "<F2>",
                function()
                    require("dap.ui.widgets").hover()
                end,
                desc = "Widgets",
            },
            {
                "<F8>",
                function()
                    require("dap").step_over()
                end,
                desc = "Step Over",
            },
            {
                "<F7>",
                function()
                    require("dap").step_into()
                end,
                desc = "Step Into",
            },
            {
                "<F6>",
                function()
                    require("dap").run_to_cursor()
                end,
                desc = "Run to Cursor",
            },
            {
                "<F9>",
                function()
                    require("dap").continue()
                end,
                desc = "Continue",
            },
            {
                "<F10>",
                function()
                    require("dap").step_out()
                end,
                desc = "Step Out",
            },
            {
                "<F12>",
                function()
                    require("dap").terminate()
                    require("dapui").toggle({})
                end,
                desc = "Terminate",
            },
        },
    },
}
