local a = {
    {
        "rcarriga/nvim-dap-ui",
        dependencies = {
            "mfussenegger/nvim-dap",
            "nvim-neotest/nvim-nio",
        },
        config = function()
            local dap, dapui = require("dap"), require("dapui")
            dapui.setup()

            dap.listeners.before.attach.dapui_config = function()
                dapui.open()
            end
            dap.listeners.before.launch.dapui_config = function()
                dapui.open()
            end
            dap.listeners.before.event_terminated.dapui_config = function()
                dapui.close()
            end
            dap.listeners.before.event_exited.dapui_config = function()
                dapui.close()
            end
        end,
    },
    {
        "mfussenegger/nvim-dap",
        dependencies = {
            "leoluz/nvim-dap-go",
        },
        config = function()
            local dap = require("dap")

            --
            --TODO: try manage dap tools by mason
            require("dap-go").setup()
            --

            -- Run Code <F4>
            -- Debug Code <F5>
            -- Step Into <F7>
            -- Step Over <F8>
            -- Continue <F9>
            vim.keymap.set("n", "<F5>", dap.continue)
            vim.keymap.set("n", "<F7>", dap.step_into)
            vim.keymap.set("n", "<F8>", dap.step_over)
            -- vim.keymap.set("n", "<F12>", dap.step_out)
            vim.keymap.set("n", "<Leader>dt", dap.toggle_breakpoint)
            vim.keymap.set("n", "<Leader>db", dap.set_breakpoint)
            vim.keymap.set("n", "<Leader>dp", function()
                dap.set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
            end)
            vim.keymap.set("n", "<Leader>dr", dap.repl.open)
            vim.keymap.set("n", "<Leader>dl", dap.run_last)
            vim.keymap.set({ "n", "v" }, "<Leader>dh", require("dap.ui.widgets").hover)
            vim.keymap.set({ "n", "v" }, "<Leader>dp", require("dap.ui.widgets").preview)
            vim.keymap.set("n", "<Leader>df", function()
                local widgets = require("dap.ui.widgets")
                widgets.centered_float(widgets.frames)
            end)
            vim.keymap.set("n", "<Leader>ds", function()
                local widgets = require("dap.ui.widgets")
                widgets.centered_float(widgets.scopes)
            end)
        end,
    },
}
return {
}
