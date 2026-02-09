return {
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                bashls = {
                    filetypes = { "sh", "zsh" },
                },
            },
        },
    },
    -- Add debugger
    {
        "mfussenegger/nvim-dap",
        optional = true,
        dependencies = "mason-org/mason.nvim",
        opts = function()
            local dap = require("dap")
            dap.adapters.bashdb = {
                type = "executable",
                command = vim.fn.stdpath("data") .. "/mason/packages/bash-debug-adapter/bash-debug-adapter",
                name = "bashdb",
            }
            -- dap.configurations.sh = {
            --     {
            --         type = "bashdb",
            --         request = "launch",
            --         name = "Launch file",
            --         showDebugOutput = true,
            --         pathBashdb = vim.fn.stdpath("data")
            --             .. "/mason/packages/bash-debug-adapter/extension/bashdb_dir/bashdb",
            --         pathBashdbLib = vim.fn.stdpath("data") .. "/mason/packages/bash-debug-adapter/extension/bashdb_dir",
            --         trace = true,
            --         file = "${file}",
            --         program = "${file}",
            --         cwd = "${workspaceFolder}",
            --         pathCat = "cat",
            --         -- pathBash = "/bin/bash",
            --         pathBash = vim.fn.has("mac") == 1 and "/opt/homebrew/bin/bash" or "/bin/bash",
            --         pathMkfifo = "mkfifo",
            --         pathPkill = "pkill",
            --         args = {},
            --         env = {},
            --         terminalKind = "integrated",
            --     },
            -- }

            -- dap.adapters.sh = {
            --     type = "executable",
            --     command = bash_debug_adapter_path .. "/bash-debug-adapter",
            -- }
            -- dap.configurations.sh = {
            --     {
            --         name = "Launch Bash debugger",
            --         type = "sh",
            --         request = "launch",
            --         program = "${file}",
            --         cwd = "${fileDirname}",
            --         pathBashdb = bashdb_dir .. "/bashdb",
            --         pathBashdbLib = bashdb_dir,
            --         pathBash = "bash",
            --         pathCat = "cat",
            --         pathMkfifo = "mkfifo",
            --         pathPkill = "pkill",
            --         env = {},
            --         args = {},
            --         -- showDebugOutput = true,
            --         -- trace = true,
            --     },
            -- }
        end,
    },
}
