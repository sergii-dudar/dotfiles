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

            -- local mason = require("mason-registry")
            -- local bash_debug_adapter_path = mason.get_package("bash-debug-adapter"):get_install_path()
            local bash_debug_adapter_path = vim.fn.exepath("bash-debug-adapter")
            local bashdb_dir = bash_debug_adapter_path .. "/extension/bashdb_dir"

            dap.adapters.sh = {
                type = "executable",
                command = bash_debug_adapter_path .. "/bash-debug-adapter",
            }
            dap.configurations.sh = {
                {
                    name = "Launch Bash debugger",
                    type = "sh",
                    request = "launch",
                    program = "${file}",
                    cwd = "${fileDirname}",
                    pathBashdb = bashdb_dir .. "/bashdb",
                    pathBashdbLib = bashdb_dir,
                    pathBash = "bash",
                    pathCat = "cat",
                    pathMkfifo = "mkfifo",
                    pathPkill = "pkill",
                    env = {},
                    args = {},
                    -- showDebugOutput = true,
                    -- trace = true,
                },
            }
        end,
    },
}
