return {
    {
        "rcasia/neotest-java",
        ft = "java",
        dependencies = {
            "mfussenegger/nvim-jdtls",
            "mfussenegger/nvim-dap", -- for the debugger
            "rcarriga/nvim-dap-ui", -- recommended
            "theHamsta/nvim-dap-virtual-text", -- recommended
        },
        init = function()
            -- override the default keymaps.
            -- needed until neotest-java is integrated in LazyVim
            local keys = require("lazyvim.plugins.lsp.keymaps").get()
            -- run test file
            keys[#keys + 1] = {
                "<leader>tt",
                function()
                    require("neotest").run.run(vim.fn.expand "%")
                end,
                mode = "n",
                desc = "Run File",
            }
            -- run nearest test
            keys[#keys + 1] = {
                "<leader>tr",
                function()
                    require("neotest").run.run()
                end,
                mode = "n",
                desc = "Run Nearest",
            }
            -- debug test file
            keys[#keys + 1] = {
                "<leader>tD",
                function()
                    require("neotest").run.run { strategy = "dap" }
                end,
                mode = "n",
                desc = "Debug File",
            }
            -- debug nearest test
            keys[#keys + 1] = {
                "<leader>td",
                function()
                    require("neotest").run.run { vim.fn.expand "%", strategy = "dap" }
                end,
                mode = "n",
                desc = "Debug Nearest",
            }
        end,
    },
    {
        "nvim-neotest/neotest",
        dependencies = {
            "nvim-neotest/nvim-nio",
            "nvim-lua/plenary.nvim",
            "antoinemadec/FixCursorHold.nvim",
            "nvim-treesitter/nvim-treesitter",
        },
        opts = {
            adapters = {
                ["neotest-java"] = {
                    -- config here
                },
            },
        },
    },

    --https://github.com/nvim-java/neotest-jdtls
    --https://github.com/atm1020/neotest-jdtls
    --[[{
        "atm1020/neotest-jdtls",
        dependencies = {
            "nvim-java/nvim-java-core",
            "nvim-java/nvim-java-test",
        },
        config = function()
            require("neotest").setup {
                adapters = {
                    require('neotest-jdtls')
                },
            }
        end
    }]]
}