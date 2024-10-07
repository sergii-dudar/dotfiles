return {
    -- execute: NeotestJava setup
    -- It will download the JUnit standalone jar from https://mvnrepository.com/artifact/org.junit.platform/junit-platform-console-standalone

    -- TODO: working not very good for now, there problem with mvn project compilation of big projects
    -- waiting to stable version of `neotest-java` or `neotest-jdtls` adapters

    --[[{
        "nvim-neotest/neotest",
        dependencies = {
            "nvim-neotest/nvim-nio",
            "nvim-lua/plenary.nvim",
            "antoinemadec/FixCursorHold.nvim",
            "nvim-treesitter/nvim-treesitter"
        },
        opts = {
            adapters = {
                ["neotest-java"] = {
                    --ignore_wrapper = true, -- whether to ignore maven/gradle wrapper
                    --junit_jar = nil, -- default: /nvim/neotest-java/junit-platform-console-standalone-[version].jar
                    incremental_build = false
                },
            },
        },
    },
    {
        "rcasia/neotest-java",
        ft = "java",
        init = function()
            -- override the default keymaps.
            -- needed until neotest-java is integrated in LazyVim
            local keys = require("lazyvim.plugins.lsp.keymaps").get()
            -- run test file
            keys[#keys + 1] = {"<leader>tt", function() require("neotest").run.run(vim.fn.expand("%")) end, mode = "n" }
            -- run nearest test
            keys[#keys + 1] = {"<leader>tr", function() require("neotest").run.run() end, mode = "n" }
            -- debug test file
            keys[#keys + 1] = {"<leader>tD", function() require("neotest").run.run({ strategy = "dap" }) end, mode = "n" }
            -- debug nearest test
            keys[#keys + 1] = {"<leader>td", function() require("neotest").run.run({ vim.fn.expand("%"), strategy = "dap" }) end, mode = "n" }
        end,
    },]]

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