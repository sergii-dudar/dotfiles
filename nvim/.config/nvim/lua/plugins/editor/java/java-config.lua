local home = os.getenv("HOME")
local java_util = require("utils.java.java-util")
local notify_title = { title = "Spring Boot Tools LS" }
--vim.lsp.set_log_level("warn")

return {
    -- vs spring-boot tools ls to integrate in jdtls
    --[[{
        "JavaHello/spring-boot.nvim",
        -- latest working only with vmware.vscode-spring-boot-1.57.0, need manual install tools
        -- "commit": "7c2c9d90691e536bb00941a143e961f4c8db647d"
        commit = "218c0c26c14d99feca778e4d13f5ec3e8b1b60f0", -- stable, working with mason spring-boot-tools@1.55.1
        ft = "java",
        dependencies = {
            "mfussenegger/nvim-jdtls",
            "ibhagwan/fzf-lua",
        },
        config = function()
            -- requiring install https://marketplace.visualstudio.com/items?itemName=vmware.vscode-spring-boot
            -- https://github.com/spring-projects/sts4/releases

            require("spring_boot").setup({
                --ls_path = vim.fn.glob(home .. "/.vscode/extensions/vmware.vscode-spring-boot-1.57.0/language-server")
                --ls_path = vim.fn.glob(home .. "/.vscode/extensions/vmware.vscode-spring-boot-1.56.0/language-server")
                ls_path = vim.fn.expand("$MASON/packages/spring-boot-tools/extension/language-server"),
            })
        end,
    },]]
    -- JDTLS config based on LazyVim with Spring-Boot Tools LS support
    {
        "mfussenegger/nvim-jdtls",
        dependencies = {
            "mason-org/mason.nvim",
            -- "JavaHello/spring-boot.nvim",
        },
        keys = {
            { "<leader>jc", ":JdtCompile<CR>", desc = "JDTLS Compile" },
            { "<leader>jf", ":JdtCompile full<CR>", desc = "JDTLS Compile Full" },
            { "<leader>jr", ":JdtRestart<CR>", desc = "JDTLS Restart" },
            {
                "<leader>tg",
                function()
                    require("jdtls.tests"):generate()
                end,
                desc = "[G]enerate Tests",
            },
            {
                "<leader>tj",
                function()
                    require("jdtls.tests").goto_subjects()
                end,
                desc = "[J]ump to tests ",
            },

            {
                "<leader>ci",
                function()
                    require("utils.java.java-import-util").import_class_and_replace()
                end,
                desc = "[I]mport class package and apply simple name",
            },
        },
        opts = {
            --[[jdtls = {
                on_attach = function()
                    require("spring_boot").init_lsp_commands()
                    LazyVim.info("jdtls lsp initialized", notify_title)
                end,
            },
            extend_jdtls_bundles = function(bundles)
                vim.list_extend(bundles, require("spring_boot").java_extensions())
                LazyVim.info("jdtls bundles extended", notify_title)
            end,]]
            settings = java_util.jdtls_settings,
        },
    },
    -- Rename packages and imports also when renaming/moving files via nvim-tree (for Java)
    -- {
    --     "simaxme/java.nvim",
    --     -- "sergii-dudar/java.nvim",
    --     ft = "java",
    --     dependencies = { "mfussenegger/nvim-jdtls" },
    --     config = function()
    --         require("simaxme-java").setup()
    --     end,
    -- },
    -- Fully customizable previewer for LSP code actions.
    --{
    --    "aznhe21/actions-preview.nvim",
    --    config = function()
    --        vim.keymap.set({ "v", "n" }, "gf", require("actions-preview").code_actions)
    --    end,
    --}
    --{
    --    "elmcgill/springboot-nvim",
    --    enabled = false,
    --    dependencies = {
    --        "neovim/nvim-lspconfig",
    --        "mfussenegger/nvim-jdtls"
    --    },
    --    config = function()
    --        -- gain acces to the springboot nvim plugin and its functions
    --        local springboot_nvim = require("spring-boot")
    --
    --        -- set a vim motion to <Space> + <Shift>J + r to run the spring boot project in a vim terminal
    --        vim.keymap.set('n', '<leader>Jr', springboot_nvim.boot_run, { desc = "[J]ava [R]un Spring Boot" })
    --        -- set a vim motion to <Space> + <Shift>J + c to open the generate class ui to create a class
    --        vim.keymap.set('n', '<leader>Jc', springboot_nvim.generate_class, { desc = "[J]ava Create [C]lass" })
    --        -- set a vim motion to <Space> + <Shift>J + i to open the generate interface ui to create an interface
    --        vim.keymap.set('n', '<leader>Ji', springboot_nvim.generate_interface, { desc = "[J]ava Create [I]nterface" })
    --        -- set a vim motion to <Space> + <Shift>J + e to open the generate enum ui to create an enum
    --        vim.keymap.set('n', '<leader>Je', springboot_nvim.generate_enum, { desc = "[J]ava Create [E]num" })
    --
    --        -- run the setup function with default configuration
    --        springboot_nvim.setup({})
    --    end
    --}
}
