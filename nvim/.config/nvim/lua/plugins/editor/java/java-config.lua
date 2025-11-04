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
    {
        "JavaHello/spring-boot.nvim", --"eslam-allam/spring-boot.nvim"
        version = "*",
        -- ft = { "java", "yaml", "jproperties" },
        ft = "java",
        dependencies = {
            "mfussenegger/nvim-jdtls",
        },
        opts = function()
            local opts = {}
            -- opts.ls_path = mason_registery.get_package("spring-boot-tools"):get_install_path()
            -- 	.. "/extension/language-server"

            -- /Users/jgarcia/.local/share/nvim/mason/packages/spring-boot-tools/extension/language-server/spring-boot-language-server-1.59.0-SNAPSHOT-exec.jar
            -- opts.ls_path = os.getenv("MASON") .. "/packages/spring-boot-tools/extension/language-server/spring-boot-language-server-1.59.0-SNAPSHOT-exec.jar"
            opts.ls_path = vim.fn.glob("$MASON/share/vscode-spring-boot-tools/*.jar")
            -- print("jdtls opts.ls_path: ", opts.ls_path)

            -- jdtls opts.ls_path:  /Users/jgarcia/.local/share/nvim/mason/packages/spring-boot-tools/extension/language-server
            -- opts.ls_path = "/home/sangram/.vscode/extensions/vmware.vscode-spring-boot-1.55.1"
            -- vim.notify("spring boot ls path : " .. opts.ls_path, vim.log.levels.INFO, {title = "Spring boot"})
            opts.java_cmd = "java"
            -- opts.exploded_ls_jar_data = true
            opts.jdtls_name = "jdtls"
            opts.log_file = home .. "/.local/state/nvim/spring-boot-ls.log"
            return opts
        end,
    },
    -- JDTLS config based on LazyVim with Spring-Boot Tools LS support
    {
        "mfussenegger/nvim-jdtls",
        dependencies = {
            "mason-org/mason.nvim",
            "JavaHello/spring-boot.nvim",
            -- {
            --     "JavaHello/spring-boot.nvim",
            --     ft = { "java", "yaml", "jproperties" },
            --     opts = {},
            -- },
        },
        -- stylua: ignore
        keys = {
            { "<leader>jc", ":JdtCompile<CR>", desc = "JDTLS Compile" },
            { "<leader>jf", ":JdtCompile full<CR>", desc = "JDTLS Compile Full" },
            { "<leader>ji", ":JdtCompile incremental<CR>", desc = "JDTLS Compile Incremental" },
            { "<leader>jr", ":JdtRestart<CR>", desc = "JDTLS Restart" },
            { "<leader>tg", function() require("jdtls.tests"):generate() end, desc = "[G]enerate Tests", },
            { "<leader>tj", function() require("jdtls.tests").goto_subjects() end, desc = "[J]ump to tests ", },
            { "<leader>ci", function() require("utils.java.java-import-util").import_class_and_replace() end, desc = "[I]mport class package and apply simple name", },
        },
        opts = {
            --[[jdtls = {
                on_attach = function()
                    require("spring_boot").init_lsp_commands()
                    LazyVim.info("jdtls lsp initialized", notify_title)
                end,
            },]]
            extend_jdtls_bundles = function(bundles)
                vim.list_extend(bundles, require("spring_boot").java_extensions())
                vim.notify("jdtls bundles extende ", vim.log.levels.INFO)
            end,
            settings = java_util.jdtls_settings,
        },
    },
    -- Rename packages and imports also when renaming/moving files via nvim-tree (for Java)
    {
        -- "simaxme/java.nvim",
        "sergii-dudar/java.nvim", -- my fork with neo-tree and oil.nvim support
        ft = "java",
        dependencies = {
            "mfussenegger/nvim-jdtls",
            "nvim-tree/nvim-tree.lua",
        },
        config = function()
            require("simaxme-java").setup({
                rename = {
                    nvimtree = false,
                    neotree = true,
                    oilnvim = true,
                },
            })
        end,
    },
    -- {
    --     "elmcgill/springboot-nvim",
    --     ft = "java",
    --     dependencies = {
    --         "neovim/nvim-lspconfig",
    --         "mfussenegger/nvim-jdtls",
    --     },
    --     -- stylua: ignore
    --     config = function()
    --         local springboot_nvim = require("springboot-nvim")
    --         -- vim.keymap.set("n", "<leader>cjr", springboot_nvim.boot_run, { desc = "Spring Boot Run Project" })
    --         vim.keymap.set("n", "<leader>cjc", springboot_nvim.generate_class, { desc = "[J]ava Create [C]lass" })
    --         vim.keymap.set("n", "<leader>cji", springboot_nvim.generate_interface, { desc = "[J]ava Create [I]nterface" })
    --         vim.keymap.set("n", "<leader>cje", springboot_nvim.generate_enum, { desc = "[J]ava Create [E]num" })
    --         springboot_nvim.setup({})
    --     end,
    -- },
}
