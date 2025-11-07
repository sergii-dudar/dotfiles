local home = os.getenv("HOME")
local java_util = require("utils.java.java-util")
local notify_title = { title = "Spring Boot Tools LS" }
--vim.lsp.set_log_level("warn")

return {
    -- JDTLS config based on LazyVim with Spring-Boot Tools LS support
    {
        "mfussenegger/nvim-jdtls",
        dependencies = {
            "mason-org/mason.nvim",
            -- "JavaHello/spring-boot.nvim",
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
            -- extend_jdtls_bundles = function(bundles)
            --     vim.list_extend(bundles, require("spring_boot").java_extensions())
            --     vim.notify("jdtls bundles extende ", vim.log.levels.INFO)
            -- end,
            settings = java_util.jdtls_settings,
            -- test = false, -- issue with java-test in latest mason module, using from vscode build instead
        },
    },
    -- vs spring-boot tools ls to integrate in jdtls
    --[[ {
        "JavaHello/spring-boot.nvim", --"eslam-allam/spring-boot.nvim"
        version = "*",
        ft = { "java", "yaml", "properties", "yml" },
        dependencies = {
            "mfussenegger/nvim-jdtls",
        },
        opts = function()
            local opts = {}
            -- opts.ls_path = os.getenv("MASON") .. "/packages/spring-boot-tools/extension/language-server/spring-boot-language-server-1.59.0-SNAPSHOT-exec.jar"
            opts.ls_path = vim.fn.glob("$MASON/share/vscode-spring-boot-tools/*.jar")
            -- opts.ls_path = os.getenv("MASON") .. "/share/vscode-spring-boot-tools/language-server.jar"
            -- print(opts.ls_path)

            -- opts.java_cmd = "java"
            -- -- opts.exploded_ls_jar_data = true
            -- opts.jdtls_name = "jdtls"
            opts.log_file = home .. "/.local/state/nvim/spring-boot-ls.log"
            return opts
        end,
    }, ]]
    {
        "JavaHello/java-deps.nvim",
        ft = { "java" },
        lazy = true,
        -- stylua: ignore
        keys = {
            { "<leader>je", function() require('java-deps').toggle_outline() end, desc = "Toogle Java Dependencies" },
            -- :lua require('java-deps').open_outline()
            -- :lua require('java-deps').close_outline()
        },
        dependencies = {
            { "mfussenegger/nvim-jdtls" },
            { "hedyhli/outline.nvim" },
        },
        opts = {
            -- jdtls_name = "jdtls",
            options = {
                width = 60,
                show_relative_numbers = true,
                position = "right",
                --     show_guides = true,
                --     auto_close = false,
                --     show_numbers = false,
                --     preview_bg_highlight = "Pmenu",
                --     winblend = 0,
                --     fold_markers = { "", "" },
                --     position = "right",
                --     wrap = false,
                --     hierarchical_view = true,
                --     keymaps = {
                --         close = "q",
                --         toggle_fold = "o",
                --     },
                --     symbols = {
                --         icons = {},
                --     },
            },
        },
        config = true,
    },
    -- Rename packages and imports also when renaming/moving files via nvim-tree (for Java)
    {
        -- "simaxme/java.nvim",
        "sergii-dudar/java.nvim", -- my fork with [ neo-tree, oil.nvim, snacks rename ] support
        ft = "java",
        -- stylua: ignore
        keys = {
            { "<leader>cR", function() require("simaxme-java").snacks.rename_current() end, desc = "Rename File (Java)" },
        },
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
