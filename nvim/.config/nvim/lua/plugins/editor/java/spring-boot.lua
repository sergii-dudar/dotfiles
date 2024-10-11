local java_util = require("utils.java-util")
local home = os.getenv('HOME')
return {
    {
        "JavaHello/spring-boot.nvim",
        ft = "java",
        dependencies = {
            "mfussenegger/nvim-jdtls", -- or nvim-java, nvim-lspconfig
            "ibhagwan/fzf-lua", -- optional
        },
        config = function()
            --requiring install https://marketplace.visualstudio.com/items?itemName=vmware.vscode-spring-boot
            -- the easiest way until it will be added to mason is install [vscode and add extension vmware.vscode-spring-boot]

            require("spring_boot").setup({
                --ls_path = vim.fn.expand("$MASON/packages/spring-boot-tools/extension/language-server"),
                ls_path = vim.fn.glob(home.."/.vscode/extensions/vmware.vscode-spring-boot-1.56.0/language-server")
            })

            --[[local boot = require('spring_boot')

            local spring_boot_tools_path = java_util.get_spring_boot_tools_path_ls_path()
            vim.g.spring_boot = {
                jdt_extensions_path = spring_boot_tools_path,
                jdt_extensions_jars = {
                    "io.projectreactor.reactor-core.jar",
                    "org.reactivestreams.reactive-streams.jar",
                    "jdt-ls-commons.jar",
                    "jdt-ls-extension.jar",
                },
            }
            boot.setup({
                ls_path = spring_boot_tools_path,
                jdtls_name = "jdtls",
                log_file = nil,
                java_cmd = nil,
            })]]
        end,
    },
    -- Rename packages and imports also when renaming/moving files via nvim-tree (for Java)
    {
        "simaxme/java.nvim",
        ft = "java",
        dependencies = { "mfussenegger/nvim-jdtls" },
        config = function()
            require("simaxme-java").setup()
        end,
    },
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

    -- Sonarlint plugin
    --[[{
        "https://gitlab.com/schrieveslaach/sonarlint.nvim",
        ft = { "java", "python", "cpp", "typescript", "typescriptreact", "html", "text", "yaml", "yml", "toml" },
        config = function()
            require("sonarlint").setup({
                server = {
                    cmd = {
                        "sonarlint-language-server",
                        -- Ensure that sonarlint-language-server uses stdio channel
                        "-stdio",
                        "-analyzers",
                        -- paths to the analyzers you need, using those for python and java in this example
                        vim.fn.expand("$MASON/share/sonarlint-analyzers/sonarpython.jar"),
                        vim.fn.expand("$MASON/share/sonarlint-analyzers/sonarcfamily.jar"),
                        vim.fn.expand("$MASON/share/sonarlint-analyzers/sonarjava.jar"),
                        vim.fn.expand("$MASON/share/sonarlint-analyzers/sonarjs.jar"),
                        vim.fn.expand("$MASON/share/sonarlint-analyzers/sonarhtml.jar"),
                        vim.fn.expand("$MASON/share/sonarlint-analyzers/sonartext.jar.jar"),
                        vim.fn.expand("$MASON/share/sonarlint-analyzers/sonariac.jar"),
                        vim.fn.expand("$MASON/share/sonarlint-analyzers/sonarjavasymbolicexecution.jar"),
                    },
                    settings = {
                        sonarlint = {
                            pathToCompileCommands = vim.fn.getcwd() .. "/compile_commands.json",
                        },
                    },
                },
                filetypes = {
                    -- Tested and working
                    "python",
                    "cpp",
                    "java",
                    "typescript",
                    "html",
                },
            })
        end,
    },]]
}