--[[
additional navigation:

1. Using Neovim's Built-in Navigation
Neovim has built-in commands to move through jump locations:
Ctrl+o: Move to the previous location (similar to "back" in IntelliJ).
Ctrl+i: Move to the next location (similar to "forward" in IntelliJ).

2.
gf - go to file under cursor,
gF - go to file under cursor and to number after : (File.java:20)

3.
:[line number] - got to line number in buffer

4.
cfdo %s/serhii_dudar/just_serhii/g | update | bd
cfdo %s/just_serhii/serhii_dudar/g | update | bd

]]

local home = os.getenv('HOME')
local java_util = require("utils.java-util")
local notify_title = { title = "Spring Boot Tools LS" }
--vim.lsp.set_log_level("warn")

return {
    -- vs spring-boot tools ls with integration in jdtls
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
                ls_path = vim.fn.glob(home .. "/.vscode/extensions/vmware.vscode-spring-boot-1.56.0/language-server")
            })
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
    -- JDTLS config based on LazyVim
    {
        "mfussenegger/nvim-jdtls",
        dependencies = {
            "williamboman/mason.nvim",
            "JavaHello/spring-boot.nvim",
        },
        keys = {
            { "<leader>jc", ":JdtCompile<CR>", desc = "JDTLS Compile" },
            { "<leader>jf", ":JdtCompile full<CR>", desc = "JDTLS Compile Full" },
            { "<leader>jr", ":JdtRestart<CR>", desc = "JDTLS Restart" },
        },
        opts = {
            jdtls = {
                on_attach = function()
                    require("spring_boot").init_lsp_commands()
                    LazyVim.info("jdtls lsp initialized", notify_title)
                end,
            },
            extend_jdtls_bundles = function(bundles)
                vim.list_extend(bundles, require("spring_boot").java_extensions())
                LazyVim.info("jdtls bundles extended", notify_title)
            end,
            settings = java_util.jdtls_settings
        }
    }
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