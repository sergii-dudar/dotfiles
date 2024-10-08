local java_util = require("utils.java-util")

return {
    {
        "JavaHello/spring-boot.nvim",
        ft = {"java", "yaml"},
        dependencies = {
            "mfussenegger/nvim-jdtls", -- or nvim-java, nvim-lspconfig
            "ibhagwan/fzf-lua", -- 可选
        },
        config = function()
            local boot = require('spring_boot')
            boot.setup({
                ls_path = java_util.get_spring_boot_tools_path_ls_path(),
            })

            boot.init_lsp_commands()

            --require("lspconfig").jdtls.setup {
            --    init_options = {
            --        bundles = boot.java_extensions(),
            --    },
            --}
        end
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
}