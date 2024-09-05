return {
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function()
            local config = require("nvim-treesitter.configs")
            config.setup({
                ensure_installed = { "json", "javascript", "lua", "python", "bash", "java" },
                auto_install = true,
                highlight = { enable = true },
                indent = { enable = true },
            })
        end
    }
}