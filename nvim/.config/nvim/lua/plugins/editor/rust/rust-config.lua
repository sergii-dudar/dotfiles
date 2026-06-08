return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            spec = {
                { "<leader>j", group = "+rust" },
                { "<leader>jc", group = "+rust code/compile" },
            },
        },
    },
    {
        "mrcjkb/rustaceanvim",
        -- stylua: ignore
        keys = {
            { "<leader>jcc", function() vim.cmd.RustLsp { "flyCheck", "run" } end, desc = "Rust Compile" },
        },
        opts = {},
    },
}