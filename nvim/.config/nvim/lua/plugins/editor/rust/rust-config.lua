return {
    {
        "mrcjkb/rustaceanvim",
        -- stylua: ignore
        keys = {
            { "<leader>jcc", function() vim.cmd.RustLsp { 'flyCheck', 'run' } end, desc = "Rust Compile" },
        },
        opts = {
        }
    },
}
