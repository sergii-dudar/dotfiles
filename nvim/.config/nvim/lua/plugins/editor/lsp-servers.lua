return {
    -- ###### openapi #######
    {
        "armsnyder/openapi-language-server",
        -- build = "go install github.com/armsnyder/openapi-language-server@latest",
        build = "go install .",
        ft = { "yaml" },
        lazy = true,
    },
    {
        "neovim/nvim-lspconfig",
        ft = { "yaml" },
        opts = {
            servers = {
                ["openapi-language-server"] = {
                    cmd = { vim.fn.expand("~/go/bin/openapi-language-server") },
                    filetypes = { "yaml" },
                    root_dir = vim.fs.root(0, { ".git" }) or vim.uv.cwd(),
                    single_file_support = true,
                },
            },
        },
    },
}