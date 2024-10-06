return {
    "nvim-treesitter/nvim-treesitter",
    opts = {
        ensure_installed = {
            "bash",
            "html",
            "javascript",
            "json",
            "lua",
            "markdown",
            "markdown_inline",
            "python",
            "query",
            "regex",
            "tsx",
            "typescript",
            "vim",
            "yaml",
            "java",
            "kotlin",
            "xml",
            "strace",
            "groovy",
            "terraform"
        }
    },
    {
        "nvim-treesitter/nvim-treesitter-context",
        enabled = false
    }
}