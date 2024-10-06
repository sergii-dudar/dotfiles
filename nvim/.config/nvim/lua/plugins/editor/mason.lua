return {
    "williamboman/mason.nvim",
    opts = {
        ensure_installed = {
            -- LSP
            "bash-language-server",
            "lemminx",

            -- DAP
            "bash-debug-adapter",

            -- Linter
            "checkstyle",
            "ktlint",
            "shellcheck",
            "ruff",
            "luacheck",

            -- Formatter
            "xmlformatter",
            "google-java-format",
            "ktfmt",
            "beautysh",
            "black",
            "stylua"
        }
    }
}