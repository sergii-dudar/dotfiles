return {
    "williamboman/mason.nvim",
    opts = {
        registries = {
            "github:mason-org/mason-registry",
            -- "github:nvim-java/mason-registry",
        },
        ensure_installed = {
            -- LSP
            "bash-language-server",
            "lemminx",
            "jdtls",
            --"lombok-nightly",
            --"spring-boot-tools", -- nvim-java/mason-registry
            "haskell-language-server",

            -- DAP
            "bash-debug-adapter",
            "haskell-debug-adapter",

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
            --"black",
            "ruff",
            "stylua",
            "shellharden",
            -- haskell
            "fourmolu",
        },
    },
}