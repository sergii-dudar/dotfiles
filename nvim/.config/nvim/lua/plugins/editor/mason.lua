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
            --"lemminx",
            "jdtls",
            --"lombok-nightly",
            --"spring-boot-tools", -- nvim-java/mason-registry

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
            --"black",
            "ruff",
            "stylua",
            "shellharden",
        },
    },
}