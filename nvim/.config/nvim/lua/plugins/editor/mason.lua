return {
    "mason-org/mason.nvim",
    opts = {
        ensure_installed = {
            -- LSP
            "bash-language-server",
            "lemminx",

            -- Java
            "jdtls",
            "vscode-spring-boot-tools",
            "vscode-java-dependency",
            "vscode-java-decompiler",

            -- "haskell-language-server", -- issues with securities on macos

            "hyprls",
            "rust-analyzer",

            -- DAP
            "bash-debug-adapter",
            -- "haskell-debug-adapter",

            -- Linter
            "checkstyle",
            "ktlint",
            "shellcheck",
            "ruff",
            "luacheck",
            "stylelint",

            -- Formatter
            "xmlformatter",
            "google-java-format",
            "ktfmt",

            -- ~/.local/share/nvim/mason/packages/beautysh/venv/bin/python -m pip install setuptools
            "beautysh",
            --"black",
            "ruff",
            "stylua",
            "shellharden",
            -- haskell
            "fourmolu",
            "prettier",
            "sqlfluff",

            -- CLI
            "tree-sitter-cli",
        },
    },
}