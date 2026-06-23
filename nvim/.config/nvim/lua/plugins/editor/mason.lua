return {
    "mason-org/mason.nvim",
    opts = function(_, opts)
        opts.ensure_installed = opts.ensure_installed or {}
        vim.list_extend(opts.ensure_installed, {
            -- LSP
            "bash-language-server",
            "lemminx",
            "autotools-language-server",

            -- Go
            "gopls",
            "golangci-lint",

            -- Java
            "jdtls",
            "vscode-spring-boot-tools",
            "vscode-java-dependency",
            "vscode-java-decompiler",
            "gradle-language-server",

            -- AI
            -- "copilot-language-server",

            -- "haskell-language-server", -- issues with securities on macos
            -- "hyprls",

            -- Rust
            "rust-analyzer",
            -- "bacon",
            -- "bacon-ls",

            -- DAP
            "bash-debug-adapter",
            -- "haskell-debug-adapter",
            "local-lua-debugger-vscode",

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
            "stylua",
            "shellharden",
            -- haskell
            "fourmolu",
            "prettier",
            "prettierd",
            "sqlfluff",

            -- CLI
            "tree-sitter-cli",

            -- some issue with lua_ls 3.16.0, use 3.15.0 for now
            -- MasonInstall lua-language-server@3.15.0
            -- "lua-language-server",
            "lua-language-server",
        })
    end,
    config = function(plugin, opts)
        -- Monkey-patch Package.install to skip if already installing
        -- Fixes race between ensure_installed and LazyVim LSP refresh
        local Package = require("mason-core.package")
        local orig_install = Package.install
        ---@diagnostic disable-next-line: duplicate-set-field
        Package.install = function(self, ...)
            if self:is_installing() then
                return
            end
            return orig_install(self, ...)
        end

        require("mason").setup(opts)
    end,
}