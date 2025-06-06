return {
    {
        "stevearc/conform.nvim",
        opts = function(_, opts)
            --opts.formatters_by_ft.xml = { "xmlformatter" }
            opts.formatters_by_ft.kotlin = { "ktfmt" }
            opts.formatters_by_ft.sh = { "beautysh" }
            -- for beautysh need:
            -- source ~/.local/share/nvim/mason/packages/beautysh/venv/bin/activate
            -- pip install setuptools

            opts.formatters_by_ft.python = { "ruff" } --{ "black" }
            opts.formatters_by_ft.lua = { "stylua" }

            opts.formatters_by_ft.cs = {} -- disable default `csharpier` formatter, to use lsp omnisharp formatter
            -- opts.formatters_by_ft.cs = { "csharpier" }
            opts.formatters_by_ft.css = { "prettier" }

            --opts.formatters_by_ft.java = { "google-java-format" }
            --opts.formatters_by_ft.java = { "spotless_custom" }

            -- haskell
            -- opts.formatters_by_ft.haskell = { "fourmolu" }

            -- opts.formatters = {
            --     black = {
            --         prepend_args = { "--line-length", "1000" },
            --     },
            -- }

            --[[ opts.formatters.ronfmt = {
                command = "ronfmt",
                args = "$FILENAME",
                --args = { "$FILENAME" },
                stdin = false,
                inherit = false,
            }
            opts.formatters_by_ft.ron = {
                "ronfmt",
            } ]]
            opts.default_format_opts.timeout_ms = 3000

            --formatters_by_ft = {
            --    xml = { "xmlformatter" },
            --    kotlin = { "ktfmt" },
            --    sh = { "beautysh" },
            --    python = { "black", },
            --    lua = { "stylua" },
            --    --java = { "google-java-format" },
            --    java = { "spotless_custom" },
            --}

            --opts.formatters.spotless_custom = {
            --    --command = "google-java-format",
            --    command = "~/dotfiles/work/formatter/spotless.sh",
            --    --args = { "-" },
            --    args = {
            --      "$FILENAME",
            --    },
            --    stdin = false,
            --}
            --formatters = {
            --    --spotless_custom = {
            --    --    command = "~/dotfiles/work/formatter/spotless.sh",
            --    --    args = { "-" }
            --    --}
            --},
        end,
    },
}
