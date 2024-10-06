return {
    {
        "stevearc/conform.nvim",
        opts = function(_, opts)
            opts.formatters_by_ft.xml = { "xmlformatter" }
            opts.formatters_by_ft.kotlin = { "ktfmt" }
            opts.formatters_by_ft.sh = { "beautysh" }
            opts.formatters_by_ft.python = { "black", }
            opts.formatters_by_ft.lua = { "stylua" }
            --opts.formatters_by_ft.java = { "google-java-format" }
            --opts.formatters_by_ft.java = { "spotless_custom" }

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
            --    command = "/home/serhii/dotfiles/work/formatter/spotless.sh",
            --    --args = { "-" },
            --    args = {
            --      "$FILENAME",
            --    },
            --    stdin = false,
            --}
            --formatters = {
            --    --spotless_custom = {
            --    --    command = "/home/serhii/dotfiles/work/formatter/spotless.sh",
            --    --    args = { "-" }
            --    --}
            --},

        end
    }
}