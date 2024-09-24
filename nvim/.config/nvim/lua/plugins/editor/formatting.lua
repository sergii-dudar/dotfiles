return {
    {
        "williamboman/mason.nvim",
        opts = {
            ensure_installed = {
                "xmlformatter",
                "google-java-format",
                "ktfmt",
                "beautysh",
                "black",
                "stylua"
            }
        },
    },
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                xml = { "xmlformatter" },
                kotlin = { "ktfmt" },
                sh = { "beautysh" },
                python = { "black", },
                lua = { "stylua" },
                --java = { "google-java-format" },
                java = { "spotless_custom" },
                --spotless_formatter = {
                --java = { "spotless_custom" }
                --java = {
                --    inherit = false,
                --    --command = "bash /home/serhii/dotfiles/work/formatter/spotless.sh", -- Use 'cmd' instead of 'command'
                --    command = "/home/serhii/dotfiles/work/formatter/spotless-java.lua",
                --    cwd = require("conform.util").root_file({ "pom.xml", "build.gradle" }),
                --    --args = {  }, -- These are correct
                --    stdin = false, -- 'mvn spotless:apply' doesn't use stdin, so set this to false
                --},
                --java = {
                --    inherit = false,
                --    command = "mvn",
                --    args = { "spotless:apply" },
                --    --args = { "spotless:apply", "-DspotlessFiles", "$FILENAME"},
                --    --args = { "spotless:apply", "-DspotlessFiles=/home/serhii/serhii.home/work/git.work/ticket-service/src/main/java/com/hitachirail/pass/service/ticket/api/service/TicketService.java"},
                --}
            }
        },
        formatters = {
            --spotless_custom = {
            --    command = "/home/serhii/dotfiles/work/formatter/spotless.sh",
            --    args = { "-" }
            --}

            --spotless_custom = {
            --    -- The command to run Maven
            --    command = 'mvn',
            --    -- Pass the Spotless goal as arguments
            --    args = { 'spotless:apply' },
            --    -- Specify where to run the command from (optional)
            --    cwd = function()
            --        print("vim.fn.getcwd():"..vim.fn.getcwd())
            --        return vim.fn.getcwd()  -- This makes sure it runs in the current project directory
            --    end,
            --    -- Optional: define conditions for formatting, e.g., only on save
            --    format_on_save = true,
            --},


            --spotless_custom = {
            --    --args = { 'format', '--option', 'align_entries=true' },
            --    --inherit = false,
            --    --command = 'mvn spotless:apply',
            --    --args = { 'spotless:apply' },
            --    --inherit = false,

            --    --cwd = require("conform.util").root_file({ "pom.xml", "build.gradle" }),
            --    --require_cwd = true,
            --    --stdin = false,
            --    --command = 'echo',
            --    --args = { 'hello', 'world', '-' },
            --    ----args = 'hello world' --{ 'hello', 'world', '-' },

            --    command = '/home/serhii/.sdkman/candidates/maven/current/bin/mvn',
            --    args = "spotless:apply",
            --}
        },
        log_level = vim.log.levels.DEBUG,
    }
}

--return {
--    "stevearc/conform.nvim",
--    event = { "BufReadPre", "BufNewFile" },
--    config = function()
--        local conform = require("conform")
--
--        conform.setup({
--            formatters_by_ft = {
--                javascript = { "prettier" },
--                typescript = { "prettier" },
--                javascriptreact = { "prettier" },
--                typescriptreact = { "prettier" },
--                svelte = { "prettier" },
--                css = { "prettier" },
--                html = { "prettier" },
--                json = { "prettier" },
--                yaml = { "prettier" },
--                markdown = { "prettier" },
--                graphql = { "prettier" },
--                liquid = { "prettier" },
--                lua = { "stylua" },
--                python = { "isort", "black" },
--            },
--            format_on_save = {
--                lsp_fallback = true,
--                async = false,
--                timeout_ms = 1000,
--            },
--        })
--
--        vim.keymap.set({ "n", "v" }, "<leader>mp", function()
--            conform.format({
--                lsp_fallback = true,
--                async = false,
--                timeout_ms = 1000,
--            })
--        end, { desc = "Format file or range (in visual mode)" })
--    end,
--}