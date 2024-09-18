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
                java = {
                    inherit = false,
                    command = "cd /home/serhii/serhii.home/work/git.work/ticket-service && ./mvnw spotless:apply", -- Use 'cmd' instead of 'command'
                    args = {  }, -- These are correct
                    stdin = false, -- 'mvn spotless:apply' doesn't use stdin, so set this to false
                },
                --java = {
                --    inherit = false,
                --    command = "mvn",
                --    args = { "spotless:apply" },
                --    --args = { "spotless:apply", "-DspotlessFiles", "$FILENAME"},
                --    --args = { "spotless:apply", "-DspotlessFiles=/home/serhii/serhii.home/work/git.work/ticket-service/src/main/java/com/hitachirail/pass/service/ticket/api/service/TicketService.java"},
                --}
            }
        }
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