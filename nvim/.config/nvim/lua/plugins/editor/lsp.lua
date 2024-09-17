vim.lsp.handlers["textDocument/hover"] = function(_, result, ctx, config)
    config = config
        or {
        border = {
            { "╭", "Comment" },
            { "─", "Comment" },
            { "╮", "Comment" },
            { "│", "Comment" },
            { "╯", "Comment" },
            { "─", "Comment" },
            { "╰", "Comment" },
            { "│", "Comment" },
        },
    }
    config.focus_id = ctx.method
    if not (result and result.contents) then
        return
    end
    local markdown_lines = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
    markdown_lines = vim.lsp.util.trim_empty_lines(markdown_lines)
    if vim.tbl_isempty(markdown_lines) then
        return
    end
    return vim.lsp.util.open_floating_preview(markdown_lines, "markdown", config)
end

vim.diagnostic.config {
    float = { border = "rounded" },
}

return {
    {
        "williamboman/mason.nvim",
        opts = {
            ensure_installed = {
                "lemminx",
                -- "bashls",
                "shellcheck",
            }
        },
    },
    --{
    --    "neovim/nvim-lspconfig",
    --      opts = function()
    --          local keys = require("lazyvim.plugins.lsp.keymaps").get()
    --          keys[#keys + 1] = { "K", false }
    --          keys[#keys + 1] = { "<leader>k", vim.lsp.buf.hover, desc = "Hover" }
    --      end,
    --},
    {
        'dgagn/diagflow.nvim',
        -- event = 'LspAttach', This is what I use personnally and it works great
        opts = {
            show_borders = true
        },
        config = function()
            require('diagflow').setup()
        end
    },
    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            -- Autocompletion
            --{ "hrsh7th/nvim-cmp" }, -- Required
            --{ "hrsh7th/cmp-nvim-lsp" }, -- Required
            --{ "L3MON4D3/LuaSnip" }, -- Required
            --{ "rafamadriz/friendly-snippets" },
            --{ "hrsh7th/cmp-buffer" },
            --{ "hrsh7th/cmp-path" },
            --{ "hrsh7th/cmp-cmdline" },
            --{ "saadparwaiz1/cmp_luasnip" }
        },
        opts = function(_, opts)
            --opts.completion.autocomplete = false
            --opts.mapping["<CR>"] = nil
            opts.window = {
                completion = {
                    border = {
                        { "󱐋", "WarningMsg" },
                        { "─", "Comment" },
                        { "╮", "Comment" },
                        { "│", "Comment" },
                        { "╯", "Comment" },
                        { "─", "Comment" },
                        { "╰", "Comment" },
                        { "│", "Comment" },
                    },
                    --scrollbar = false,
                    --winblend = 0,
                },
                documentation = {
                    border = {
                        { "󰙎", "DiagnosticHint" },
                        { "─", "Comment" },
                        { "╮", "Comment" },
                        { "│", "Comment" },
                        { "╯", "Comment" },
                        { "─", "Comment" },
                        { "╰", "Comment" },
                        { "│", "Comment" },
                    },
                    --scrollbar = false,
                    --winblend = 0,
                },
            }

            local cmp = require("cmp")
            opts.mapping = vim.tbl_deep_extend("force", opts.mapping, {
                ["<C-k>"] = cmp.mapping.select_prev_item(),
                ["<C-j>"] = cmp.mapping.select_next_item()
            })


            --table.insert(opts.sources, { name = "path" })

            --opts.sources = {
            --    { name = "nvim_lsp" }
            --}
            --opts.sources = {
            --    { name = "nvim_lsp" },
            --    { name = "luasnip", keyword_length = 2 },
            --    { name = "buffer", keyword_length = 3 },
            --    { name = "path" },
            --}

           --opts.sources = cmp.config.sources({
           --    { name = "nvim_lsp" },
           --    { name = "path" },
           --}, {
           --    { name = "buffer" },
           --})
        end,
    },
    {
        "hrsh7th/cmp-cmdline",
        dependencies = {
            "hrsh7th/nvim-cmp"
        },
        config = function()
            local cmp = require("cmp")

            -- `/` cmdline setup.
            cmp.setup.cmdline("/", {
                mapping = cmp.mapping.preset.cmdline(),
                --mapping = opts.mapping;
                sources = {
                    { name = "buffer" },
                },
            })

            -- `:` cmdline setup.
            cmp.setup.cmdline(":", {
                mapping = cmp.mapping.preset.cmdline(),
                --mapping = opts.mapping;
                sources = cmp.config.sources({
                    { name = "path" },
                }, {
                    {
                        name = "cmdline",
                        option = {
                            ignore_cmds = { "Man", "!", "q", "w", "wa", "qa" },
                        },
                    },
                }),
            })
        end
    }
}