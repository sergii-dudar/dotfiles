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
    {
        "neovim/nvim-lspconfig",
          opts = function()
              local keys = require("lazyvim.plugins.lsp.keymaps").get()
              keys[#keys + 1] = { "K", false }
              keys[#keys + 1] = { "<leader>k", vim.lsp.buf.hover, desc = "Hover" }
          end,
    },
    {
        "hrsh7th/nvim-cmp",
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
        end,
    }
}