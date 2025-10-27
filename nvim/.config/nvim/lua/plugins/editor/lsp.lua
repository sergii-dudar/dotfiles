-- disable lsp right bottom message spam about lsp progress\status
-- but maybe useful in case debugging, then comment next handlers:
-- vim.lsp.handlers['language/status'] = function(_, result)
-- end
vim.lsp.handlers["$/progress"] = function(_, result, ctx) end
-- vim.lsp.handlers['textDocument/inlayHint'] = function() end

vim.diagnostic.config({
    float = { border = "rounded" },
})

return {
    -- Fully customizable previewer for LSP code actions.
    {
        "aznhe21/actions-preview.nvim",
        config = function()
            require("actions-preview").setup({
                highlight_command = {
                    require("actions-preview.highlight").delta(),
                    -- require("actions-preview.highlight").diff_so_fancy(),
                    -- require("actions-preview.highlight").diff_highlight(),
                },
                backend = { "snacks" },
                snacks = {
                    layout = {
                        preset = function()
                            return vim.o.columns >= 120 and "custom_horizontal" or "custom_vertical"
                        end,
                    },
                },
            })
        end,
    },
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "aznhe21/actions-preview.nvim",
        },
        opts = {
            servers = {
                ["*"] = {
                    -- stylua: ignore
                    keys = {
                        { "K", false },
                        { "<leader>k", function() return vim.lsp.buf.hover() end, desc = "Hover", },
                        { "<leader>ca", function() require("actions-preview").code_actions() end, desc = "Code Action (With Preview)", mode = { "n", "x" }, has = "codeAction", },
                        -- { "<F1>", function() return vim.lsp.buf.hover() end, desc = "Hover", },
                    },
                },
            },
        },
    },
    -- Nvim plugin for nvim-lspconfig: stop idle servers & restart upon focus; keep your RAM usage low
    -- {
    --     "hinell/lsp-timeout.nvim",
    --     dependencies = { "neovim/nvim-lspconfig" },
    -- },
    -- LSP diagnostics in virtual text at the top right of your screen
    {
        "dgagn/diagflow.nvim",
        -- event = 'LspAttach', This is what I use personnally and it works great
        opts = {
            show_borders = true,
        },
        config = function()
            require("diagflow").setup()
        end,
    },
    -- A small Neovim plugin for previewing definitions using floating windows.
    {
        "rmagatti/goto-preview",
        event = "BufEnter",
        dependencies = { "folke/which-key.nvim", "rmagatti/logger.nvim" },
        config = function()
            require("goto-preview").setup({
                default_mappings = false,
                preview_window_title = { enable = false },
                references = { -- Configure the telescope UI for slowing the references cycling window.
                    provider = "snacks", -- telescope|fzf_lua|snacks|mini_pick|default
                    -- telescope = require("telescope.themes").get_dropdown({ hide_preview = false }),
                },
                post_open_hook = function(buffer, window)
                    --vim.api.nvim_buf_set_keymap(0, "n", "q", ":q<CR>", { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(0, "n", "q", ":bdelete<CR>", { noremap = true, silent = true })
                end,
                --[[post_close_hook = function(buffer, window)
                    --vim.api.nvim_buf_delete(buffer, {})
                    vim.notify('deleted' .. buffer)
                end]]
            })

            local preview = require("goto-preview")
            local wk = require("which-key")
            wk.add({
                {
                    mode = { "n" },
                    { "<leader>p", group = "LPS [P]review" },
                    { "<leader>pd", preview.goto_preview_definition, desc = "Preview [d]efinition" },
                    { "<leader>pt", preview.goto_preview_type_definition, desc = "Preview [t]ype definition" },
                    { "<leader>pi", preview.goto_preview_implementation, desc = "Preview [i]implementation" },
                    { "<leader>pD", preview.goto_preview_declaration, desc = "Preview [D]declaration" },
                    { "<leader>pr", preview.goto_preview_references, desc = "Preview [r]references" },
                    { "<leader>pP", preview.close_all_win, desc = "close all preview windows" },
                },
            })
        end,
    },
    --{
    --    "ray-x/lsp_signature.nvim",
    --    event = "VeryLazy",
    --    opts = {},
    --    config = function(_, opts)
    --        require('lsp_signature').setup(opts)
    --    end
    --},
}