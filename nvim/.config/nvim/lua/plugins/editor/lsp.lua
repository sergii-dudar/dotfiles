-- disable lsp right bottom message spam about lsp progress\status
-- but maybe useful in case debugging, then comment next handlers:
-- vim.lsp.handlers['language/status'] = function(_, result)
-- end
vim.lsp.handlers["$/progress"] = function(_, result, ctx) end
-- vim.lsp.handlers['textDocument/inlayHint'] = function() end
-- local originalHint = vim.lsp.handlers['textDocument/inlayHint'] = function() end

vim.diagnostic.config({
    float = { border = "rounded" },
})

local original_handler = vim.lsp.buf_request_all
---@diagnostic disable-next-line: duplicate-set-field
vim.lsp.buf_request_all = function(bufnr, method, params, handler)
    if method ~= "textDocument/hover" then
        original_handler(bufnr, method, params, handler)
        return
    end

    -- INFO: extension with filtering empty hover results to not show them in pretty-hover plugin
    original_handler(bufnr, method, params, function(results, ctx)
        local non_empty_result = {}
        for client_id, resp in pairs(results) do
            local contents = resp and resp.result and resp.result.contents
            if contents then
                local cont_type = type(contents)
                if cont_type == "string" and contents ~= "" then
                    non_empty_result[client_id] = resp
                elseif cont_type == "table" and not vim.tbl_isempty(contents) then
                    non_empty_result[client_id] = resp
                end
            end
        end
        handler(non_empty_result, ctx)
    end)
end

return {
    -- A Neovim plugin for displaying inline diagnostic messages with customizable styles and icons.
    {
        "rachartier/tiny-inline-diagnostic.nvim",
        event = "VeryLazy",
        priority = 1000,
        opts = {
            preset = "modern", -- Available: "modern", "classic", "minimal", "powerline", "ghost", "simple", "nonerdfont", "amongus"
            transparent_bg = false,
            -- hi = {
            --     error = "DiagnosticError", -- Highlight for error diagnostics
            --     warn = "DiagnosticWarn", -- Highlight for warning diagnostics
            --     info = "DiagnosticInfo", -- Highlight for info diagnostics
            --     hint = "DiagnosticHint", -- Highlight for hint diagnostics
            --     arrow = "NonText", -- Highlight for the arrow pointing to diagnostic
            --     background = "CursorLine", -- Background highlight for diagnostics
            --     mixing_color = "Normal", -- Color to blend background with (or "None")
            -- },
            options = {
                use_icons_from_diagnostic = false,
                multilines = {
                    enabled = true, -- Enable support for multiline diagnostic messages
                    always_show = true, -- Always show messages on all lines of multiline diagnostics
                    trim_whitespaces = false, -- Remove leading/trailing whitespace from each line
                },
                show_source = { -- Display the source of diagnostics (e.g., "lua_ls", "pyright")
                    enabled = true, -- Enable showing source names
                    if_many = true, -- Only show source if multiple sources exist for the same diagnostic
                },
                -- Only show diagnostics when the cursor is directly over them, no fallback to line diagnostics
                show_diags_only_under_cursor = false,
            },
        },
    },
    -- Show diagnostics and lsp info inside a custom window
    -- {
    --     "soulis-1256/eagle.nvim",
    --     config = function()
    --         require("eagle").setup({
    --             keyboard_mode = true,
    --         })
    --         vim.keymap.set("n", "<Tab>", ":EagleWin<CR>", { noremap = true, silent = true })
    --     end,
    -- },
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
            inlay_hints = {
                enabled = true,
                -- exclude = { "java" }, -- filetypes for which you don't want to enable inlay hints
            },
            diagnostics = { virtual_text = false }, -- disable to use tiny-inline-diagnostic.nvim instead
            servers = {
                ["*"] = {
                    -- stylua: ignore
                    keys = {
                        { "K", false },
                        -- { "<leader>k", function() return vim.lsp.buf.hover() end, desc = "Hover", },
                        { "<leader>k", function() require("pretty_hover").hover() end, desc = "Pretty hover" },
                        -- override default preveiw mapping
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
    -- {
    --     "dgagn/diagflow.nvim",
    --     -- event = 'LspAttach', This is what I use personnally and it works great
    --     opts = {
    --         show_borders = true,
    --     },
    --     config = function()
    --         require("diagflow").setup()
    --     end,
    -- },
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
