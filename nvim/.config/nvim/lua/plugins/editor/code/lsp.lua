local clean_javadoc = function(javadoc)
    --javadoc = javadoc:gsub(" %* ", ""):gsub("%[(.-)%]%((jdt://.-)%)", "**%1**"):gsub("(%*%*)\n", "%1")
    --javadoc = javadoc:gsub("%[(.-)%]%((jdt://.-)%)", "**%1**")
    javadoc = javadoc
        :gsub("%[(.-)%]%((jdt://.-)%)", "**%1**")
        --:gsub("%[(.-)%]%((https://.-)%)", "**%1**")
        --:gsub("%[(.-)%]%((http://.-)%)", "**%1**")
        :gsub(
            "(%*%*)\n",
            "%1"
        )
    return javadoc
end

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

    -- javadoc
    local bufnr = vim.api.nvim_get_current_buf()
    local filetype = vim.api.nvim_buf_get_option(bufnr, "filetype")
    if filetype == "java" and result.contents[2] and type(result.contents[2]) == "string" then
        if result.contents[3] then
            result.contents[2] = clean_javadoc(result.contents[2])
        else
            result.contents[1] = clean_javadoc(result.contents[2])
        end
    end

    local markdown_lines = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
    markdown_lines = vim.lsp.util.trim_empty_lines(markdown_lines)
    if vim.tbl_isempty(markdown_lines) then
        return
    end
    return vim.lsp.util.open_floating_preview(markdown_lines, "markdown", config)
end

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
    {
        "neovim/nvim-lspconfig",
        opts = function()
            local keys = require("lazyvim.plugins.lsp.keymaps").get()
            keys[#keys + 1] = { "K", false }
            keys[#keys + 1] = { "<leader>k", vim.lsp.buf.hover, desc = "Hover" }
        end,
    },
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
        dependencies = { "folke/which-key.nvim" },
        config = function()
            require("goto-preview").setup({
                default_mappings = false,
                preview_window_title = { enable = false },
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
    {
        "hrsh7th/nvim-cmp",
        event = "VeryLazy",
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
            --"hrsh7th/cmp-nvim-lsp-signature-help",
            "onsails/lspkind.nvim",
        },
        opts = function(_, opts)
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
                --["<C-k>"] = cmp.mapping.select_prev_item(),
                --["<C-j>"] = cmp.mapping.select_next_item()
                ["<C-k>"] = cmp.mapping.select_prev_item({
                    behavior = cmp.SelectBehavior.Insert,
                }),
                ["<C-j>"] = cmp.mapping.select_next_item({
                    behavior = cmp.SelectBehavior.Insert,
                }),
                --["<CR>"] = LazyVim.cmp.confirm({ select = true, cmp.ConfirmBehavior.Replace }),
                --["<CR>"] = LazyVim.cmp.confirm({ select = auto_select }),
                --["<C-y>"] = LazyVim.cmp.confirm({ select = true }),
                --["<S-CR>"] = LazyVim.cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace }),
            })

            --local current_format = opts.formatting.format
            local lspkind = require("lspkind")
            opts.formatting = {
                fields = { "kind", "abbr", "menu" },
                format = function(entry, vim_item)
                    local kind = lspkind.cmp_format({
                        mode = "symbol_text",
                        maxwidth = 50,
                    })(entry, vim_item)
                    local strings = vim.split(kind.kind, "%s", { trimempty = true })
                    kind.kind = " " .. (strings[1] or "") .. " "
                    kind.menu = "    " .. (strings[2] or "")

                    return kind
                end,
            }

            --[[ default `LazyVim`
            { {
                  group_index = 1,
                  name = "nvim_lsp"
              }, {
                  group_index = 1,
                  name = "path"
              }, {
                  group_index = 2,
                  name = "buffer"
              }, {
                  name = "snippets"
              }, {
                  group_index = 0,
                  name = "lazydev"
              }, {
                  name = "git"
              } }]]

            local buffer_util = require("utils.buffer-util")
            local custom_sources = {
                {
                    name = "nvim_lsp_signature_help",
                    priority = 100,
                    --group_index = 1,
                },
                {
                    name = "nvim_lsp",
                    priority = 100,
                    --group_index = 1,
                },
                {
                    name = "nvim_lua",
                    priority = 100,
                    --group_index = 1,
                },
                {
                    name = "path",
                    priority = 90,
                    --group_index = 1,
                },
                {
                    name = "luasnip",
                    keyword_length = 3,
                    max_item_count = 3,
                    autocomplete = true,
                    priority = 80,
                    --group_index = 3,
                },
                {
                    name = "buffer",
                    keyword_length = 3,
                    autocomplete = true,
                    max_item_count = 3,
                    priority = 50,
                    --group_index = 3,
                    option = {
                        get_bufnrs = buffer_util.get_active_ls_buffers,
                    },
                },
            }

            local list_util = require("utils.list-util")

            for i, source in ipairs(opts.sources) do
                local custom_source = list_util.find_by(custom_sources, "name", source.name)

                if custom_source then
                    opts.sources[i] = vim.tbl_deep_extend("force", source, custom_source)
                end
            end

            --vim.notify(vim.inspect(opts.sources))

            --log_table(opts.sources)

            --table.insert(opts.sources, 1, { name = "nvim_lsp_signature_help" })
            --log_table(opts.sources)

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
            "rcarriga/cmp-dap",
            "hrsh7th/nvim-cmp",
            "hrsh7th/cmp-nvim-lsp-document-symbol",
        },
        keys = { ":", "/", "?" },
        config = function(_, opts)
            local cmp = require("cmp")
            cmp.setup.cmdline({ "/", "?" }, {
                completion = {
                    completeopt = "menu,menuone,noselect",
                },
                mapping = cmp.mapping.preset.cmdline(),
                sources = cmp.config.sources({
                    { name = "nvim_lsp_document_symbol" },
                }, {
                    { name = "buffer" },
                }),
            })

            cmp.setup.cmdline(":", {
                completion = {
                    completeopt = "menu,menuone,noselect",
                },
                mapping = cmp.mapping.preset.cmdline(),
                sources = cmp.config.sources({
                    { name = "path" },
                }, {
                    { name = "cmdline" },
                }),
            })
        end,
    },
}
