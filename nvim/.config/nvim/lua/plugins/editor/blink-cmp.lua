-- local buffer_util = require("utils.buffer-util")

-- local json_string = '{"name": "Neovim", "version": 0.10, "is_editor": true, "plugins": ["telescope", "lsp"]}'
-- local data_table = vim.json.decode(json_string)
-- dd(data_table)

return {
    {
        "saghen/blink.cmp",
        enabled = true,
        dependencies = {
            --"hrsh7th/cmp-cmdline",
            "nvim-tree/nvim-web-devicons",
            "onsails/lspkind.nvim",
            --"echasnovski/mini.icons",
        },
        opts = {
            keymap = {
                -- https://cmp.saghen.dev/configuration/keymap.html#presets
                preset = "enter",
                -- ["<Tab>"] = {
                --     LazyVim.cmp.map({ "snippet_forward", "ai_accept" }),
                --     "fallback",
                -- },
                ["<C-k>"] = { "select_prev", "fallback" },
                ["<C-j>"] = { "select_next", "fallback" },
                -- ["<C-h>"] = {
                --     function()
                --         require("pretty_hover").close()
                --     end,
                -- },

                -- ["<Space>"] = { "select_and_accept", "fallback" },
                -- ["<C-h>"] = { "show_signature", "hide_signature", "fallback" },

                -- show with a list of providers
                -- ["<C-space>"] = {
                --     function(cmp)
                --         cmp.show({ providers = { "snippets" } })
                --     end,
                -- },
                -- control whether the next command will be run when using a function
            },
            fuzzy = {
                implementation = "rust", -- prefer_rust_with_warning(default)|prefer_rust|rust|lua
            },
            completion = {
                menu = {
                    auto_show = true,
                    scrollbar = false,
                    border = "rounded",
                    draw = {
                        columns = {
                            { "kind_icon", "label", "label_description", gap = 2 },
                            { "kind" },
                            { "source_name" },
                            -- { "source_id" },
                        },
                        -- components = { -- mini.icons
                        --     kind_icon = {
                        --         text = function(ctx)
                        --             local kind_icon, _, _ = require("mini.icons").get("lsp", ctx.kind)
                        --             return kind_icon
                        --         end,
                        --         -- (optional) use highlights from mini.icons
                        --         highlight = function(ctx)
                        --             local _, hl, _ = require("mini.icons").get("lsp", ctx.kind)
                        --             return hl
                        --         end,
                        --     },
                        --     kind = {
                        --         -- (optional) use highlights from mini.icons
                        --         highlight = function(ctx)
                        --             local _, hl, _ = require("mini.icons").get("lsp", ctx.kind)
                        --             return hl
                        --         end,
                        --     },
                        -- },
                        components = { -- nvim-web-devicons
                            kind_icon = {
                                text = function(ctx)
                                    local icon = ctx.kind_icon
                                    if vim.tbl_contains({ "Path" }, ctx.source_name) then
                                        local dev_icon, _ = require("nvim-web-devicons").get_icon(ctx.label)
                                        if dev_icon then
                                            icon = dev_icon
                                        end
                                    elseif ctx.source_name == "mapstruct" then
                                        -- Use appropriate icon for MapStruct fields/methods
                                        icon = require("lspkind").symbol_map[ctx.kind] or "󰜢"
                                    else
                                        icon = require("lspkind").symbol_map[ctx.kind]
                                    end

                                    return (icon or "󰦗") .. ctx.icon_gap
                                end,

                                -- Optionally, use the highlight groups from nvim-web-devicons
                                -- You can also add the same function for `kind.highlight` if you want to
                                -- keep the highlight groups in sync with the icons.
                                highlight = function(ctx)
                                    local hl = ctx.kind_hl
                                    if vim.tbl_contains({ "Path" }, ctx.source_name) then
                                        local dev_icon, dev_hl = require("nvim-web-devicons").get_icon(ctx.label)
                                        if dev_icon then
                                            hl = dev_hl
                                        end
                                    end
                                    return hl
                                end,
                            },
                            source_name = {
                                text = function(ctx)
                                    if ctx.source_name == "mapstruct" then
                                        -- return "[MS]" -- Short display for MapStruct
                                        return "MapStruct"
                                    end
                                    return ctx.source_name
                                end,
                            },
                        },
                    },
                },
                documentation = {
                    auto_show = true,
                    window = {
                        border = "rounded",
                    },
                },
                ghost_text = {
                    enabled = true,
                    show_with_menu = true,
                },
            },
            -- Experimental signature help support
            signature = { -- disabled as using lsp doc
                enabled = true,
                window = {
                    border = "rounded",
                    show_documentation = true, -- to only show the signature, and not the documentation.
                },
            },
            cmdline = {
                keymap = {
                    -- recommended, as the default keymap will only show and select the next item
                    -- ["<Tab>"] = { "show", "accept" },
                    -- ["<Tab>"] = { "accept" },
                    -- ["<CR>"] = { "accept_and_enter", "fallback" },
                    ["<C-k>"] = { "select_prev", "fallback" },
                    ["<C-j>"] = { "select_next", "fallback" },
                    -- ["<C-k>"] = { "show_signature", "hide_signature", "fallback" },
                    -- ["C-<Space>"] = { "accept", "fallback" },
                },
                enabled = true,
                -- keymap = { preset = "inherit" },
                -- completion = { menu = { auto_show = true } },
            },
            sources = {
                providers = {
                    lsp = { fallbacks = {} },
                    path = {
                        opts = {
                            show_hidden_files_by_default = true,
                        },
                    },
                    -- Buffer completion from all open buffers
                    -- buffer = {
                    --     opts = {
                    --         -- get all buffers, even ones like neo-tree
                    --         -- get_bufnrs = vim.api.nvim_list_bufs,
                    --
                    --         -- or (recommended) filter to only "normal" buffers
                    --         -- get_bufnrs = function()
                    --         --     return vim.tbl_filter(function(bufnr)
                    --         --         return vim.bo[bufnr].buftype == ""
                    --         --     end, vim.api.nvim_list_bufs())
                    --         -- end,
                    --
                    --         get_bufnrs = buffer_util.get_active_ls_buffers,
                    --     },
                    -- },
                    mapstruct = {
                        name = "mapstruct",
                        module = "utils.blink.mapstruct-source",

                        opts = {
                            -- Required: path to mapstruct-path-explorer.jar
                            jar_path = "~/tools/java-extensions/mapstruct/mapstruct-path-explorer.jar",
                            -- jar_path = "~/serhii.home/personal/git/mapstruct-path-explorer/target/mapstruct-path-explorer.jar",

                            -- Optional: use jdtls classpath (default: true)
                            use_jdtls_classpath = true,

                            -- Optional: manual classpath (fallback if jdtls fails)
                            -- classpath = nil,

                            -- Optional: custom Java command
                            -- java_cmd = "java",

                            -- Optional: log level (default: vim.log.levels.INFO)
                            -- Can be vim.log.levels.* or string ("DEBUG", "INFO", etc.)
                            -- Controls both Java and Lua logging
                            log_level = vim.log.levels.DEBUG,

                            -- Optional: Java server log file (default: ~/.local/state/nvim/mapstruct-source-server.log)
                            -- log_file = "~/.local/state/nvim/mapstruct-source-server.log",
                        },
                    },
                },
                default = { "lsp", "mapstruct", "path", "snippets", "buffer" },
                -- default = { "mapstruct" },
            },
            -- snippets = { preset = 'default' | 'luasnip' | 'mini_snippets' },
            snippets = { preset = "luasnip" },
        },
    },
}
