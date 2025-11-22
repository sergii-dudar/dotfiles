local clients_lsp = function()
    local bufnr = vim.api.nvim_get_current_buf()

    local clients = vim.lsp.buf_get_clients(bufnr)
    if next(clients) == nil then
        return ""
    end

    local c = {}
    for _, client in pairs(clients) do
        table.insert(c, client.name)
    end
    return "\u{f085} " .. table.concat(c, " | \u{f085} ")
end
local get_filename = function()
    -- print(vim.fn.expand("%:p"):match("^.+/(.+)$"))
    local path = vim.api.nvim_eval_statusline("%f", {}).str
    return path:match("^.+/(.+)$")
end

local icons = LazyVim.config.icons
return {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
        options = {
            -- theme = "catppuccin"
            -- ... the rest of your lualine config
            --codedark,ayu_dark,iceberg_dark,nightfly,palenight,
            --onedark, pywal
            -- theme = "iceberg_dark",
            theme = "gruvbox-material",
            section_separators = "",
            component_separators = " ",
        },
        sections = {
            lualine_c = {
                LazyVim.lualine.root_dir(),
                {
                    "diagnostics",
                    symbols = {
                        error = icons.diagnostics.Error,
                        warn = icons.diagnostics.Warn,
                        info = icons.diagnostics.Info,
                        hint = icons.diagnostics.Hint,
                    },
                },
                { "filetype", icon_only = true, separator = "", padding = { left = 1, right = 0 } },
                {
                    LazyVim.lualine.pretty_path({
                        length = 0,
                        filename_hl = "GreenBold", -- "PurpleBold", "BlueBold", "Function", "QuickFixLine",
                        directory_hl = "Grey",
                        -- filename_hl = "LuaLineFileNameHl",
                        -- directory_hl = "LuaLineDirectoryHl",
                    }),
                },
                --{ get_filename, color = { gui = "bold" } },
            },
            lualine_y = {
                { "progress", separator = " ", padding = { left = 1, right = 1 } },
                { "location", padding = { left = 0, right = 1 } },
                --{ clients_lsp, color = { fg = "black", bg = "#676868", gui = "bold" } },
                --{ "'['..table.concat(vim.tbl_map(function(client) return client.name end, vim.lsp.get_active_clients()),\"|\")..']'", padding = { left = 0, right = 1 } }
                {
                    "lsp_status",
                    icon = "\u{f085}",
                    symbols = {
                        -- Standard unicode symbols to cycle through for LSP progress:
                        spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" },
                        -- Standard unicode symbol for when LSP is done:
                        done = "✓",
                        -- Delimiter inserted between LSP names:
                        separator = " | \u{f085} ",
                    },
                    -- List of LSP names to ignore (e.g., `null-ls`):
                    ignore_lsp = {},
                    -- Display the LSP name
                    show_name = true,
                    color = { fg = "black", bg = "#676868", gui = "bold" },
                },
            },
        },
        --sections = {
        --    lualine_a = { 'mode' },
        --    --lualine_b = { 'branch', 'diff', 'diagnostics' },
        --    lualine_b = {
        --        {'branch', icon = {'', align='right', color={fg='red'}}},
        --        'diff', 'diagnostics'
        --    },
        --    lualine_c = { 'filename' },
        --    lualine_x = { 'encoding', 'fileformat', 'filetype' },
        --    lualine_y = { 'progress' },
        --    lualine_z = {}
        --    --lualine_z = {'location'}
        --},
        --inactive_sections = {
        --    lualine_a = {},
        --    lualine_b = {},
        --    lualine_c = { 'filename' },
        --    lualine_x = { 'location' },
        --    lualine_y = {},
        --    lualine_z = {}
        --},
    },
}
