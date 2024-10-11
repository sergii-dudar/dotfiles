return {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons'  },
    opts = {
        options = {
            -- theme = "catppuccin"
            -- ... the rest of your lualine config
            --codedark,ayu_dark,iceberg_dark,nightfly,palenight,
            --onedark, pywal
            theme = 'iceberg_dark',
            section_separators = '',
            component_separators = ' '
        },
        sections = {
            lualine_y = {
                { "progress", separator = " ", padding = { left = 1, right = 0 } },
                { "location", padding = { left = 0, right = 0 } },
                { "'['..table.concat(vim.tbl_map(function(client) return client.name end, vim.lsp.get_active_clients()),\"|\")..']'", padding = { left = 0, right = 1 } }
            }
        }
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
    }
}