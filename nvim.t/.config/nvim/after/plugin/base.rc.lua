--require('lualine').setup({
--    options = {
--        -- theme = "catppuccin"
--        -- ... the rest of your lualine config
--        theme = 'codedark',
--        section_separators = '',
--        component_separators = ''
--    },
--    sections = {
--        lualine_a = { 'mode' },
--        lualine_b = { 'branch', 'diff', 'diagnostics' },
--        lualine_c = { 'filename' },
--        lualine_x = { 'encoding', 'fileformat', 'filetype' },
--        lualine_y = { 'progress' },
--        lualine_z = {}
--        --lualine_z = {'location'}
--    },
--    inactive_sections = {
--        lualine_a = {},
--        lualine_b = {},
--        lualine_c = { 'filename' },
--        lualine_x = { 'location' },
--        lualine_y = {},
--        lualine_z = {}
--    },
--})
--
--require("catppuccin").setup({
--    flavour = "mocha", -- latte, frappe, macchiato, mocha
--})
--
--vim.cmd.colorscheme "catppuccin"