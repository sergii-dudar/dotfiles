--return {
--    'nvimdev/dashboard-nvim',
--    event = 'VimEnter',
--    config = function()
--        require('dashboard').setup {
--            theme = 'doom',
--            config = {
--                header = {
--                    "",
--                    "",
--                    " ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗",
--                    " ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║",
--                    " ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║",
--                    " ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║",
--                    " ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║",
--                    " ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝",
--                    "",
--                    " [ TIP: To exit Neovim, just power off your computer. ] ",
--                    "",
--                }, --your header
--                center = {
--                    {
--                        icon = "  ",
--                        desc = "Recent sessions                         ",
--                        shortcut = "SPC s l",
--                        action = "SessionLoad",
--                    },
--                    {
--                        icon = "  ",
--                        desc = "Find recent files                       ",
--                        action = "Telescope oldfiles",
--                        shortcut = "SPC f r",
--                    },
--                    {
--                        icon = "  ",
--                        desc = "Find files                              ",
--                        action = "Telescope find_files find_command=rg,--hidden,--files",
--                        shortcut = "SPC f f",
--                    },
--                    {
--                        desc = ' dotfiles',
--                        group = 'Number',
--                        action = 'Telescope dotfiles',
--                        key = 'd',
--                    },
--                    {
--                        icon = "  ",
--                        desc = "File browser                            ",
--                        action = "Telescope file_browser",
--                        shortcut = "SPC f b",
--                    },
--                    {
--                        icon = "  ",
--                        desc = "Find word                               ",
--                        action = "Telescope live_grep",
--                        shortcut = "SPC f w",
--                    },
--                    {
--                        icon = "  ",
--                        desc = "Load new theme                          ",
--                        action = "Telescope colorscheme",
--                        shortcut = "SPC h t",
--                    }
--                    --{
--                    --    icon = ' ',
--                    --    icon_hl = 'Title',
--                    --    desc = 'Find File           ',
--                    --    desc_hl = 'String',
--                    --    key = 'b',
--                    --    keymap = 'SPC f f',
--                    --    key_hl = 'Number',
--                    --    key_format = ' %s', -- remove default surrounding `[]`
--                    --    action = 'lua print(2)'
--                    --},
--                    --{
--                    --    icon = ' ',
--                    --    desc = 'Find Dotfiles',
--                    --    key = 'f',
--                    --    keymap = 'SPC f d',
--                    --    key_format = ' %s', -- remove default surrounding `[]`
--                    --    action = 'lua print(3)'
--                    --},
--                },
--                footer = { "", "🎉 If I'm using Neovim, then my Emacs config must be broken!" }  --your footer
--            }
--            -- config
--        }
--    end,
--    dependencies = { { 'nvim-tree/nvim-web-devicons' } }
--}

-- https://github.com/nvimdev/dashboard-nvim
-- https://github.com/goolord/alpha-nvim

return {
    "goolord/alpha-nvim",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },

    config = function()
        --require'alpha'.setup(require'alpha.themes.startify'.config)
        --require'alpha'.setup(require'alpha.themes.dashboard'.config)
        --require'alpha'.setup(require'alpha.themes.theta'.config)

        local alpha = require('alpha')
        local dashboard = require('alpha.themes.theta.config')

        -- Set custom header
        dashboard.section.header.val = {
            "",
            "",
            " ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗",
            " ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║",
            " ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║",
            " ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║",
            " ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║",
            " ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝",
            "",
            " [ TIP: Welcome to Neovim! ] ",
            "",
        }

        -- Set custom buttons
        --dashboard.section.buttons.val = {
        --    dashboard.button('r', '  Recent files', ':Telescope oldfiles<CR>'),
        --    dashboard.button('n', '  New file', ':ene <BAR> startinsert <CR>'),
        --    dashboard.button('q', '  Quit', ':qa<CR>'),
        --}

        -- Set custom footer
        dashboard.section.footer.val = { "🎉 It's always good return to neovim, it's like return to home :) 🎉" }

        -- Setup alpha with the dashboard theme
        --alpha.setup(dashboard.opts)
        alpha.setup(dashboard)
    end,
}
