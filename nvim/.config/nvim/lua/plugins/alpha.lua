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

--return {
--    "goolord/alpha-nvim",
--    dependencies = {
--        "nvim-tree/nvim-web-devicons",
--    },
--
--    config = function()
--        --require'alpha'.setup(require'alpha.themes.startify'.config)
--        --require'alpha'.setup(require'alpha.themes.dashboard'.config)
--        --require'alpha'.setup(require'alpha.themes.theta'.config)
--
--        local alpha = require('alpha')
--        local dashboard = require('alpha.themes.theta.config')
--
--        -- Set custom header
--        dashboard.section.header.val = {
--            "",
--            "",
--            " ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗",
--            " ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║",
--            " ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║",
--            " ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║",
--            " ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║",
--            " ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝",
--            "",
--            " [ TIP: Welcome to Neovim! ] ",
--            "",
--        }
--
--        -- Set custom buttons
--        --dashboard.section.buttons.val = {
--        --    dashboard.button('r', '  Recent files', ':Telescope oldfiles<CR>'),
--        --    dashboard.button('n', '  New file', ':ene <BAR> startinsert <CR>'),
--        --    dashboard.button('q', '  Quit', ':qa<CR>'),
--        --}
--
--        -- Set custom footer
--        dashboard.section.footer.val = { "🎉 It's always good return to neovim, it's like return to home :) 🎉" }
--
--        -- Setup alpha with the dashboard theme
--        --alpha.setup(dashboard.opts)
--        alpha.setup(dashboard)
--    end,
--}


local coolLines = {
    [[    ███╗   ███╗ █████╗ ██╗  ██╗███████╗   ]],
    [[    ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝   ]],
    [[    ██╔████╔██║███████║█████╔╝ █████╗     ]],
    [[    ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝     ]],
    [[    ██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗   ]],
    [[    ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝   ]],
    [[      ██████╗ ██████╗  ██████╗ ██╗        ]],
    [[     ██╔════╝██╔═══██╗██╔═══██╗██║        ]],
    [[     ██║     ██║   ██║██║   ██║██║        ]],
    [[     ██║     ██║   ██║██║   ██║██║        ]],
    [[     ╚██████╗╚██████╔╝╚██████╔╝███████╗   ]],
    [[      ╚═════╝ ╚═════╝  ╚═════╝ ╚══════╝   ]],
    [[███████╗████████╗██╗   ██╗███████╗███████╗]],
    [[██╔════╝╚══██╔══╝██║   ██║██╔════╝██╔════╝]],
    [[███████╗   ██║   ██║   ██║█████╗  █████╗  ]],
    [[╚════██║   ██║   ██║   ██║██╔══╝  ██╔══╝  ]],
    [[███████║   ██║   ╚██████╔╝██║     ██║     ]],
    [[╚══════╝   ╚═╝    ╚═════╝ ╚═╝     ╚═╝     ]],
}

local solveLines = {
    [[╔═╗┬┬─┐┌─┐┌┬┐  ┌─┐┌─┐┬ ┬  ┬┌─┐  ┌┬┐┬ ┬┌─┐  ┌─┐┬─┐┌─┐┌┐ ┬  ┌─┐┌┬┐   ]],
    [[╠╣ │├┬┘└─┐ │   └─┐│ ││ └┐┌┘├┤    │ ├─┤├┤   ├─┘├┬┘│ │├┴┐│  ├┤ │││   ]],
    [[╚  ┴┴└─└─┘ ┴┘  └─┘└─┘┴─┘└┘ └─┘   ┴ ┴ ┴└─┘  ┴  ┴└─└─┘└─┘┴─┘└─┘┴ ┴ o ]],
    [[      ╔╦╗┬ ┬┌─┐┌┐┌  ┬ ┬┬─┐┬┌┬┐┌─┐  ┌┬┐┬ ┬┌─┐  ┌─┐┌─┐┌┬┐┌─┐         ]],
    [[       ║ ├─┤├┤ │││  │││├┬┘│ │ ├┤    │ ├─┤├┤   │  │ │ ││├┤          ]],
    [[       ╩ ┴ ┴└─┘┘└┘  └┴┘┴└─┴ ┴ └─┘   ┴ ┴ ┴└─┘  └─┘└─┘─┴┘└─┘         ]],
    [[                                                     - John Johnson]]
}

local humourLines = {
    [[╔═╗┌─┐┌┬┐┌─┐  ┬┌─┐  ┬  ┬┬┌─┌─┐  ┬ ┬┬ ┬┌┬┐┌─┐┬─┐                           ]],
    [[║  │ │ ││├┤   │└─┐  │  │├┴┐├┤   ├─┤│ │││││ │├┬┘                           ]],
    [[╚═╝└─┘─┴┘└─┘  ┴└─┘  ┴─┘┴┴ ┴└─┘  ┴ ┴└─┘┴ ┴└─┘┴└─o                          ]],
    [[╦ ╦┬ ┬┌─┐┌┐┌  ┬ ┬┌─┐┬ ┬  ┬ ┬┌─┐┬  ┬┌─┐  ┌┬┐┌─┐  ┌─┐─┐ ┬┌─┐┬  ┌─┐┬┌┐┌  ┬┌┬┐]],
    [[║║║├─┤├┤ │││  └┬┘│ ││ │  ├─┤├─┤└┐┌┘├┤    │ │ │  ├┤ ┌┴┬┘├─┘│  ├─┤││││  │ │ ]],
    [[╚╩╝┴ ┴└─┘┘└┘   ┴ └─┘└─┘  ┴ ┴┴ ┴ └┘ └─┘   ┴ └─┘  └─┘┴ └─┴  ┴─┘┴ ┴┴┘└┘  ┴ ┴┘]],
    [[                                                        ┬┌┬┐┌─┐  ┌┐ ┌─┐┌┬┐]],
    [[                                                        │ │ └─┐  ├┴┐├─┤ ││]],
    [[                                                        ┴ ┴ └─┘  └─┘┴ ┴─┴┘]],
    [[                                                              - Cory House]],
}

local bugLines = {
    [[  _____   _                          _                 _                             ]],
    [[ |_   _| | |_    ___   _ _   ___    (_)  ___    __ _  | | __ __ __  __ _   _  _   ___]],
    [[   | |   | ' \  / -_) | '_| / -_)   | | (_-<   / _` | | | \ V  V / / _` | | || | (_-<]],
    [[   |_|   |_||_| \___| |_|   \___|   |_| /__/   \__,_| |_|  \_/\_/  \__,_|  \_, | /__/]],
    [[                                                                           |__/      ]],
    [[                                                   _                                 ]],
    [[  ___   _ _    ___     _ __    ___   _ _   ___    | |__   _  _   __ _                ]],
    [[ / _ \ | ' \  / -_)   | '  \  / _ \ | '_| / -_)   | '_ \ | || | / _` |               ]],
    [[ \___/ |_||_| \___|   |_|_|_| \___/ |_|   \___|   |_.__/  \_,_| \__, |               ]],
    [[                                                                |___/                ]],
    [[  _              __   _                                                              ]],
    [[ | |_   ___     / _| (_) __ __                                                       ]],
    [[ |  _| / _ \   |  _| | | \ \ /  _                                                    ]],
    [[  \__| \___/   |_|   |_| /_\_\ (_)                                     - Ellen Ullman]],
}

local fixLines = {
    [[    ┌─┐┬─┐ ┬  ┌┬┐┬ ┬┌─┐  ┌─┐┌─┐┬ ┬┌─┐┌─┐      ]],
    [[    ├┤ │┌┴┬┘   │ ├─┤├┤   │  ├─┤│ │└─┐├┤       ]],
    [[    └  ┴┴ └─   ┴ ┴ ┴└─┘  └─┘┴ ┴└─┘└─┘└─┘┘     ]],
    [[╔╗╔╔═╗╔╦╗  ╔╦╗╦ ╦╔═╗  ╔═╗╦ ╦╔╦╗╔═╗╔╦╗╔═╗╔╦╗╔═╗]],
    [[║║║║ ║ ║    ║ ╠═╣║╣   ╚═╗╚╦╝║║║╠═╝ ║ ║ ║║║║╚═╗]],
    [[╝╚╝╚═╝ ╩    ╩ ╩ ╩╚═╝  ╚═╝ ╩ ╩ ╩╩   ╩ ╚═╝╩ ╩╚═╝]],
    [[                              - Steve Maguire ]],
}

local processLines = {
    [[╔╦╗╦ ╦╔═╗  ╔═╗╦═╗╔═╗╔═╗╔╦╗╦╦  ╦╔═╗  ╔═╗╦═╗╔═╗╔═╗╔═╗╔═╗╔═╗]],
    [[ ║ ╠═╣║╣   ║  ╠╦╝║╣ ╠═╣ ║ ║╚╗╔╝║╣   ╠═╝╠╦╝║ ║║  ║╣ ╚═╗╚═╗]],
    [[ ╩ ╩ ╩╚═╝  ╚═╝╩╚═╚═╝╩ ╩ ╩ ╩ ╚╝ ╚═╝  ╩  ╩╚═╚═╝╚═╝╚═╝╚═╝╚═╝]],
    [[This is Amazing!]],
    [[This is difficult]],
    [[This is shit]],
    [[I am shit]],
    [[This might be OK]],
    [[This is Amazing!]],
}

local neovim1 = {
    [[                                                                       ]],
    [[       ████ ██████           █████      ██                        ]],
    [[      ███████████             █████                                ]],
    [[      █████████ ███████████████████ ███   ███████████      ]],
    [[     █████████  ███    █████████████ █████ ██████████████      ]],
    [[    █████████ ██████████ █████████ █████ █████ ████ █████      ]],
    [[  ███████████ ███    ███ █████████ █████ █████ ████ █████     ]],
    [[ ██████  █████████████████████ ████ █████ █████ ████ ██████    ]]
}

local neovim2 = {
    [[███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗]],
    [[████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║]],
    [[██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║]],
    [[██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║]],
    [[██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║]],
    [[╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝]],
    [[                                [ Welcome to Neovim! ] ]]
}

--            "",
--            "",
--            " ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗",
--            " ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║",
--            " ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║",
--            " ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║",
--            " ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║",
--            " ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝",
--            "",
--            " [ TIP: Welcome to Neovim! ] ",
--            "",

local function lineColor(lines, popStart, popEnd)
    local out = {}
    for i, line in ipairs(lines) do
        local hi = "StartLogo" .. i
        if i > popStart and i <= popEnd then
            hi = "StartLogoPop" .. i - popStart
        elseif i > popStart then
            hi = "StartLogo" .. i - popStart
        else
            hi = "StartLogo" .. i
        end
        table.insert(out, { hi = hi, line = line})
    end
    return out
end

local headers = {
    lineColor(neovim2, 6, 9),
    --[[lineColor(coolLines, 6, 12),
    lineColor(solveLines, 0, 0),
    lineColor(humourLines, 6, 9),
    lineColor(bugLines, 5, 10),
    lineColor(processLines, 0, 3),
    lineColor(fixLines, 0, 0),]]
}

local function header_chars()
    math.randomseed(os.time())
    return headers[math.random(#headers)]
end

-- Map over the headers, setting a different color for each line.
-- This is done by setting the Highligh to StartLogoN, where N is the row index.
-- Define StartLogo1..StartLogoN to get a nice gradient.
local function header_color()
    local lines = {}
    for _, lineConfig in pairs(header_chars()) do
        local hi = lineConfig.hi
        local line_chars = lineConfig.line
        local line = {
            type = "text",
            val = line_chars,
            opts = {
                hl = hi,
                shrink_margin = false,
                position = "center",
            },
        }
        table.insert(lines, line)
    end

    local output = {
        type = "group",
        val = lines,
        opts = { position = "center", },
    }

    return output
end

local function configure()
    local theme = require("alpha.themes.theta")
    local themeconfig = theme.config
    local dashboard = require("alpha.themes.dashboard")
    local buttons = {
        type = "group",
        val = {
            { type = "text", val = "Quick links", opts = { hl = "SpecialComment", position = "center" } },
            { type = "padding", val = 1 },
            dashboard.button("e",     "  New file",        "<cmd>ene<CR>"),
            dashboard.button("SPC f", "  Find file",       ":Telescope find_files find_command=rg,--hidden,--files<CR>"),
            dashboard.button("SPC F", "  Find text",       ":Telescope live_grep<CR>"),
            dashboard.button("u",     "󱐥  Update plugins", "<cmd>Lazy sync<CR>"),
            dashboard.button("t",     "  Install language tools", "<cmd>Mason<CR>"),
            dashboard.button("q",     "󰩈  Quit", "<cmd>qa<CR>"),


            dashboard.button('r', '  Recent files', ':Telescope oldfiles<CR>'),
        },
        position = "center",
    }

    local footer = {
        type = "group",
        val = {
            { type = "padding", val = 2 },
            { type = "text", val = "🎉 It's always good to return to Neovim   It's like returning to home   🎉",
              opts = { hl = "SpecialComment", position = "center" } },
            { type = "padding", val = 2 },
        },
        position = "footer",
    }

    themeconfig.layout[2] = header_color()
    themeconfig.layout[6] = buttons
    themeconfig.layout[14] = footer

    return themeconfig
end

return {
    'goolord/alpha-nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function ()
        require'alpha'.setup(configure())
    end
};