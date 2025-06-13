local M = {}

M.default = {
    enabled = true,
    -- width = 80,
    -- https://patorjk.com/software/taag/#p=display&f=ANSI%20Shadow&t=NEOVIM
    preset = {
        header = [[                 

                          

███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗
████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║
██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║
██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║
██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║
╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝

                          

        ]],
    },
    sections = {
        { section = "header", padding = { 0, 0 } },
        { icon = " ", title = "Keymaps: ", section = "keys", indent = 3, padding = { 1, 0 } },
        {
            icon = " ",
            title = "Recent Files: ",
            section = "recent_files",
            limit = 20,
            cwd = true,
            indent = 3,
            padding = { 1, 0 },
        },
        -- { icon = " ", title = "Projects: ", section = "projects", indent = 3, padding = { 1, 0 } },
        -- { section = "terminal", cmd = "fortune -s | cowsay", hl = "header", padding = 1, indent = 8 },
        -- { section = "terminal", cmd = "fortune -s", ttl = 0, hl = "header", padding = 1, indent = 8 },
        -- {
        --     section = "terminal",
        --     cmd = "bash ~/tools/colorscripts/ColorScripts/alpha",
        --     -- cmd = "fastfetch",
        --     hl = "header",
        --     height = 5,
        --     -- indent = 1,
        -- },
        -- { section = "startup", padding = { 0, 5 } },
    },
    formats = {
        key = function(item)
            return { { "[", hl = "special" }, { item.key, hl = "key" }, { "]", hl = "special" } }
        end,
    },
}
-- invaders
-- pacman
-- rally-x

return M
