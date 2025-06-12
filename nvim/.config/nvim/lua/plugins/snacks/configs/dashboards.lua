local M = {}

M.default = {
    enabled = true,
    preset = {
        header = [[
███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗
████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║
██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║
██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║
██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║
╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝
[ Welcome to The Best Editor in The Universe! ]
        ]],
    },
    sections = {
        { section = "header", padding = { 0, 0 } },
        { icon = " ", title = "Keymaps: ", section = "keys", indent = 3, padding = { 1, 0 } },
        { icon = " ", title = "Recent Files: ", section = "recent_files", indent = 3, padding = { 1, 0 } },
        { icon = " ", title = "Projects: ", section = "projects", indent = 3, padding = { 1, 0 } },
        -- { section = "terminal", cmd = "fortune -s | cowsay", hl = "header", padding = 1, indent = 8 },
        -- { section = "terminal", cmd = "fortune -s", ttl = 0, hl = "header", padding = 1, indent = 8 },
        -- { section = "startup", padding = { 0, 5 } },
    },
}

return M
