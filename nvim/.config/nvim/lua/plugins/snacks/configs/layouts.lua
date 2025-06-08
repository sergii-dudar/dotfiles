local M = {}

M.explorer = {
    preview = false,
    cycle = false,
    layout = {
        backdrop = false,
        width = 40,
        min_width = 40,
        height = 0,
        position = "left",
        border = "none",
        box = "vertical",
        {
            win = "input",
            height = 1,
            border = "rounded",
            -- title = "{title} {live} {flags}",
            title = "{title} 󱙓",
            title_pos = "center",
        },
        { win = "list", border = "none" },
        { win = "preview", title = "{preview}", height = 0.4, border = "top" },
    },
}

return M