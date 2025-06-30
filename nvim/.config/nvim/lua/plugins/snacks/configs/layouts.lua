local all_layouts = require("snacks.picker.config.layouts")

local M = {}

-- based on default
M.custom_explorer = {
    preview = false,
    cycle = false,
    reverse = false,
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
            -- title = "{title} 󱙓",
            title = "{title} 󰐰",
            title_pos = "center",
        },
        { win = "list", border = "none" },
        { win = "preview", title = "{preview}", height = 0.4, border = "top" },
    },
}

-- vertical telescope based style with bottom input
M.custom_vertical = {
    cycle = false,
    reverse = true,
    layout = {
        backdrop = false,
        width = 0.8,
        min_width = 80,
        height = 0.9,
        min_height = 30,
        box = "vertical",
        border = "none",
        -- border = "rounded",
        -- title = "{title} {live} {flags}",
        -- title_pos = "center",
        -- { win = "input", height = 1, border = "bottom" },
        -- { win = "list", border = "none" },
        -- { win = "preview", title = "{preview}", height = 0.4, border = "top" },
        {
            win = "preview",
            title = "{preview:Preview}",
            height = 0.4,
            border = "rounded",
            title_pos = "center",
        },
        {
            win = "list",
            title = " Results ",
            title_pos = "center",
            border = "rounded",
        },
        {
            -- border = "top",
            -- title = "{title} {live} {flags}",

            win = "input",
            height = 1,
            border = "rounded",
            -- title = "{title} {live} {flags}",
            title = "{title} 󰐰",
            title_pos = "center",
        },
    },
}

-- telescope based style
M.custom_horizontal = {
    cycle = false,
    reverse = true,
    layout = {
        box = "horizontal",
        backdrop = false,
        width = 0.8,
        height = 0.9,
        border = "none",
        {
            box = "vertical",
            {
                win = "list",
                title = " Results ",
                title_pos = "center",
                border = "rounded",
            },
            {
                win = "input",
                height = 1,
                border = "rounded",
                -- title = "{title} {live} {flags}",
                title = "{title} 󰐰",
                title_pos = "center",
            },
        },
        {
            win = "preview",
            title = "{preview:Preview}",
            width = 0.45,
            border = "rounded",
            title_pos = "center",
        },
    },
}

-- register presents
all_layouts.custom_explorer = M.custom_explorer
all_layouts.custom_horizontal = M.custom_horizontal
all_layouts.custom_vertical = M.custom_vertical

M.custom_default = {
    preview = true,
    cycle = true,
    reverse = true,
    preset = function()
        return vim.o.columns >= 120 and "custom_horizontal" or "custom_vertical"
    end,
}

return M