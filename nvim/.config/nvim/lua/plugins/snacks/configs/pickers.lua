local M = {}

Snacks.util.set_hl({
    PathHidden = "",
}, { prefix = "SnacksPicker", default = true })

local layouts = require("plugins.snacks.configs.layouts")

M.picker = {
    -- layout = {
    --     cycle = false,
    -- },
    layout = layouts.custom_default,
    hidden = true, -- Include hidden files in grep
    ignored = true, -- Exclude git-ignored files
    formatters = {
        file = {
            filename_first = true, -- display filename before the file path
            filename_only = false, -- only show the filename
            truncate = 250, -- truncate the file path to (roughly) this length
        },
    },
    sources = {
        explorer = {
            -- layout = layouts.custom_explorer,
            layout = layouts.custom_default,
            auto_close = false,
            focus = "input", -- input|list
        },
        files = {
            -- cmd = "fd", -- "fd"| "rg"| "find" command to use. Leave empty to auto-detect
            cmd = "fd",
            hidden = true, -- Show hidden files
            ignored = true, -- Exclude git-ignored files
            -- exclude = { "node_modules/*", "*.pyc", "*.log" }, -- Exclude patterns
            -- preview = false, -- Enable file preview in picker
        },
        grep = {
            hidden = true, -- Show hidden files
            ignored = true, -- Exclude git-ignored files
        },
    },
    toggles = {
        hidden = "󱞞",
        ignored = "",
        modified = "",
    },
}
M.explorer = {
    hidden = true, -- Show hidden files
    ignored = true, -- Show git-ignored files
    replace_netrw = true,
    auto_close = false, -- Keep explorer open
}

return M
