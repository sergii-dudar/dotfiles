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
    exclude = {
        "target/classes",
        "target/test-classes",
        "bin",
        ".git",
        ".idea",
    },
    formatters = {
        file = {
            filename_first = true, -- display filename before the file path
            filename_only = false, -- only show the filename
            truncate = 250, -- truncate the file path to (roughly) this length
        },
    },
    -- matcher = {
    --     smartcase = false, -- use smartcase
    --     -- ignorecase = true, -- use ignorecase
    -- },
    win = {
        input = {
            keys = {
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
            },
        },
        list = {
            keys = {
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
            },
        },
    },
    sources = {
        explorer = {
            -- layout = layouts.custom_explorer,
            layout = layouts.custom_default,
            auto_close = true,
            focus = "input", -- input|list
        },
        files = {
            -- cmd = "fd", -- "fd"| "rg"| "find" command to use. Leave empty to auto-detect
            cmd = "fd",
            hidden = true, -- Show hidden files
            ignored = true, -- Exclude git-ignored files
            -- exclude = { "node_modules/*", "*.pyc", "*.log" }, -- Exclude patterns
            -- preview = false, -- Enable file preview in picker
            -- args = {
            --     "--ignore-case",
            -- },
        },
        grep = {
            hidden = true, -- Show hidden files
            ignored = true, -- Exclude git-ignored files
            -- args = {
            --     "--ignore-case",
            -- },
        },
        projects = {
            dev = {
                "~/serhii.home/work/git.work",
                "~/serhii.home/work/git.infra",
                "~/serhii.home/work/git.java.base/",
            },
            patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "package.json", "Makefile" },
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