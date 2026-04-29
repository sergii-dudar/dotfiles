local M = {}

Snacks.util.set_hl({
    PathHidden = "",
}, { prefix = "SnacksPicker", default = true })

local layouts = require("plugins.snacks.configs.layouts")

---Toggle between horizontal and vertical layouts.
---@param picker snacks.Picker
local function toggle_layout(picker)
    local current = picker._layout_toggle_name or "custom_horizontal"
    local next_name = current == "custom_horizontal" and "custom_vertical" or "custom_horizontal"
    picker._layout_toggle_name = next_name
    picker:set_layout(Snacks.picker.config.layout({ layout = next_name }))
end

---Grep within selected files from the files picker.
---Use <Tab> to multi-select files, then trigger this action.
---Falls back to the current item if nothing is multi-selected.
---@param picker snacks.Picker
local function grep_selected_files(picker)
    local selected = picker:selected({ fallback = true })
    if not selected or #selected == 0 then
        vim.notify("No files selected", vim.log.levels.WARN)
        return
    end
    local files = {}
    for _, item in ipairs(selected) do
        if item.file then
            local path = item.cwd and (item.cwd .. "/" .. item.file) or item.file
            table.insert(files, path)
        end
    end
    if #files == 0 then
        vim.notify("No file paths found in selection", vim.log.levels.WARN)
        return
    end
    picker:close()
    vim.schedule(function()
        Snacks.picker.grep({ dirs = files, title = "Search in selected" })
    end)
end

---Diff two selected files in a new tab.
---Use <Tab> to multi-select exactly 2 files, then trigger this action.
---@param picker snacks.Picker
local function diff_selected(picker)
    local selected = picker:selected({ fallback = false })
    picker:close()
    vim.schedule(function()
        require("utils.diff-util").diff_selected(selected)
    end)
end

M.picker = {
    -- layout = {
    --     cycle = false,
    -- },
    layout = layouts.custom_default,
    hidden = true, -- Include hidden files in grep
    ignored = true, -- Exclude git-ignored files
    exclude = {
        "**/target/classes",
        "**/target/test-classes",
        "**/target/maven-*",
        "**/target/surefire-reports",
        "**/target/failsafe-reports",
        "**/target/junit-report",
        "**/target/jacoco-output",
        "**/target/site",
        "**/target/.cache",
        "**/target/*.jar",
        "**/target/*.war",
        "**/target/*.ear",
        "**/bin",
        ".git",
        ".settings",
        ".idea",
        ".mvn",
        "**/.classpath",
        "**/.factorypath",
        "**/.project",
    },
    formatters = {
        file = {
            filename_first = true, -- display filename before the file path
            filename_only = false, -- only show the filename
            min_width = 150, -- truncate the file path to (roughly) this length
            truncate = "left", -- "left"|"center"|"right"
        },
    },
    previewers = {
        diff = {
            wo = {
                wrap = false,
            },
        },
    },
    -- matcher = {
    --     smartcase = false, -- use smartcase
    --     -- ignorecase = true, -- use ignorecase
    -- },
    actions = {
        diff_selected = diff_selected,
        toggle_layout = toggle_layout,
        grep_selected_files = grep_selected_files,
    },
    win = {
        input = {
            keys = {
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
                ["<c-d>"] = { "diff_selected", mode = { "i", "n" } },
                ["<c-y>"] = { "toggle_layout", mode = { "i", "n" } },
            },
        },
        list = {
            keys = {
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
                ["<c-d>"] = { "diff_selected", mode = { "i", "n" } },
                ["<c-y>"] = { "toggle_layout", mode = { "n" } },
            },
        },
    },
    sources = {
        explorer = {
            -- layout = layouts.custom_explorer,
            layout = vim.tbl_extend("force", layouts.custom_default, { reverse = false }),
            auto_close = true,
            focus = "input", -- input|list
            win = {
                input = {
                    keys = {
                        ["<c-g>"] = { "grep_selected_files", mode = { "i", "n" } },
                    },
                },
                list = {
                    keys = {
                        ["<c-g>"] = { "grep_selected_files", mode = { "i", "n" } },
                    },
                },
            },
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
            --     "--full-path"
            -- },
            win = {
                input = {
                    keys = {
                        ["<c-g>"] = { "grep_selected_files", mode = { "i", "n" } },
                    },
                },
                list = {
                    keys = {
                        ["<c-g>"] = { "grep_selected_files", mode = { "i", "n" } },
                    },
                },
            },
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
        select = {
            layout = {
                preset = "select",
                reverse = false,
            },
        },
        buffers = {
            current = false,
            -- layout = layouts.custom_vertical,
            layout = layouts.custom_buffers,
            -- layout = {
            --     preset = "custom_vertical",
            --     layout = {
            --         width = 0.5,
            --         height = 0.6,
            --     },
            -- },
            win = {
                input = {
                    keys = {
                        ["ii"] = { "confirm", mode = { "n", "i" } },
                    },
                },
                list = { keys = { ["ii"] = "confirm" } },
            },
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
    reverse = false,
}

return M
