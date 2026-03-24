local M = {}

Snacks.util.set_hl({
    PathHidden = "",
}, { prefix = "SnacksPicker", default = true })

local layouts = require("plugins.snacks.configs.layouts")

---@param item snacks.picker.Item
---@return string?
local function resolve_path(item)
    local file = item.file
    if not file then
        return nil
    end
    if item.cwd and not vim.startswith(file, "/") then
        file = item.cwd .. "/" .. file
    end
    return vim.fs.normalize(file)
end

---Diff two selected files in a new tab.
---Use <Tab> to multi-select exactly 2 files, then trigger this action.
---@param picker snacks.Picker
local function diff_selected(picker)
    local selected = picker:selected({ fallback = false })
    if #selected ~= 2 then
        local msg = #selected < 2 and "Select exactly 2 files to diff (use <Tab> to select)"
            or ("Select exactly 2 files to diff (got " .. #selected .. ")")
        vim.notify(msg, vim.log.levels.WARN)
        return
    end
    local file1 = resolve_path(selected[1])
    local file2 = resolve_path(selected[2])
    if not file1 or not file2 then
        vim.notify("Could not resolve file paths", vim.log.levels.ERROR)
        return
    end
    picker:close()
    vim.schedule(function()
        vim.cmd("tabnew " .. vim.fn.fnameescape(file1))
        vim.cmd("diffthis")
        vim.cmd("vsplit " .. vim.fn.fnameescape(file2))
        vim.cmd("diffthis")
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
            truncate = 250, -- truncate the file path to (roughly) this length
        },
    },
    -- matcher = {
    --     smartcase = false, -- use smartcase
    --     -- ignorecase = true, -- use ignorecase
    -- },
    actions = {
        diff_selected = diff_selected,
    },
    win = {
        input = {
            keys = {
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
                ["<c-y>"] = { "diff_selected", mode = { "i", "n" } },
            },
        },
        list = {
            keys = {
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
                ["<c-y>"] = { "diff_selected", mode = { "i", "n" } },
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
        select = {
            layout = {
                preset = "select",
                reverse = false,
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
}

return M
