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

---Copy the selected item's file path to the system clipboard.
---@param picker snacks.Picker
---@param item snacks.picker.Item?
local function copy_path(picker, item)
    local path = item and Snacks.picker.util.path(item) or picker:dir()
    if path then
        path = vim.fn.fnamemodify(path, ":p:~:.")
        vim.fn.setreg("+", path)
        Snacks.notify("Copied to clipboard:\n" .. path, { title = "Snacks Picker" })
    end
end

---Insert the selected item's file path at the current cursor position.
---@param picker snacks.Picker
---@param item snacks.picker.Item?
local function paste_path(picker, item)
    local path = item and Snacks.picker.util.path(item) or picker:dir()
    if path then
        path = vim.fn.fnamemodify(path, ":p:~:.")
        picker:close()
        vim.schedule(function()
            vim.api.nvim_paste(path, true, -1)
        end)
    end
end

---Relativize an absolute path against a list of base directories.
---Returns the shortest relative match, or the original path if no match.
---@param abs_path string
---@param base_dirs string[]
---@return string
local function relativize_to_dirs(abs_path, base_dirs)
    abs_path = vim.fs.normalize(abs_path)
    for _, dir in ipairs(base_dirs) do
        dir = vim.fs.normalize(dir)
        if not dir:match("/$") then
            dir = dir .. "/"
        end
        if abs_path:sub(1, #dir) == dir then
            return abs_path:sub(#dir + 1)
        end
    end
    return abs_path
end

---Open a file picker scoped to context-aware resource directories.
---For Java modules: scoped to src/main/resources (and src/test/resources for test files).
---For everything else: falls back to default file picker from CWD.
function M.pick_resource_path()
    local resolver = require("utils.resource-cwd-resolver")
    local result = resolver.resolve()

    if result then
        local resource_dirs = result.dirs

        local function copy_res_path(picker, item)
            local abs_path = item and Snacks.picker.util.path(item)
            if abs_path then
                local rel = relativize_to_dirs(abs_path, resource_dirs)
                vim.fn.setreg("+", rel)
                Snacks.notify("Copied to clipboard:\n" .. rel, { title = "Snacks Picker" })
            end
        end

        local function paste_res_path(picker, item)
            local abs_path = item and Snacks.picker.util.path(item)
            if abs_path then
                local rel = relativize_to_dirs(abs_path, resource_dirs)
                picker:close()
                vim.schedule(function()
                    vim.api.nvim_paste(rel, true, -1)
                end)
            end
        end

        Snacks.picker.files({
            dirs = resource_dirs,
            title = result.title,
            actions = {
                copy_resource_path = copy_res_path,
                paste_resource_path = paste_res_path,
            },
            confirm = "paste_resource_path",
            win = {
                input = {
                    keys = {
                        ["<c-s>y"] = { "copy_resource_path", mode = { "i", "n" } },
                        ["<c-s>p"] = { "paste_resource_path", mode = { "i", "n" } },
                    },
                },
                list = {
                    keys = {
                        ["<c-s>y"] = { "copy_resource_path", mode = { "i", "n" } },
                        ["<c-s>p"] = { "paste_resource_path", mode = { "i", "n" } },
                    },
                },
            },
        })
    else
        Snacks.picker.files()
    end
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
    -- matcher = {
    --     smartcase = false, -- use smartcase
    --     -- ignorecase = true, -- use ignorecase
    -- },
    actions = {
        diff_selected = diff_selected,
        toggle_layout = toggle_layout,
        copy_path = copy_path,
        paste_path = paste_path,
    },
    win = {
        input = {
            keys = {
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
                ["<c-d>"] = { "diff_selected", mode = { "i", "n" } },
                ["<c-y>"] = { "toggle_layout", mode = { "i", "n" } },
                ["<c-s>y"] = { "copy_path", mode = { "i", "n" } },
                ["<c-s>p"] = { "paste_path", mode = { "i", "n" } },
            },
        },
        list = {
            keys = {
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
                ["<c-d>"] = { "diff_selected", mode = { "i", "n" } },
                ["<c-y>"] = { "toggle_layout", mode = { "n" } },
                ["<c-s>y"] = { "copy_path", mode = { "i", "n" } },
                ["<c-s>p"] = { "paste_path", mode = { "i", "n" } },
            },
        },
    },
    sources = {
        explorer = {
            -- layout = layouts.custom_explorer,
            layout = vim.tbl_extend("force", layouts.custom_default, { reverse = false }),
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
            --     "--full-path"
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
