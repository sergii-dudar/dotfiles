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

---Get the content of the enclosing string literal at cursor via treesitter.
---@return { text: string, start_row: integer, start_col: integer, end_row: integer, end_col: integer }|nil
local function get_string_at_cursor()
    local ok, node = pcall(vim.treesitter.get_node)
    if not ok or not node then
        return nil
    end
    while node do
        local ntype = node:type()
        if ntype:match("string") and not ntype:match("string_interpolation") then
            local sr, sc, er, ec = node:range()
            local text = vim.treesitter.get_node_text(node, 0)
            -- Strip surrounding quotes
            if text:match('^["\']') and #text >= 2 then
                text = text:sub(2, -2)
                sc = sc + 1
                ec = ec - 1
            end
            return { text = text, start_row = sr, start_col = sc, end_row = er, end_col = ec }
        end
        node = node:parent()
    end
    return nil
end

---Get the visual selection text and range.
---@return { text: string, start_row: integer, start_col: integer, end_row: integer, end_col: integer }|nil
local function get_visual_selection()
    local s = vim.fn.getpos("'<")
    local e = vim.fn.getpos("'>")
    local sr, sc = s[2] - 1, s[3] - 1
    local er, ec = e[2] - 1, e[3]
    local end_line = vim.api.nvim_buf_get_lines(0, er, er + 1, false)[1] or ""
    ec = math.min(ec, #end_line)
    local lines = vim.api.nvim_buf_get_text(0, sr, sc, er, ec, {})
    local text = table.concat(lines, "\n")
    if text == "" then
        return nil
    end
    return { text = text, start_row = sr, start_col = sc, end_row = er, end_col = ec }
end

---Open a file picker scoped to context-aware directories.
---For Java: scoped to module resource dirs. For other registered types: their resolver.
---Fallback: project CWD. Enter pastes the relative path at cursor.
---@param opts? { mode: "n"|"v" }
function M.pick_resource_path(opts)
    opts = opts or {}
    local uv = vim.uv or vim.loop
    local resolver = require("utils.resource-cwd-resolver")
    local result = resolver.resolve()

    local base_dirs, title
    if result then
        base_dirs = result.dirs
        title = result.title
    else
        local cwd = vim.fn.getcwd()
        base_dirs = { cwd }
        title = "Files [" .. vim.fn.fnamemodify(cwd, ":t") .. "]"
    end

    -- Get partial path context based on invocation mode
    local context = nil
    if opts.mode == "v" then
        context = get_visual_selection()
    elseif opts.mode == "n" then
        context = get_string_at_cursor()
    end

    -- Split partial path into directory prefix for narrowing
    local dir_prefix = ""
    local resource_dirs = base_dirs
    local dirs_narrowed = false
    local replace_range = context

    if context then
        local last_slash = context.text:match(".*()/" )
        if last_slash then
            dir_prefix = context.text:sub(1, last_slash)
            local narrowed = vim.tbl_filter(function(d)
                return uv.fs_stat(d) ~= nil
            end, vim.tbl_map(function(d)
                return vim.fs.normalize(d .. "/" .. dir_prefix)
            end, base_dirs))
            if #narrowed > 0 then
                resource_dirs = narrowed
                dirs_narrowed = true
            end
        end
        title = title .. " > " .. context.text
    end

    local function make_rel_path(abs_path)
        local rel = relativize_to_dirs(abs_path, resource_dirs)
        if dirs_narrowed then
            rel = dir_prefix .. rel
        end
        return rel
    end

    local function copy_res_path(picker, item)
        local abs_path = item and Snacks.picker.util.path(item)
        if abs_path then
            local rel = make_rel_path(abs_path)
            vim.fn.setreg("+", rel)
            Snacks.notify("Copied to clipboard:\n" .. rel, { title = "Snacks Picker" })
        end
    end

    local function paste_res_path(picker, item)
        local abs_path = item and Snacks.picker.util.path(item)
        if abs_path then
            local rel = make_rel_path(abs_path)
            picker:close()
            vim.schedule(function()
                if replace_range then
                    vim.api.nvim_buf_set_text(
                        0,
                        replace_range.start_row,
                        replace_range.start_col,
                        replace_range.end_row,
                        replace_range.end_col,
                        { rel }
                    )
                else
                    vim.api.nvim_paste(rel, true, -1)
                end
            end)
        end
    end

    Snacks.picker.files({
        dirs = resource_dirs,
        title = title,
        confirm = "paste_resource_path",
        actions = {
            copy_resource_path = copy_res_path,
            paste_resource_path = paste_res_path,
        },
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
