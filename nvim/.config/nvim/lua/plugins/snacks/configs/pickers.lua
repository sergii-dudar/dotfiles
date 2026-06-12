local M = {}

Snacks.util.set_hl({
    PathHidden = "",
}, { prefix = "SnacksPicker", default = true })

local layouts = require("plugins.snacks.configs.layouts")
local layout_vertical = { layout = layouts.custom_vertical }

---Toggle between horizontal and vertical layouts.
---@param picker snacks.Picker
local function toggle_layout(picker)
    local current = picker._layout_toggle_name or "custom_horizontal"
    local next_name = current == "custom_horizontal" and "custom_vertical" or "custom_horizontal"
    picker._layout_toggle_name = next_name
    picker:set_layout(Snacks.picker.config.layout({ layout = next_name }))
end

---Close the active picker and open another one.
---@param picker snacks.Picker
---@param open fun()
local function switch_picker(picker, open)
    picker:norm(function()
        picker:close()
        vim.schedule(open)
    end)
end

---@param picker snacks.Picker
---@return number
local function picker_current_buf(picker)
    local filter_buf = picker.input and picker.input.filter and picker.input.filter.current_buf
    if filter_buf and vim.api.nvim_buf_is_valid(filter_buf) then
        return filter_buf
    end

    local ok, main_buf = pcall(vim.api.nvim_win_get_buf, picker.main)
    if ok and vim.api.nvim_buf_is_valid(main_buf) then
        return main_buf
    end

    return vim.api.nvim_get_current_buf()
end

---@param picker snacks.Picker
---@return boolean
local function has_buffer_picker_results(picker)
    local current_buf = picker_current_buf(picker)
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        local keep = vim.bo[buf].buflisted and buf ~= current_buf and vim.bo[buf].buftype ~= "nofile"
        if keep then
            return true
        end
    end
    return false
end

local picker_switch_order = { "files", "recent", "buffers", "grep" }
local picker_switch_open = {
    files = function()
        LazyVim.pick.open("files", { root = false })
    end,
    recent = function()
        Snacks.picker.recent({ filter = { cwd = true } })
    end,
    buffers = function()
        Snacks.picker.buffers()
    end,
    grep = function()
        LazyVim.pick.open("live_grep", { root = false })
    end,
}
local picker_switch_has_results = {
    buffers = has_buffer_picker_results,
}

---@param picker snacks.Picker
---@param source string
local function switch_to_picker(picker, source)
    switch_picker(picker, picker_switch_open[source])
end

---@param picker snacks.Picker
---@param source string
---@return boolean
local function can_switch_to_picker(picker, source)
    local has_results = picker_switch_has_results[source]
    return not has_results or has_results(picker)
end

---@param picker snacks.Picker
---@param step integer
local function rotate_picker(picker, step)
    local current = picker.opts.source
    local current_index = step > 0 and 0 or #picker_switch_order + 1
    for index, source in ipairs(picker_switch_order) do
        if source == current then
            current_index = index
            break
        end
    end

    for offset = 1, #picker_switch_order do
        local next_index = ((current_index - 1 + step * offset) % #picker_switch_order) + 1
        local source = picker_switch_order[next_index]
        if can_switch_to_picker(picker, source) then
            switch_to_picker(picker, source)
            return
        end
    end
end

---@param picker snacks.Picker
local function switch_to_files(picker)
    switch_to_picker(picker, "files")
end

---@param picker snacks.Picker
local function switch_to_buffers(picker)
    switch_to_picker(picker, "buffers")
end

---@param picker snacks.Picker
local function switch_to_grep(picker)
    switch_to_picker(picker, "grep")
end

---@param picker snacks.Picker
local function switch_to_recent(picker)
    switch_to_picker(picker, "recent")
end

---@param picker snacks.Picker
local function switch_to_next_picker(picker)
    rotate_picker(picker, 1)
end

---@param picker snacks.Picker
local function switch_to_prev_picker(picker)
    rotate_picker(picker, -1)
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

-- Picker excludes grouped by project type. The active project's language list is
-- appended to the common base (see project_excludes); add a language = add a table.
local exclude_common = {
    ".git",
    ".idea",
}

local exclude_java = {
    -- "**/target/classes",
    -- "**/target/test-classes",
    "**/target/maven-*",
    "**/target/surefire-reports",
    "**/target/failsafe-reports",
    "**/target/junit-report",
    "**/build/junit-report",
    "**/target/jacoco-output",
    "**/target/site",
    "**/target/.cache",
    "**/target/*.jar",
    "**/target/*.war",
    "**/target/*.ear",
    -- "**/bin",
    ".settings",
    ".mvn",
    "**/.classpath",
    "**/.factorypath",
    "**/.project",
}

local exclude_rust = {
    "**/target/debug",
    "**/target/release",
    "**/target/nextest",
    "**/target/doc",
}

local exclude_by_lang = {
    java = exclude_java,
    rust = exclude_rust,
}

-- Common excludes + the active project language's excludes.
local function project_excludes()
    local list = vim.deepcopy(exclude_common)
    local lang = require("utils.lang.lang-project").current()
    if lang and exclude_by_lang[lang] then
        vim.list_extend(list, exclude_by_lang[lang])
    end
    return list
end

M.picker = {
    -- Do NOT sejkt a fully-formed layout here. Snacks deep-merges the global layout into every
    -- source, so a global with a `.layout` array leaks into sources that rely on `preset`,
    -- causing `if not (layout.layout and layout.layout[1])` in snacks/picker/config/init.lua
    -- to skip preset resolution. Each source below sets its own layout explicitly.
    hidden = true, -- Include hidden files in grep
    ignored = false, -- Exclude git-ignored files
    exclude = project_excludes(),
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
        switch_to_files = switch_to_files,
        switch_to_buffers = switch_to_buffers,
        switch_to_grep = switch_to_grep,
        switch_to_recent = switch_to_recent,
        switch_to_next_picker = switch_to_next_picker,
        switch_to_prev_picker = switch_to_prev_picker,
    },
    win = {
        input = {
            keys = {
                ["<c-,>"] = { "switch_to_files", mode = { "i", "n" }, desc = "Switch to files" },
                ["<c-.>"] = { "switch_to_buffers", mode = { "i", "n" }, desc = "Switch to buffers" },
                ["<c-/>"] = { "switch_to_grep", mode = { "i", "n" }, desc = "Switch to grep" },
                ["<c-_>"] = { "switch_to_grep", mode = { "i", "n" }, desc = "Switch to grep" },
                ["<c-o>"] = { "switch_to_recent", mode = { "i", "n" }, desc = "Switch to recent" },
                ["<c-]>"] = { "switch_to_next_picker", mode = { "i", "n" }, desc = "Switch to next picker" },
                ["<c-e>"] = { "switch_to_prev_picker", mode = { "i", "n" }, desc = "Switch to previous picker" },
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
                ["<c-d>"] = { "diff_selected", mode = { "i", "n" } },
                ["<c-y>"] = { "toggle_layout", mode = { "i", "n" } },
                -- <c-[> is not mapped because Neovim receives it as <Esc>.
                -- remap
                -- ["<a-h>"] = "toggle_hidden",
                -- ["<a-i>"] = "toggle_ignored",
                -- ["<a-m>"] = "toggle_maximize",
                -- ["<a-p>"] = "toggle_preview",
            },
        },
        list = {
            keys = {
                ["<c-,>"] = { "switch_to_files", mode = { "n" }, desc = "Switch to files" },
                ["<c-.>"] = { "switch_to_buffers", mode = { "n" }, desc = "Switch to buffers" },
                ["<c-/>"] = { "switch_to_grep", mode = { "n" }, desc = "Switch to grep" },
                ["<c-_>"] = { "switch_to_grep", mode = { "n" }, desc = "Switch to grep" },
                ["<c-o>"] = { "switch_to_recent", mode = { "n" }, desc = "Switch to recent" },
                ["<c-]>"] = { "switch_to_next_picker", mode = { "n" }, desc = "Switch to next picker" },
                ["<c-e>"] = { "switch_to_prev_picker", mode = { "n" }, desc = "Switch to previous picker" },
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
            ignored = false, -- Exclude git-ignored files
            layout = layouts.custom_vertical,
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
            ignored = false, -- Exclude git-ignored files
            layout = layouts.custom_vertical,
            -- args = {
            --     "--ignore-case",
            -- },
        },
        grep_buffers = layout_vertical,
        grep_word = layout_vertical,
        resume = layout_vertical,
        undo = layout_vertical,
        recent = layout_vertical,
        smart = {
            layout = layouts.custom_vertical,
            -- multi = { "buffers", "recent", "files" },
        },
        git_files = layout_vertical,
        git_branches = layout_vertical,
        git_log = layout_vertical,
        git_log_line = layout_vertical,
        git_status = layout_vertical,
        git_stash = layout_vertical,
        git_diff = layout_vertical,
        git_log_file = layout_vertical,
        lsp_declarations = layout_vertical,
        lsp_definitions = layout_vertical,
        lsp_implementations = layout_vertical,
        lsp_incoming_calls = layout_vertical,
        lsp_outgoing_calls = layout_vertical,
        lsp_references = layout_vertical,
        lsp_symbols = layout_vertical,
        lsp_type_definitions = layout_vertical,
        lsp_workspace_symbols = layout_vertical,
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
