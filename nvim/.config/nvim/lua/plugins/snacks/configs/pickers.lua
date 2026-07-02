local M = {}

Snacks.util.set_hl({
    PathHidden = "",
}, { prefix = "SnacksPicker", default = true })

local layouts = require("plugins.snacks.configs.layouts")
local layout_vertical = { layout = layouts.custom_vertical }
local picker_util = require("utils.snacks-pickers-util")

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

-- Picker excludes grouped by project type. The active project's language list is
-- appended to the common base (see project_excludes); add a language = add a table.
local exclude_common = {
    ".git",
    ".idea",
}

local exclude_java = {
    -- maven
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
    ".mvn",

    -- gradle (uncomment for gradle)
    -- "**/bin",
    -- "**/build",
    -- "**/gradle",
    -- "gradlew",
    -- "gradlew.bat",
    -- ".gradle",

    -- jdtls
    "**/.classpath",
    "**/.factorypath",
    "**/.project",
    ".settings",
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

---Build common excludes plus active project language-specific excludes.
local function project_excludes()
    local list = vim.deepcopy(exclude_common)
    local lang = require("utils.lang.lang-project").current()
    if lang and exclude_by_lang[lang] then
        vim.list_extend(list, exclude_by_lang[lang])
    end
    return list
end

---Toggle the picker's `exclude` patterns on/off, then re-run the find.
---Unlike `toggle_ignored` (which only flips the git-ignore `--no-ignore` flag),
---this clears/restores `opts.exclude` (the hard `fd -E` patterns from
---`project_excludes()`), so excluded dirs like `target/`, `bin/`, `.idea`
---become searchable.
---@param picker snacks.Picker
local function toggle_exclude(picker)
    if picker.opts.exclude and #picker.opts.exclude > 0 then
        picker.opts._saved_exclude = picker.opts.exclude
        picker.opts.exclude = {}
    else
        picker.opts.exclude = picker.opts._saved_exclude or project_excludes()
    end
    picker.list:set_target()
    picker:find()
end

-- Decompile-and-preview script: runs Fernflower and prints bat-highlighted Java
-- to stdout. Previewed in terminal mode (no ft) so bat's ANSI colors render.
local decompile_script = _G.global.dotfiles_path("scripts/java/decompile_stdout.sh")

---Default previewer that decompiles `.class` files via Fernflower instead of
---showing Snacks' "binary file" warning. Everything else falls through to the
---built-in file previewer.
---@param ctx snacks.picker.preview.ctx
local function file_preview(ctx)
    -- `jdt://` URIs come from LSP pickers (references/definitions/…) that land in
    -- library/JDK classes. They aren't files on disk, so hand them straight to the
    -- built-in file previewer: it bufadd/bufloads the URI and jdtls' BufReadCmd
    -- resolves the source via `java/classFileContents`. Check the RAW item.file —
    -- `util.path` normalizes `jdt://` down to `jdt:/`, which would break matching.
    local file = ctx.item and ctx.item.file
    if type(file) == "string" and file:sub(1, 6) == "jdt://" then
        return Snacks.picker.preview.file(ctx)
    end

    local path = Snacks.picker.util.path(ctx.item)
    if path and path:sub(-6) == ".class" then
        -- No `ft` => runs in a terminal/pty preview, which renders bat's ANSI.
        return Snacks.picker.preview.cmd({ decompile_script, path }, ctx)
    end
    return Snacks.picker.preview.file(ctx)
end

---Show the current selection's position in the picker (input) title, refreshed as
---the cursor moves. Snacks' built-in `x/y` readout (right-aligned in the input line)
---is matched/total and does NOT track the cursor; this appends `[row/matched]` to the
---title via the supported `on_change` hook + `update_titles`, so no Snacks internals
---are patched (survives upgrades). Visible where the layout title has a `{title}`
---placeholder (e.g. `custom_vertical`'s input box).
---@param picker snacks.Picker
local function show_cursor_position(picker)
    -- Cache the source's own title once, then rebuild from it on every move.
    local base = picker._pos_base_title
    if base == nil then
        base = picker.title or ""
        picker._pos_base_title = base
    end

    local matched = picker.list:count()
    local parts = {}
    if base ~= "" then
        parts[#parts + 1] = base
    end
    if matched > 0 then
        parts[#parts + 1] = ("[%d/%d]"):format(picker.list.cursor or 0, matched)
    end
    local title = table.concat(parts, " ")

    if picker.title ~= title then
        picker.title = title
        picker:update_titles()
    end
end

M.picker = {
    -- Do NOT sejkt a fully-formed layout here. Snacks deep-merges the global layout into every
    -- source, so a global with a `.layout` array leaks into sources that rely on `preset`,
    -- causing `if not (layout.layout and layout.layout[1])` in snacks/picker/config/init.lua
    -- to skip preset resolution. Each source below sets its own layout explicitly.
    hidden = true, -- Include hidden files in grep
    ignored = false, -- Exclude git-ignored files
    exclude = project_excludes(),
    -- Append the current selection's `[row/matched]` to the title as the cursor moves
    -- (the built-in x/y counter is matched/total and doesn't follow the cursor).
    on_change = show_cursor_position,
    -- Default previewer for sources without their own (files/grep/explorer/…):
    -- decompiles .class files via Fernflower, otherwise the built-in file preview.
    -- preview = file_preview,
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
        toggle_exclude = toggle_exclude,
        toggle_layout = toggle_layout,
        grep_selected_files = grep_selected_files,
        switch_to_files = picker_util.switch_to_files,
        switch_to_buffers = picker_util.switch_to_buffers,
        switch_to_grep = picker_util.switch_to_grep,
        switch_to_recent = picker_util.switch_to_recent,
        switch_to_next_picker = picker_util.switch_to_next_picker,
        switch_to_prev_picker = picker_util.switch_to_prev_picker,
    },
    win = {
        input = {
            keys = {
                ["<c-,>"] = { "switch_to_prev_picker", mode = { "i", "n" }, desc = "Switch to previous picker" },
                ["<c-.>"] = { "switch_to_next_picker", mode = { "i", "n" }, desc = "Switch to next picker" },
                -- ["<c-/>"] = { "switch_to_grep", mode = { "i", "n" }, desc = "Switch to grep" },
                -- ["<c-_>"] = { "switch_to_grep", mode = { "i", "n" }, desc = "Switch to grep" },
                -- ["<c-o>"] = { "switch_to_recent", mode = { "i", "n" }, desc = "Switch to recent" },
                -- ["<c-]>"] = { "switch_to_next_picker", mode = { "i", "n" }, desc = "Switch to next picker" },
                -- ["<c-e>"] = { "switch_to_prev_picker", mode = { "i", "n" }, desc = "Switch to previous picker" },
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
                ["<c-d>"] = { "diff_selected", mode = { "i", "n" } },
                ["<c-y>"] = { "toggle_layout", mode = { "i", "n" } },
                ["<c-e>"] = { "toggle_exclude", mode = { "i", "n" }, desc = "Toggle excluded dirs/files" },
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
                ["<c-,>"] = { "switch_to_prev_picker", mode = { "n" }, desc = "Switch to previous picker" },
                ["<c-.>"] = { "switch_to_next_picker", mode = { "n" }, desc = "Switch to next picker" },
                -- ["<c-/>"] = { "switch_to_grep", mode = { "n" }, desc = "Switch to grep" },
                -- ["<c-_>"] = { "switch_to_grep", mode = { "n" }, desc = "Switch to grep" },
                -- ["<c-o>"] = { "switch_to_recent", mode = { "n" }, desc = "Switch to recent" },
                -- ["<c-]>"] = { "switch_to_next_picker", mode = { "n" }, desc = "Switch to next picker" },
                -- ["<c-e>"] = { "switch_to_prev_picker", mode = { "n" }, desc = "Switch to previous picker" },
                ["<c-\\>"] = { "edit_vsplit", mode = { "i", "n" } },
                ["<c-d>"] = { "diff_selected", mode = { "i", "n" } },
                ["<c-y>"] = { "toggle_layout", mode = { "n" } },
                ["<c-e>"] = { "toggle_exclude", mode = { "n" }, desc = "Toggle excluded dirs/files" },
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
