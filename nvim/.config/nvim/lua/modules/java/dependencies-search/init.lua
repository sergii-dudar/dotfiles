local classpath_util = require("utils.java.jdtls-classpath-util")
local jarentry = require("modules.java.dependencies-search.jarentry")
local spinner = require("utils.ui.spinner")

local M = {}

-- stylua: ignore
local ignored_extensions = {
        "txt",
        "MD",
        "md",
        "MF",
        "mf",
        "html",
        "css",
        "js",
        "png",
        "gif",
        "jpg",
        "svg",
        "dtd",
        "factories",
        "imports",
        "Provider"
}

local ignored_file_names = {
    "module-info.java",
    "package-info.java",
    "DEPENDENCIES",
    "NOTICE",
    "LICENSE",
}

-- it's filtering inside particular library directory
local ignored_packages = {
    -- "org.springframework.*",
    -- "org.junit.*",
}

-- Filter dependencies by Maven coordinates (matched against ~/.m2/repository path).
-- Format: "groupId" to match all artifacts in group, or "groupId:artifactId" for specific one.
--
-- When include_dependencies is non-empty, ONLY matching jars are included (whitelist mode).
-- When it's empty, ignored_dependencies is used as a blacklist instead.
-- stylua: ignore
local include_dependencies = {
    -- "org.mapstruct",
    -- "com.fasterxml.jackson.core:jackson-databind",
    "ua.raiffeisen.payments"
}

-- stylua: ignore
local ignored_dependencies = {
        "software.amazon.awssdk",
        "org.springdoc",
        "org.springframework",
        "com.google",
        "org.apache"
    -- "org.springframework.boot:spring-boot-starter-actuator",
    -- "org.springframework.boot:spring-boot-health",
}

local state = {
    loaded = false,
    source_dirs = {},
    source_dirs_all = {},
    modules = {}, -- { label, dir } for all source dirs
    exclude = {},
}

local function to_path_patterns(deps)
    local patterns = {}
    for _, dep in ipairs(deps) do
        table.insert(patterns, dep:gsub(":", "/"):gsub("%.", "/") .. "/")
    end
    return patterns
end

local include_dep_patterns = to_path_patterns(include_dependencies)
local ignored_dep_patterns = to_path_patterns(ignored_dependencies)

local function is_jar_ignored(jar_path)
    if #include_dep_patterns > 0 then
        for _, pattern in ipairs(include_dep_patterns) do
            if jar_path:find(pattern, 1, true) then
                return false
            end
        end
        return true
    end
    for _, pattern in ipairs(ignored_dep_patterns) do
        if jar_path:find(pattern, 1, true) then
            return true
        end
    end
    return false
end

-- Parse Maven coordinates from a source dir path
-- ~/.m2/repository/org/mapstruct/mapstruct/1.5.5.Final/mapstruct-1.5.5.Final-sources
-- -> { label = "org.mapstruct:mapstruct:1.5.5.Final", dir = "..." }
local function source_dir_to_module(dir)
    local repo_rel = dir:match(".m2/repository/(.+)")
    if not repo_rel then
        return nil
    end
    local parts = vim.split(repo_rel, "/")
    -- parts: {groupId...}, artifactId, version, dirname
    if #parts < 4 then
        return nil
    end
    local version = parts[#parts - 1]
    local artifact_id = parts[#parts - 2]
    local group_parts = {}
    for i = 1, #parts - 3 do
        table.insert(group_parts, parts[i])
    end
    local group_id = table.concat(group_parts, ".")
    return {
        label = group_id .. ":" .. artifact_id .. ":" .. version,
        dir = dir,
    }
end

local function jar_to_sources_dir(jar_path)
    local base = jar_path:gsub("%.jar$", "")
    return base .. "-sources"
end

local function jar_to_sources_jar(jar_path)
    return jar_to_sources_dir(jar_path) .. ".jar"
end

---@param opts? { on_done?: fun() }
function M.load_sources(opts)
    local on_done = opts and opts.on_done
    local classpaths = classpath_util.get_classpath_for_main_method_table({ scope = "test" })
    if not classpaths then
        vim.notify("[Dep Search] Failed to get classpath from jdtls", vim.log.levels.WARN)
        return
    end

    -- Filter to .jar entries only
    local jars = {}
    for _, entry in ipairs(classpaths) do
        if entry:match("%.jar$") then
            table.insert(jars, entry)
        end
    end

    spinner.start("Loading dependency sources...")

    local source_dirs = {}
    local source_dirs_all = {}
    local to_extract = {}
    local missing_sources = {}

    for _, jar in ipairs(jars) do
        local sources_jar = jar_to_sources_jar(jar)
        local sources_dir = jar_to_sources_dir(jar)

        if vim.fn.isdirectory(sources_dir) == 1 then
            table.insert(source_dirs_all, sources_dir)
            if not is_jar_ignored(jar) then
                table.insert(source_dirs, sources_dir)
            end
        elseif vim.fn.filereadable(sources_jar) == 1 then
            table.insert(to_extract, { sources_jar = sources_jar, sources_dir = sources_dir })
            table.insert(source_dirs_all, sources_dir)
            if not is_jar_ignored(jar) then
                table.insert(source_dirs, sources_dir)
            end
        else
            table.insert(missing_sources, vim.fn.fnamemodify(jar, ":t"))
        end
    end

    state.source_dirs = source_dirs
    state.source_dirs_all = source_dirs_all

    local modules = {}
    for _, dir in ipairs(source_dirs_all) do
        local mod = source_dir_to_module(dir)
        if mod then
            table.insert(modules, mod)
        end
    end
    table.sort(modules, function(a, b)
        return a.label < b.label
    end)
    state.modules = modules

    local exclude = {}
    for _, ext in ipairs(ignored_extensions) do
        table.insert(exclude, "*." .. ext)
    end
    for _, file_name in ipairs(ignored_file_names) do
        table.insert(exclude, file_name)
    end

    for _, pkg in ipairs(ignored_packages) do
        -- org.springframework.* -> org/springframework/**
        local dir_pattern = "**/" .. pkg:gsub("%.", "/"):gsub("%*$", "**")
        table.insert(exclude, dir_pattern)
    end
    state.exclude = exclude

    local function notify_missing()
        if #missing_sources == 0 then
            return
        end
        local lines = {
            string.format("[Dep Search] %d dependencies have no sources jar:", #missing_sources),
        }
        for i, name in ipairs(missing_sources) do
            if i > 10 then
                table.insert(lines, string.format("  ... and %d more", #missing_sources - 10))
                break
            end
            table.insert(lines, "  " .. name)
        end
        table.insert(lines, "")
        table.insert(lines, "Run in project root to download sources:")
        table.insert(lines, "  mvn dependency:resolve -Dclassifier=sources")
        table.insert(lines, "Then press <leader>jdl again.")
        -- NOTE: disable for now notification
        -- vim.notify(table.concat(lines, "\n"), vim.log.levels.WARN)
    end

    state.loaded = true

    if #to_extract == 0 then
        spinner.stop(
            true,
            string.format("[Dep Search] Loaded %d sources (%d not available)", #source_dirs, #missing_sources)
        )
        notify_missing()
        if on_done then
            on_done()
        end
        return
    end

    -- Extract all pending sources asynchronously
    local pending = #to_extract
    for _, item in ipairs(to_extract) do
        vim.system({ "unzip", "-o", "-q", item.sources_jar, "-d", item.sources_dir }, {}, function(result)
            pending = pending - 1
            if result.code ~= 0 then
                vim.schedule(function()
                    vim.notify(
                        "[Dep Search] Failed to extract: " .. vim.fn.fnamemodify(item.sources_jar, ":t"),
                        vim.log.levels.WARN
                    )
                end)
            end
            if pending == 0 then
                vim.schedule(function()
                    spinner.stop(
                        true,
                        string.format(
                            "[Dep Search] Loaded %d sources (%d newly extracted, %d not available)",
                            #source_dirs,
                            #to_extract,
                            #missing_sources
                        )
                    )
                    notify_missing()
                    if on_done then
                        on_done()
                    end
                end)
            end
        end)
    end
end

local function ensure_loaded(callback)
    if state.loaded then
        callback()
    else
        M.load_sources({ on_done = callback })
    end
end

local use_jdt_opener = true

-- When jdt opener is on:
--   .java files: open via jdtls FQCN (jdt://contents/)
--   other files: open via jdt://jarentry/ URI
-- When jdt opener is off: default Snacks jump (raw file)
local function dep_confirm(picker, item)
    if not item then
        return
    end
    local file = item.file or ""
    if not use_jdt_opener then
        return picker:action("jump")
    end
    picker:close()
    local line = item.pos and item.pos[1] or 1
    if file:match("%.java$") then
        local fqcn = require("utils.java.java-common").file_to_fqcn(file)
        require("utils.java.jdtls-util").jdt_open_class(fqcn, line)
    else
        if not jarentry.open(file, state.source_dirs_all, line) then
            vim.cmd("edit " .. vim.fn.fnameescape(file))
            if line > 1 then
                pcall(vim.api.nvim_win_set_cursor, 0, { line, 0 })
            end
        end
    end
end

local function toggle_jdt_opener(picker)
    use_jdt_opener = not use_jdt_opener
    local mode = use_jdt_opener and "jdtls" or "file"
    vim.notify("[Dep Search] Open mode: " .. mode, vim.log.levels.INFO)
end

local use_all_dirs = false

local function toggle_all_sources(picker)
    use_all_dirs = not use_all_dirs
    local dirs = use_all_dirs and state.source_dirs_all or state.source_dirs
    picker.opts.dirs = dirs
    picker:find()
    local label = use_all_dirs and "all" or "filtered"
    vim.notify(string.format("[Dep Search] Sources: %s (%d dirs)", label, #dirs), vim.log.levels.INFO)
end

-- Forward declarations for select_module (needs open_picker reference)
local open_picker

local function select_module(picker)
    local labels = vim.tbl_map(function(m)
        return m.label
    end, state.modules)

    -- Remember picker type so we can relaunch after selection
    local source = picker.opts.source
    picker:close()

    vim.ui.select(labels, { prompt = "Select dependency module" }, function(choice)
        if not choice then
            -- Relaunch with original dirs on cancel
            open_picker(source)
            return
        end
        for _, mod in ipairs(state.modules) do
            if mod.label == choice then
                open_picker(source, { mod.dir }, "Dep: " .. mod.label)
                break
            end
        end
    end)
end

local dep_picker_actions = {
    toggle_jdt_opener = toggle_jdt_opener,
    toggle_all_sources = toggle_all_sources,
    select_module = select_module,
}

local dep_picker_keys = {
    ["<C-o>"] = { "toggle_jdt_opener", mode = { "n", "i" }, desc = "Toggle jdtls/file opener" },
    ["<C-a>"] = { "toggle_all_sources", mode = { "n", "i" }, desc = "Toggle all/filtered sources" },
    ["<C-s>"] = { "select_module", mode = { "n", "i" }, desc = "Scope to single module" },
}

---@param source "files"|"grep"
---@param dirs? string[] override dirs (nil = use default filtered dirs)
---@param title? string override title
open_picker = function(source, dirs, title)
    local picker_fn = source == "files" and Snacks.picker.files or Snacks.picker.grep
    local default_title = source == "files" and "Dependency Sources" or "Grep Dependency Sources"
    picker_fn({
        dirs = dirs or state.source_dirs,
        exclude = state.exclude,
        title = title or default_title,
        confirm = dep_confirm,
        actions = dep_picker_actions,
        win = {
            input = { keys = dep_picker_keys },
            list = { keys = dep_picker_keys },
        },
    })
end

function M.find_files()
    ensure_loaded(function()
        open_picker("files")
    end)
end

function M.grep()
    ensure_loaded(function()
        open_picker("grep")
    end)
end

local selected_explore_module = nil

local function change_module(picker)
    picker:close()
    selected_explore_module = nil
    M.explore()
end

local explorer_actions = {
    toggle_jdt_opener = toggle_jdt_opener,
    change_module = change_module,
}

local explorer_keys = {
    ["<C-o>"] = { "toggle_jdt_opener", mode = { "n", "i" }, desc = "Toggle jdtls/file opener" },
    ["<C-s>"] = { "change_module", mode = { "n", "i" }, desc = "Change module" },
}

local function open_explorer(mod)
    selected_explore_module = mod

    Snacks.picker.explorer({
        cwd = mod.dir,
        title = "Explore: " .. mod.label,
        actions = explorer_actions,
        win = {
            input = { keys = explorer_keys },
            list = { keys = explorer_keys },
        },
        config = function(opts)
            local orig_confirm = opts.actions.confirm
            opts.actions.confirm = function(picker, item, action)
                if not item or item.dir or not use_jdt_opener then
                    return orig_confirm(picker, item, action)
                end
                local file = item.file or ""
                if file:match("%.java$") then
                    picker:close()
                    local fqcn = require("utils.java.java-common").file_to_fqcn(file)
                    require("utils.java.jdtls-util").jdt_open_class(fqcn)
                else
                    picker:close()
                    if not jarentry.open(file, state.source_dirs_all) then
                        return orig_confirm(picker, item, action)
                    end
                end
            end
            return opts
        end,
        on_show = function(picker)
            local Tree = require("snacks.explorer.tree")
            local function open_all(path)
                Tree:open(path)
                for name, t in vim.fs.dir(path) do
                    if t == "directory" then
                        open_all(path .. "/" .. name)
                    end
                end
            end
            open_all(mod.dir)
            picker:find()
        end,
    })
end

function M.explore()
    ensure_loaded(function()
        if selected_explore_module then
            open_explorer(selected_explore_module)
            return
        end

        local labels = vim.tbl_map(function(m)
            return m.label
        end, state.modules)

        vim.ui.select(labels, { prompt = "Select dependency to explore" }, function(choice)
            if not choice then
                return
            end
            for _, mod in ipairs(state.modules) do
                if mod.label == choice then
                    open_explorer(mod)
                    break
                end
            end
        end)
    end)
end

return M