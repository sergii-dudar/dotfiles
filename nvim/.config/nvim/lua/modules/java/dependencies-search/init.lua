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
local include_dependencies = {
    -- "org.mapstruct",
    -- "com.fasterxml.jackson.core:jackson-databind",
    "ua.raiffeisen.payments",
}

local ignored_dependencies = {
    "software.amazon.awssdk",
    "org.springdoc",
    "org.springframework",
    "com.google",
    "org.apache",
    -- "org.springframework.boot:spring-boot-starter-actuator",
    -- "org.springframework.boot:spring-boot-health",
}

local state = {
    loaded = false,
    source_dirs_main = {},
    source_dirs_main_all = {},
    source_dirs_test = {},
    source_dirs_test_all = {},
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

-- Gradle module cache layout: <GRADLE_USER_HOME>/caches/modules-<N>/files-<X.Y>/
-- The numeric format versions (e.g. modules-2, files-2.1) have changed across
-- Gradle releases, so match them with wildcards instead of hardcoding.
local GRADLE_CACHE_PATTERN = "/caches/modules%-%d+/files%-[%d.]+/"

---@param path string
---@return boolean
local function is_gradle_cache_path(path)
    return path:find(GRADLE_CACHE_PATTERN) ~= nil
end

--- Return the coordinate-relative remainder after the gradle cache root, or nil.
---@param path string
---@return string|nil
local function gradle_cache_rel(path)
    return path:match(GRADLE_CACHE_PATTERN .. "(.+)")
end

-- Normalize a jar/dir path to a slash-form coordinate path for pattern matching.
-- Maven already stores the group as nested dirs (org/mapstruct/...), but Gradle
-- uses a single dotted segment (files-2.1/org.mapstruct/...), so we convert that
-- group segment's dots to slashes. Returns the input unchanged for Maven paths.
---@param path string
---@return string
local function coord_match_path(path)
    local g = gradle_cache_rel(path)
    if not g then
        return path
    end
    local group, rest = g:match("^([^/]+)/(.+)$")
    if group then
        return group:gsub("%.", "/") .. "/" .. rest
    end
    return g
end

-- Exposed for reuse (e.g. static-import-explorer's preferred-dep matching).
M.coord_match_path = coord_match_path

local function is_jar_ignored(jar_path)
    local match_path = coord_match_path(jar_path)
    if #include_dep_patterns > 0 then
        for _, pattern in ipairs(include_dep_patterns) do
            if match_path:find(pattern, 1, true) then
                return false
            end
        end
        return true
    end
    for _, pattern in ipairs(ignored_dep_patterns) do
        if match_path:find(pattern, 1, true) then
            return true
        end
    end
    return false
end

-- Parse Maven coordinates from a source dir path
-- ~/.m2/repository/org/mapstruct/mapstruct/1.5.5.Final/mapstruct-1.5.5.Final-sources
-- -> { label = "org.mapstruct:mapstruct:1.5.5.Final", dir = "..." }
local function source_dir_to_module(dir)
    -- Gradle: <cache>/modules-<N>/files-<X.Y>/<group.dotted>/<artifact>/<version>/<hash>/<artifact>-<version>-sources
    local gradle_rel = gradle_cache_rel(dir)
    if gradle_rel then
        local parts = vim.split(gradle_rel, "/")
        -- parts: {group.dotted, artifactId, version, hash, dirname}
        if #parts < 4 then
            return nil
        end
        return {
            label = parts[1] .. ":" .. parts[2] .. ":" .. parts[3],
            dir = dir,
        }
    end

    -- Maven: ~/.m2/repository/<group/parts>/<artifact>/<version>/<artifact>-<version>-sources
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

-- Resolve the sources jar + extraction dir for a binary dependency jar,
-- handling both Maven and Gradle cache layouts.
--   Maven : sources jar sits beside the binary jar in the same dir.
--   Gradle: each file lives in its own SHA1 hash dir under the version dir, so
--           the sources jar is in a sibling hash dir (not predictable from the
--           binary jar's hash) — we glob the version dir for it.
---@param jar string absolute path to a binary dependency jar
---@return string|nil sources_jar, string|nil sources_dir
local function resolve_sources(jar)
    if is_gradle_cache_path(jar) then
        local jar_name = vim.fn.fnamemodify(jar, ":t")
        local sources_name = jar_name:gsub("%.jar$", "-sources.jar")
        local version_dir = vim.fn.fnamemodify(jar, ":h:h")
        local matches = vim.fn.glob(version_dir .. "/*/" .. sources_name, false, true)
        if #matches > 0 then
            local sjar = matches[1]
            return sjar, (sjar:gsub("%.jar$", ""))
        end
        return nil, nil
    end

    local base = jar:gsub("%.jar$", "")
    return base .. "-sources.jar", base .. "-sources"
end

---@param opts? { on_done?: fun() }
--- Process a list of jars into source_dirs (filtered) and source_dirs_all (unfiltered).
--- Returns { dirs, dirs_all, to_extract, missing }
local function process_jars(jars)
    local dirs = {}
    local dirs_all = {}
    local to_extract = {}
    local missing = {}

    for _, jar in ipairs(jars) do
        local sources_jar, sources_dir = resolve_sources(jar)

        if sources_dir and vim.fn.isdirectory(sources_dir) == 1 then
            table.insert(dirs_all, sources_dir)
            if not is_jar_ignored(jar) then
                table.insert(dirs, sources_dir)
            end
        elseif sources_jar and vim.fn.filereadable(sources_jar) == 1 then
            table.insert(to_extract, { sources_jar = sources_jar, sources_dir = sources_dir })
            table.insert(dirs_all, sources_dir)
            if not is_jar_ignored(jar) then
                table.insert(dirs, sources_dir)
            end
        else
            table.insert(missing, vim.fn.fnamemodify(jar, ":t"))
        end
    end

    return { dirs = dirs, dirs_all = dirs_all, to_extract = to_extract, missing = missing }
end

local function classpaths_to_jars(classpaths)
    local jars = {}
    for _, entry in ipairs(classpaths) do
        if entry:match("%.jar$") then
            table.insert(jars, entry)
        end
    end
    return jars
end

function M.load_sources(opts)
    local on_done = opts and opts.on_done
    local bufnr = opts and opts.bufnr

    -- Load runtime (main) scope
    local main_cp = classpath_util.get_classpath_for_main_method_table({ scope = "runtime", bufnr = bufnr })
    if not main_cp then
        vim.notify("[Dep Search] Failed to get classpath from jdtls", vim.log.levels.WARN)
        return
    end

    -- Load test scope (superset of main)
    local test_cp = classpath_util.get_classpath_for_main_method_table({ scope = "test", bufnr = bufnr })
    if not test_cp then
        test_cp = main_cp
    end

    spinner.start("Loading dependency sources...")

    local main_result = process_jars(classpaths_to_jars(main_cp))
    local test_result = process_jars(classpaths_to_jars(test_cp))

    state.source_dirs_main = main_result.dirs
    state.source_dirs_main_all = main_result.dirs_all
    state.source_dirs_test = test_result.dirs
    state.source_dirs_test_all = test_result.dirs_all

    -- Modules from test (superset) for selectors
    local modules = {}
    for _, dir in ipairs(test_result.dirs_all) do
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
        local dir_pattern = "**/" .. pkg:gsub("%.", "/"):gsub("%*$", "**")
        table.insert(exclude, dir_pattern)
    end
    state.exclude = exclude

    -- Merge to_extract from both (deduplicated by sources_dir)
    local seen_extract = {}
    local to_extract = {}
    for _, item in ipairs(test_result.to_extract) do
        if not seen_extract[item.sources_dir] then
            seen_extract[item.sources_dir] = true
            table.insert(to_extract, item)
        end
    end

    -- Merge missing from both
    local missing_set = {}
    local missing_sources = {}
    for _, name in ipairs(test_result.missing) do
        if not missing_set[name] then
            missing_set[name] = true
            table.insert(missing_sources, name)
        end
    end

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
        table.insert(lines, "Run in project root to download sources, then re-run the search:")
        table.insert(lines, "  maven : mvn dependency:resolve -Dclassifier=sources")
        table.insert(lines, "  gradle: ./gradlew eclipse  (or enable downloadSources in your IDE plugin)")
        -- NOTE: disable for now notification
        -- vim.notify(table.concat(lines, "\n"), vim.log.levels.WARN)
    end

    state.loaded = true

    local total_dirs = #test_result.dirs
    if #to_extract == 0 then
        spinner.stop(
            true,
            string.format(
                "[Dep Search] Loaded %d main + %d test sources (%d not available)",
                #main_result.dirs,
                #test_result.dirs,
                #missing_sources
            )
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
                            "[Dep Search] Loaded %d main + %d test sources (%d extracted, %d not available)",
                            #main_result.dirs,
                            #test_result.dirs,
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

local function is_project_file(file)
    return not file:find("/.m2/repository/", 1, true) and not is_gradle_cache_path(file)
end

-- When jdt opener is on:
--   .java files: open via jdtls FQCN (jdt://contents/)
--   other files: open via jdt://jarentry/ URI
-- When jdt opener is off: default Snacks jump (raw file)
local function dep_confirm(picker, item)
    if not item then
        return
    end
    local file = item.file or ""
    if not use_jdt_opener or is_project_file(file) then
        return picker:action("jump")
    end
    picker:close()
    local line = item.pos and item.pos[1] or 1
    if file:match("%.java$") then
        local fqcn = require("utils.java.java-common").file_to_fqcn(file)
        require("utils.java.jdtls-util").jdt_open_class(fqcn, line)
    else
        if not jarentry.open(file, state.source_dirs_test_all, line) then
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

local function get_module_src_dir()
    local java_util = require("utils.java.java-common")
    local module_root = java_util.get_buffer_project_path()
    if not module_root then
        return nil
    end
    local src_dir = module_root .. "/src"
    if vim.fn.isdirectory(src_dir) == 1 then
        return src_dir
    end
    return nil
end

local function get_current_dirs()
    local dirs = use_all_dirs and state.source_dirs_test_all or state.source_dirs_test
    local src_dir = get_module_src_dir()
    if src_dir then
        dirs = vim.list_extend({ src_dir }, dirs)
    end
    return dirs
end

local function toggle_all_sources(picker)
    use_all_dirs = not use_all_dirs
    local dirs = get_current_dirs()
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
        dirs = dirs or get_current_dirs(),
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
                    if not jarentry.open(file, state.source_dirs_test_all) then
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

function M.reset()
    state.loaded = false
    state.source_dirs_main = {}
    state.source_dirs_main_all = {}
    state.source_dirs_test = {}
    state.source_dirs_test_all = {}
    state.modules = {}
    state.exclude = {}
    vim.notify("[Dep Search] Cache cleared. Will reload on next use.", vim.log.levels.INFO)
end

vim.api.nvim_create_user_command("DepSearchReset", function()
    M.reset()
    -- Clear static-import-explorer preferred deps cache
    pcall(function()
        require("modules.java.static-import-explorer.util").clear_preferred_cache()
    end)
end, { desc = "Clear dependency search cache and reload on next use" })

function M.is_loaded()
    return state.loaded
end

---@param scope? "main"|"test" defaults to "test" (superset)
function M.get_source_dirs(scope)
    if scope == "main" then
        return state.source_dirs_main
    end
    return state.source_dirs_test
end

---@param scope? "main"|"test" defaults to "test" (superset)
function M.get_source_dirs_all(scope)
    if scope == "main" then
        return state.source_dirs_main_all
    end
    return state.source_dirs_test_all
end

function M.get_exclude()
    return state.exclude
end

return M
