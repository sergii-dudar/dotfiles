local classpath_util = require("utils.java.jdtls-classpath-util")
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

    -- Filter to .jar entries only, skip ignored dependencies
    local jars = {}
    for _, entry in ipairs(classpaths) do
        if entry:match("%.jar$") and not is_jar_ignored(entry) then
            table.insert(jars, entry)
        end
    end

    spinner.start("Loading dependency sources...")

    local source_dirs = {}
    local to_extract = {}
    local missing_sources = {}

    for _, jar in ipairs(jars) do
        local sources_jar = jar_to_sources_jar(jar)
        local sources_dir = jar_to_sources_dir(jar)

        if vim.fn.isdirectory(sources_dir) == 1 then
            table.insert(source_dirs, sources_dir)
        elseif vim.fn.filereadable(sources_jar) == 1 then
            table.insert(to_extract, { sources_jar = sources_jar, sources_dir = sources_dir })
            table.insert(source_dirs, sources_dir)
        else
            table.insert(missing_sources, vim.fn.fnamemodify(jar, ":t"))
        end
    end

    state.source_dirs = source_dirs

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

-- For .java files: open via jdtls (navigates to decompiled source with proper LSP support)
-- For other files: fall back to default Snacks jump (open file directly)
local function dep_confirm(picker, item)
    if not item then
        return
    end
    local file = item.file or ""
    if file:match("%.java$") then
        picker:close()
        local line = item.pos and item.pos[1] or 1
        local fqcn = file_to_fqcn(file)
        require("utils.java.jdtls-util").jdt_open_class(fqcn, line)
    else
        return Snacks.picker.actions.jump(picker, item)
    end
end

function M.find_files()
    ensure_loaded(function()
        Snacks.picker.files({
            dirs = state.source_dirs,
            exclude = state.exclude,
            title = "Dependency Sources",
            confirm = dep_confirm,
        })
    end)
end

function M.grep()
    ensure_loaded(function()
        Snacks.picker.grep({
            dirs = state.source_dirs,
            exclude = state.exclude,
            title = "Grep Dependency Sources",
            confirm = dep_confirm,
        })
    end)
end

return M
