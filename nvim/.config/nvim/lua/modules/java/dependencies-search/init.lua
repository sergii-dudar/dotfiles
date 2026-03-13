local classpath_util = require("utils.java.jdtls-classpath-util")
local spinner = require("utils.ui.spinner")

local M = {}

-- stylua: ignore
local ignored_extensions = { "txt", "md", "mf", "html", "css", "js", "png", "gif", "jpg", "svg", "dtd", "xsd", "xml" }

-- stylua: ignore
local ignored_packages = {
    "org.springframework.*",
    "org.junit.*",
}

local state = {
    loaded = false,
    source_dirs = {},
    exclude_globs = {},
}

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

    local exclude_globs = {}
    for _, ext in ipairs(ignored_extensions) do
        table.insert(exclude_globs, "!*." .. ext)
    end
    for _, pkg in ipairs(ignored_packages) do
        -- org.springframework.* -> !**/org/springframework/**
        local dir_pattern = pkg:gsub("%.", "/"):gsub("%*$", "**")
        table.insert(exclude_globs, "!**/" .. dir_pattern)
    end
    state.exclude_globs = exclude_globs

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
        vim.notify(table.concat(lines, "\n"), vim.log.levels.WARN)
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

function M.find_files()
    ensure_loaded(function()
        Snacks.picker.files({ dirs = state.source_dirs, glob = state.exclude_globs, title = "Dependency Sources" })
    end)
end

function M.grep()
    ensure_loaded(function()
        Snacks.picker.grep({ dirs = state.source_dirs, glob = state.exclude_globs, title = "Grep Dependency Sources" })
    end)
end

return M
