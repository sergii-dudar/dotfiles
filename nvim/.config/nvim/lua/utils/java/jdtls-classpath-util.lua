-- JDTLS Classpath Utility
-- Handles classpath resolution from jdtls and project structure
-- Can be used by any plugin that needs Java classpath information
--
-- • set_log_level — configure logging verbosity for this module
-- • is_jdtls_ready — check if JDTLS is attached and ready for a buffer
-- • get_classpath — resolve classpath for current project
-- • get_classpath_for_main_method_table — classpath as table for main class execution
-- • get_classpath_for_main_method — classpath as string for main class execution
-- • get_all_project_modules — list all maven/gradle modules in workspace
-- • get_classpath_for_module_uri — resolve classpath for a specific module URI
-- • clear_cache — clear cached classpath data

local log = require("utils.logging-util").new({ name = "Java.Classpath", filename = "java-classpath.log" })
local lsp_util = require("utils.lsp-util")

local M = {}

-- Set log level for this module
--- Set the log level for this module.
function M.set_log_level(level)
    log.set_level(level)
end

-- Cache for classpath results to avoid expensive LSP queries
local classpath_cache = {
    classpath = nil,
    timestamp = 0,
}
local CACHE_TTL_MS = 60000 -- 1 minute cache

-- Cache for jdtls ready state (monotonic - once ready, stays ready)
local jdtls_ready_cache = {
    ready = false,
    timestamp = 0,
}
local READY_CACHE_TTL_MS = 5000 -- 5 seconds cache (conservative)

-- Check if jdtls is ready and can provide classpath
--- Check whether JDTLS is ready for the buffer.
function M.is_jdtls_ready(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()

    -- Check cache first - jdtls ready state is monotonic (never goes backward)
    if jdtls_ready_cache.ready then
        local age_ms = vim.loop.now() - jdtls_ready_cache.timestamp
        if age_ms < READY_CACHE_TTL_MS then
            log.debug("Using cached jdtls ready state (age:", age_ms, "ms)")
            return true
        else
            log.debug("jdtls ready cache expired, rechecking...")
        end
    end

    -- Check if jdtls client is attached
    local client = lsp_util.get_client_by_name("jdtls", { bufnr = bufnr })
    if not client then
        log.debug("jdtls not attached")
        return false
    end

    -- Check if client is initialized
    if not client.initialized then
        log.debug("jdtls not initialized")
        return false
    end

    -- Try a quick test to see if we can get projects
    local projects_result, projects_err = client:request_sync("workspace/executeCommand", {
        command = "java.project.getAll",
        arguments = {},
    }, 2000, bufnr)

    if projects_err or not projects_result or not projects_result.result then
        log.debug("jdtls not ready - cannot get projects")
        return false
    end

    local project_uris = projects_result.result
    if type(project_uris) ~= "table" or #project_uris == 0 then
        log.debug("jdtls ready but no projects loaded yet")
        return false
    end

    log.debug("jdtls is ready with", #project_uris, "projects")

    -- Cache the positive result
    jdtls_ready_cache.ready = true
    jdtls_ready_cache.timestamp = vim.loop.now()
    log.debug("Cached jdtls ready state")

    return true
end

-- Deduplicate classpath entries (remove duplicates while preserving order)
local function deduplicate_classpaths(classpaths)
    local seen = {}
    local result = {}

    for _, cp in ipairs(classpaths) do
        if not seen[cp] then
            seen[cp] = true
            table.insert(result, cp)
        end
    end

    return result
end

-- Find project root directory (fallback when jdtls is not available)
local function find_project_root_fallback(bufnr)
    local file_path = vim.api.nvim_buf_get_name(bufnr)
    local dir = vim.fn.fnamemodify(file_path, ":p:h")

    -- Look for pom.xml, build.gradle, or build.gradle.kts
    while dir ~= "/" and dir ~= "." do
        if
            vim.fn.filereadable(dir .. "/pom.xml") == 1
            or vim.fn.filereadable(dir .. "/build.gradle") == 1
            or vim.fn.filereadable(dir .. "/build.gradle.kts") == 1
        then
            return dir
        end
        dir = vim.fn.fnamemodify(dir, ":h")
    end

    return nil
end

-- Get classpath from project structure (fallback)
local function get_classpath_from_project(bufnr)
    local project_root = find_project_root_fallback(bufnr)
    if not project_root then
        log.debug("Could not find project root")
        return nil
    end

    log.debug("Project root:", project_root)

    local classpaths = {}

    -- Determine build tool
    local is_maven = vim.fn.filereadable(project_root .. "/pom.xml") == 1
    local is_gradle = vim.fn.filereadable(project_root .. "/build.gradle") == 1
        or vim.fn.filereadable(project_root .. "/build.gradle.kts") == 1

    if is_maven then
        -- Maven structure
        local maven_paths = {
            project_root .. "/target/classes",
            project_root .. "/target/test-classes",
            -- Include generated sources (like MapStruct generated code)
            project_root .. "/target/generated-sources/annotations",
        }

        for _, path in ipairs(maven_paths) do
            if vim.fn.isdirectory(path) == 1 then
                table.insert(classpaths, path)
                log.debug("Found Maven classpath:", path)
            end
        end
    elseif is_gradle then
        -- Gradle structure - check multiple possible layouts
        local gradle_paths = {
            -- Standard Gradle layout
            project_root .. "/build/classes/java/main",
            project_root .. "/build/classes/java/test",
            -- Kotlin DSL
            project_root .. "/build/classes/kotlin/main",
            project_root .. "/build/classes/kotlin/test",
            -- Generated sources (MapStruct)
            project_root .. "/build/generated/sources/annotationProcessor/java/main",
        }

        for _, path in ipairs(gradle_paths) do
            if vim.fn.isdirectory(path) == 1 then
                table.insert(classpaths, path)
                log.debug("Found Gradle classpath:", path)
            end
        end
    end

    if #classpaths > 0 then
        return table.concat(classpaths, ":")
    end

    log.warn("No classpath entries found in project structure")
    return nil
end

-- Get classpath from jdtls for all modules
local function get_jdtls_classpath_of_all_modules(bufnr)
    -- Find jdtls client
    local client = lsp_util.get_client_by_name("jdtls", { bufnr = bufnr })
    if not client then
        log.warn("No jdtls client found")
        vim.notify("[MapStruct] No jdtls client found - classpath may be incomplete", vim.log.levels.WARN)
        return nil
    end

    -- Get all projects/modules
    log.debug("Getting all projects from jdtls...")
    local projects_result, projects_err = client:request_sync("workspace/executeCommand", {
        command = "java.project.getAll",
        arguments = {},
    }, 5000, bufnr)

    if projects_err then
        log.warn("jdtls java.project.getAll error:", projects_err)
        vim.notify(
            "[Java Classpath] Failed to get projects from jdtls: " .. tostring(projects_err),
            vim.log.levels.WARN
        )
        return nil
    end

    if not projects_result or not projects_result.result then
        log.warn("jdtls java.project.getAll returned empty result")
        vim.notify("[Java Classpath] No projects found from jdtls", vim.log.levels.WARN)
        return nil
    end

    local project_uris = projects_result.result
    if type(project_uris) ~= "table" or #project_uris == 0 then
        log.warn("No projects found from jdtls")
        vim.notify("[Java Classpath] No projects found from jdtls", vim.log.levels.WARN)
        return nil
    end

    log.info("Found", #project_uris, "projects/modules from jdtls")

    -- Collect all classpaths from all modules
    local all_classpaths = {}
    local successful_modules = 0

    for _, project_uri in ipairs(project_uris) do
        local project_path = vim.uri_to_fname(project_uri)
        log.debug("Getting classpath for module:", project_path)

        local module_has_classpath = false

        -- Get BOTH runtime and test classpaths for this module
        for _, scope in ipairs({ "runtime", "test" }) do
            local result, err = client:request_sync("workspace/executeCommand", {
                command = "java.project.getClasspaths",
                arguments = { project_uri, vim.json.encode({ scope = scope }) },
            }, 10000, bufnr)

            if result and result.result then
                local classpaths = result.result.classpaths or result.result
                if type(classpaths) == "table" and #classpaths > 0 then
                    log.debug("Got", #classpaths, scope, "classpath entries for", project_path)
                    for _, cp in ipairs(classpaths) do
                        table.insert(all_classpaths, cp)
                    end
                    module_has_classpath = true
                end
            else
                if err then
                    log.debug("jdtls", scope, "scope error for", project_path, ":", err)
                end
            end
        end

        if module_has_classpath then
            successful_modules = successful_modules + 1
        else
            log.warn("No classpath entries found for module:", project_path)
        end
    end

    if #all_classpaths == 0 then
        log.warn("No classpath entries found from any module")
        vim.notify("[Java Classpath] Could not load classpath from jdtls", vim.log.levels.WARN)
        return nil
    end

    -- Deduplicate classpaths
    local deduplicated = deduplicate_classpaths(all_classpaths)
    log.info(
        "Loaded classpath from",
        successful_modules,
        "modules |",
        "Total entries:",
        #all_classpaths,
        "| After deduplication:",
        #deduplicated
    )

    return table.concat(deduplicated, ":")
end

-- Get classpath with caching
-- opts: { use_cache = true, bufnr = nil }
--- Resolve the project classpath with caching.
function M.get_classpath(opts)
    opts = opts or {}
    local bufnr = opts.bufnr or vim.api.nvim_get_current_buf()
    local use_cache = opts.use_cache ~= false -- default true

    -- Check cache
    if use_cache and classpath_cache.classpath then
        local age_ms = vim.loop.now() - classpath_cache.timestamp
        if age_ms < CACHE_TTL_MS then
            log.debug("Using cached classpath (age:", age_ms, "ms)")
            return classpath_cache.classpath
        else
            log.debug("Cache expired")
            classpath_cache.classpath = nil
        end
    end

    -- Try jdtls first
    local classpath = get_jdtls_classpath_of_all_modules(bufnr)

    if not classpath then
        -- Fallback to project structure
        log.info("Falling back to project structure classpath")
        classpath = get_classpath_from_project(bufnr)

        if not classpath then
            log.error("Failed to get classpath from both jdtls and project structure")
            vim.notify("[Java Classpath] Failed to load classpath", vim.log.levels.WARN)
        end
    end

    -- Cache the result if successful
    if classpath and use_cache then
        classpath_cache.classpath = classpath
        classpath_cache.timestamp = vim.loop.now()
        log.debug("Cached classpath")
    end

    return classpath
end

-- Detect if current file is in test scope based on path
local function is_test_file(file_path)
    return file_path:match("/src/test/") ~= nil
end

---@class jdtls.classpath.Options
---@field bufnr? integer
---@field scope? "runtime"|"test"|nil

-- Get classpath for running main method in current buffer
-- Automatically detects if buffer file is in test or main scope
---@param opts? jdtls.classpath.Options
function M.get_classpath_for_main_method_table(opts)
    opts = opts or {}
    local bufnr = opts.bufnr or vim.api.nvim_get_current_buf()

    -- Get jdtls client
    local client = lsp_util.get_client_by_name("jdtls", { bufnr = bufnr })
    if not client then
        log.warn("jdtls not attached to buffer")
        vim.notify("[Java Classpath] jdtls not attached", vim.log.levels.WARN)
        return nil
    end

    if not client.initialized then
        log.warn("jdtls not initialized")
        vim.notify("[Java Classpath] jdtls not initialized", vim.log.levels.WARN)
        return nil
    end

    -- Get current file URI.
    -- java.project.getClasspaths is keyed on a file that BELONGS to a project. A
    -- jdt:// (library) buffer or an unnamed buffer has no project-owning URI —
    -- vim.uri_from_fname would also mangle a jdt:// name into a bogus file:// URI —
    -- so fall back to a real source file that identifies the right project.
    local function is_real_java(p)
        return p ~= "" and not p:match("^%w[%w+.-]*://") and p:match("%.java$") ~= nil and vim.fn.filereadable(p) == 1
    end

    local file_path = vim.api.nvim_buf_get_name(bufnr)
    local file_uri
    if file_path ~= "" and not file_path:match("^%w[%w+.-]*://") and vim.fn.filereadable(file_path) == 1 then
        file_uri = vim.uri_from_fname(file_path)
    else
        -- Resolve a representative project file. Order matters for multi-module repos:
        -- we want a file in the module the user actually came from, so getClasspaths
        -- keys to the correct module.
        --   1. Walk the jumplist backwards for the most recent real project .java file.
        --      This survives library→library hops (gd into LibA, then into LibB): the
        --      alternate buffer would only be LibA, but the jumplist still holds the
        --      source file underneath. LSP go-to-definition pushes a jump, so the origin
        --      is recorded; entries may reference wiped buffers, hence the validation.
        --   2. Alternate buffer (#) — covers non-jump navigation (:b#, picker) the
        --      jumplist misses.
        --   3. root_dir globs. root_dir is the NEAREST marker dir (vim.fs.root) — i.e.
        --      the module dir in a Maven/Gradle reactor — so src/main/java usually exists
        --      directly under it; the recursive `**` glob is a last resort for
        --      aggregator-root layouts (arbitrary module, potentially slow).
        local candidate
        local jumps = vim.fn.getjumplist()[1] -- current window's jumplist, oldest→newest
        for i = #jumps, 1, -1 do
            local b = jumps[i].bufnr
            if b and vim.api.nvim_buf_is_valid(b) then
                local name = vim.api.nvim_buf_get_name(b)
                if is_real_java(name) then
                    candidate = name
                    break
                end
            end
        end
        if not candidate then
            local alt = vim.fn.bufnr("#")
            if alt > 0 then
                local alt_name = vim.api.nvim_buf_get_name(alt)
                if is_real_java(alt_name) then
                    candidate = alt_name
                end
            end
        end
        if not candidate then
            local root = client.config.root_dir
            candidate = root
                and (
                    vim.fn.glob(root .. "/src/main/java/**/*.java", false, true)[1]
                    or vim.fn.glob(root .. "/src/test/java/**/*.java", false, true)[1]
                    or vim.fn.glob(root .. "/**/*.java", false, true)[1]
                )
        end
        if not candidate then
            log.warn("No project file found to resolve classpath for buffer:", file_path)
            vim.notify(
                "[Java Classpath] Can't resolve a project file for classpath (library/unnamed buffer)",
                vim.log.levels.WARN
            )
            return nil
        end
        file_path = candidate
        file_uri = vim.uri_from_fname(candidate)
    end

    -- Use provided scope or auto-detect from file path
    local scope = opts.scope or (is_test_file(file_path) and "test" or "runtime")
    log.info("Using scope:", scope, "for file:", file_path)

    -- Get classpath for the file's project
    local result, err = client:request_sync("workspace/executeCommand", {
        command = "java.project.getClasspaths",
        arguments = { file_uri, vim.json.encode({ scope = scope }) },
    }, 10000, bufnr)

    if err then
        log.warn("Failed to get classpath:", err)
        vim.notify("[Java Classpath] Failed to get classpath: " .. tostring(err), vim.log.levels.WARN)
        return nil
    end

    if not result or not result.result then
        log.warn("Empty result from java.project.getClasspaths")
        vim.notify("[Java Classpath] No classpath returned", vim.log.levels.WARN)
        return nil
    end

    local classpaths = result.result.classpaths or result.result
    if type(classpaths) ~= "table" or #classpaths == 0 then
        log.warn("No classpath entries found")
        vim.notify("[Java Classpath] No classpath entries found", vim.log.levels.WARN)
        return nil
    end

    local deduplicated = deduplicate_classpaths(classpaths)
    log.info("Got", #deduplicated, scope, "classpath entries for main method")

    -- return table.concat(deduplicated, ":")
    return deduplicated
end

---@param opts? jdtls.classpath.Options
function M.get_classpath_for_main_method(opts)
    local classpath = M.get_classpath_for_main_method_table(opts)
    if classpath then
        return table.concat(classpath, ":")
    end
    return nil
end

-- Get all project modules from jdtls
---@param bufnr? integer
---@return { uri: string, path: string, name: string }[]|nil
function M.get_all_project_modules(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()

    local client = lsp_util.get_client_by_name("jdtls", { bufnr = bufnr })
    if not client then
        log.warn("No jdtls client found")
        return nil
    end

    local result, err = client:request_sync("workspace/executeCommand", {
        command = "java.project.getAll",
        arguments = {},
    }, 5000, bufnr)

    if err or not result or not result.result then
        log.warn("java.project.getAll failed:", err)
        return nil
    end

    local project_uris = result.result
    if type(project_uris) ~= "table" or #project_uris == 0 then
        log.warn("No projects found")
        return nil
    end

    local modules = {}
    for _, uri in ipairs(project_uris) do
        -- Only include file: URIs (skip jdt:// virtual projects)
        if uri:match("^file:") then
            local path = vim.uri_to_fname(uri):gsub("/$", "")
            local name = vim.fn.fnamemodify(path, ":t")
            table.insert(modules, { uri = uri, path = path, name = name })
        end
    end

    log.info("Found", #modules, "file-based project modules")
    return #modules > 0 and modules or nil
end

-- Get classpath for a single module URI
---@param module_uri string
---@param opts? { scope?: string, bufnr?: integer }
---@return string|nil
function M.get_classpath_for_module_uri(module_uri, opts)
    opts = opts or {}
    local bufnr = opts.bufnr or vim.api.nvim_get_current_buf()
    local scope = opts.scope or "test"

    local client = lsp_util.get_client_by_name("jdtls", { bufnr = bufnr })
    if not client then
        log.warn("No jdtls client found")
        return nil
    end

    local all_classpaths = {}

    for _, s in ipairs({ "runtime", scope }) do
        local result, err = client:request_sync("workspace/executeCommand", {
            command = "java.project.getClasspaths",
            arguments = { module_uri, vim.json.encode({ scope = s }) },
        }, 10000, bufnr)

        if result and result.result then
            local classpaths = result.result.classpaths or result.result
            if type(classpaths) == "table" then
                for _, cp in ipairs(classpaths) do
                    table.insert(all_classpaths, cp)
                end
            end
        else
            if err then
                log.debug("getClasspaths", s, "error for", module_uri, ":", err)
            end
        end
    end

    if #all_classpaths == 0 then
        log.warn("No classpath entries for module:", module_uri)
        return nil
    end

    local deduplicated = deduplicate_classpaths(all_classpaths)
    return table.concat(deduplicated, ":")
end

-- Clear cache
--- Clear cached JDTLS classpath state.
function M.clear_cache()
    classpath_cache.classpath = nil
    classpath_cache.timestamp = 0
    jdtls_ready_cache.ready = false
    jdtls_ready_cache.timestamp = 0
    log.debug("Cleared classpath and jdtls ready caches")
end

return M
