-- JDTLS Classpath Utility
-- Handles classpath resolution from jdtls and project structure
-- Can be used by any plugin that needs Java classpath information

local log = require("utils.logging-util").new({ name = "Java.Classpath", filename = "java-classpath.log" })

local M = {}

-- Set log level for this module
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
    local clients = vim.lsp.get_clients({ name = "jdtls", bufnr = bufnr })
    if not clients or #clients == 0 then
        log.debug("jdtls not attached")
        return false
    end

    local client = clients[1]

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
    local clients = vim.lsp.get_clients({ name = "jdtls", bufnr = bufnr })
    if not clients or #clients == 0 then
        log.warn("No jdtls client found")
        vim.notify("[MapStruct] No jdtls client found - classpath may be incomplete", vim.log.levels.WARN)
        return nil
    end

    local client = clients[1]

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

-- Get classpath for running main method in current buffer
-- Automatically detects if file is in test or main scope
function M.get_classpath_for_main_method_table(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()

    -- Get jdtls client
    local clients = vim.lsp.get_clients({ name = "jdtls", bufnr = bufnr })
    if not clients or #clients == 0 then
        log.warn("jdtls not attached to buffer")
        vim.notify("[Java Classpath] jdtls not attached", vim.log.levels.WARN)
        return nil
    end

    local client = clients[1]
    if not client.initialized then
        log.warn("jdtls not initialized")
        vim.notify("[Java Classpath] jdtls not initialized", vim.log.levels.WARN)
        return nil
    end

    -- Get current file URI
    local file_path = vim.api.nvim_buf_get_name(bufnr)
    local file_uri = vim.uri_from_fname(file_path)

    -- Detect scope based on file path
    local scope = is_test_file(file_path) and "test" or "runtime"
    log.info("Detected scope:", scope, "for file:", file_path)

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

function M.get_classpath_for_main_method(bufnr)
    local classpath = M.get_classpath_for_main_method_table(bufnr)
    if classpath then
        return table.concat(classpath, ":")
    end
    return nil
end

-- Clear cache
function M.clear_cache()
    classpath_cache.classpath = nil
    classpath_cache.timestamp = 0
    jdtls_ready_cache.ready = false
    jdtls_ready_cache.timestamp = 0
    log.debug("Cleared classpath and jdtls ready caches")
end

return M