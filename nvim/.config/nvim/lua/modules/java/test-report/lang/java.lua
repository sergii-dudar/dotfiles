local java_util = require("utils.java.java-common")
local junit_xml = require("modules.java.test-report.junit-xml")
local log = require("utils.logging-util").new({
    name = "test-report-java",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type test_report.LangAdapter
local M = {
    group_separator = ".",
    diagnostic_source = "junit",
    trouble_source = "junit_diagnostics",
    display_name = "JUnit",
}

---@param dirs string[]
---@return table<string, test_report.TestResult>
function M.parse_results(dirs)
    local results = {}
    for _, dir in ipairs(dirs) do
        for _, filepath in ipairs(junit_xml.list_report_files(dir)) do
            for id, r in pairs(junit_xml.parse_file(filepath)) do
                results[id] = r
            end
        end
    end
    return results
end

---@param id string  Full test id "pkg.Class#method"
---@return test_report.IdDisplay
function M.id_to_display(id)
    local container_id, member = id:match("^(.+)#(.+)$")
    if not container_id then
        return { container = id, member = "", group = nil }
    end
    local group = container_id:match("^(.+)%.[^%.]+$")
    local container_name = container_id:match("([^%.]+)$") or container_id
    return { container = container_name, member = member, group = group }
end

-- project_root -> { "com/foo/Bar.java" -> "/abs/path/to/com/foo/Bar.java" }
local class_index_cache = {}
-- project_root -> { "com/foo/Bar.java" -> true }: suffixes looked up and NOT found since the
-- last index build. Prevents a rebuild walk on every run for a genuinely unknown class.
local class_index_misses = {}

local _test_query
local function test_query()
    if _test_query then
        return _test_query
    end
    _test_query = vim.treesitter.query.parse(
        "java",
        [[
        (class_declaration
          name: (identifier) @class.name
        ) @class.definition

        (method_declaration
          (modifiers
            [
              (marker_annotation
                name: (identifier) @annotation
                (#any-of? @annotation "Test" "ParameterizedTest" "TestFactory" "CartesianTest" "RepeatedTest" "TestTemplate")
              )
              (annotation
                name: (identifier) @annotation
                (#any-of? @annotation "Test" "ParameterizedTest" "TestFactory" "CartesianTest" "RepeatedTest" "TestTemplate")
              )
            ]
          )
          name: (identifier) @test.name
        ) @test.definition
    ]]
    )
    return _test_query
end

-- Directories never descended into when indexing sources. Build outputs (target/, build/,
-- bin/, out/) may contain full source copies (e.g. maven-release `target/checkout`) that
-- would shadow the real files; VCS/IDE/tooling dirs are pure noise. Pruning them is also
-- what makes the walk ~8x faster than `glob("**/*.java")` on a typical module.
local SKIP_DIRS = {
    target = true,
    build = true,
    bin = true,
    out = true,
    node_modules = true,
    [".git"] = true,
    [".idea"] = true,
    [".gradle"] = true,
    [".mvn"] = true,
}

---@param path string
---@return integer
local function path_depth(path)
    local _, n = path:gsub("/", "")
    return n
end

--- Walk `root` and report every *.java file (path relative to `root`, absolute path),
--- pruning SKIP_DIRS. Symlinked directories are followed once (cycle-safe via realpath).
---@param root string
---@param on_file fun(rel: string, abs: string)
---@param visited? table<string, boolean>
local function walk_java_files(root, on_file, visited)
    visited = visited or {}
    local real = vim.uv.fs_realpath(root) or root
    if visited[real] then
        return
    end
    visited[real] = true

    local ok, err = pcall(function()
        local iter = vim.fs.dir(root, {
            depth = math.huge,
            skip = function(dir)
                return not SKIP_DIRS[vim.fs.basename(dir)]
            end,
        })
        for name, ftype in iter do
            if ftype == "file" then
                if name:sub(-5) == ".java" then
                    on_file(name, root .. "/" .. name)
                end
            elseif ftype == "link" then
                local abs = root .. "/" .. name
                local st = vim.uv.fs_stat(abs)
                if st and st.type == "directory" then
                    if not SKIP_DIRS[vim.fs.basename(name)] then
                        walk_java_files(abs, function(rel, abs_file)
                            on_file(name .. "/" .. rel, abs_file)
                        end, visited)
                    end
                elseif st and st.type == "file" and name:sub(-5) == ".java" then
                    on_file(name, abs)
                end
            end
        end
    end)
    if not ok then
        log.error("class index walk failed for " .. root .. ": " .. tostring(err))
    end
end

--- One pruned filesystem walk per project root, indexed by Java source-layout suffix
--- ("com/foo/Bar.java"). Replaces N recursive globs with N hash lookups.
--- Covers Maven/Gradle conventions (src/main/java, src/test/java, src/integTest/java)
--- including multi-module layouts since the prefix can match anywhere in the path.
---@param project_root string
---@return table<string, string>
local function build_class_index(project_root)
    local t0 = vim.uv.hrtime()
    local index = {}
    walk_java_files(project_root, function(rel, abs)
        local suffix = ("/" .. rel):match("/src/[^/]+/java/(.+)$")
        if suffix then
            local current = index[suffix]
            -- Deterministic on duplicates (nested modules): prefer the shallowest path, then
            -- the lexicographically smallest, regardless of filesystem iteration order.
            if
                not current
                or path_depth(abs) < path_depth(current)
                or (path_depth(abs) == path_depth(current) and abs < current)
            then
                index[suffix] = abs
            end
        end
    end)
    class_index_cache[project_root] = index
    class_index_misses[project_root] = {}
    log.info(
        string.format(
            "[perf class_index] %s entries=%d build=%.1fms",
            project_root,
            vim.tbl_count(index),
            (vim.uv.hrtime() - t0) / 1e6
        )
    )
    return index
end

--- Resolve a source-layout suffix through the project's class index, building the index on
--- first use. A miss on a suffix not seen before triggers ONE rebuild: the index may predate
--- a test class created after the first run of the session.
---@param project_root string
---@param suffix string
---@return string|nil
local function index_lookup(project_root, suffix)
    local index = class_index_cache[project_root]
    if index then
        local hit = index[suffix]
        if hit then
            return hit
        end
        if class_index_misses[project_root][suffix] then
            return nil
        end
        log.info("class index miss for " .. suffix .. ", rebuilding index of " .. project_root)
    end
    index = build_class_index(project_root)
    local hit = index[suffix]
    if not hit then
        class_index_misses[project_root][suffix] = true
    end
    return hit
end

function M.clear_cache()
    class_index_cache = {}
    class_index_misses = {}
    _test_query = nil
end

--- Resolve a container_id (fully-qualified Java class name) to a source file path.
---@param classname string Fully-qualified Java class name (e.g., "com.example.MyTest")
---@param report_dir string Path to report directory (used to derive project root)
---@return string|nil
function M.id_to_file(classname, report_dir)
    -- Inner classes (Outer$Inner) live in the outer class file
    local outer_class = classname:match("^([^%$]+)") or classname
    local relative_path = outer_class:gsub("%.", "/") .. ".java"

    -- Accept both maven (target/junit-report) and gradle (build/junit-report).
    local project_root = report_dir:match("^(.+)/[^/]+/junit%-report$")
    if project_root then
        local hit = index_lookup(project_root, relative_path)
        if hit then
            return hit
        end
    end
    local fallback = java_util.java_class_to_proj_path(outer_class)
    if type(fallback) == "string" and fallback ~= "" then
        -- glob() without {list} joins several matches with newlines; take the first one.
        return vim.split(fallback, "\n", { plain = true })[1]
    end
    return nil
end

---@param file_path string
---@param opts? test_report.FindOpts
---@return table<string, number> method_name -> 0-indexed line number
---@return number|nil class_line 0-indexed line of class declaration
function M.find_test_positions(file_path, opts)
    local positions = {}
    local class_line
    local silent = not opts or opts.silent ~= false

    local bufnr = vim.fn.bufadd(file_path)
    if not vim.api.nvim_buf_is_loaded(bufnr) then
        if silent then
            -- Load content without firing FileType/BufRead autocmds — skips the
            -- JDTLS attach + highlight cascade. Buffer has no filetype until the
            -- rescue autocmd (or anything else) triggers detection.
            pcall(vim.cmd, "noautocmd call bufload(" .. bufnr .. ")")
        else
            vim.fn.bufload(bufnr)
        end
    end

    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        log.error("treesitter parser failed for bufnr=" .. bufnr .. " err=" .. tostring(parser))
        vim.notify("test-report: treesitter parser failed for " .. file_path, vim.log.levels.ERROR)
        return positions, class_line
    end

    local tree = parser:parse()[1]
    if not tree then
        return positions, class_line
    end

    local query = test_query()
    for id, node in query:iter_captures(tree:root(), bufnr) do
        local capture = query.captures[id]
        if capture == "class.name" then
            -- The file-level sign belongs on the OUTERMOST class: keep the first (topmost)
            -- declaration, not the last nested one the query happens to visit.
            local row = node:range()
            if class_line == nil or row < class_line then
                class_line = row
            end
        elseif capture == "test.name" then
            positions[vim.treesitter.get_node_text(node, bufnr)] = node:range()
        end
    end

    return positions, class_line
end

---@param classname string
---@param stacktrace string
---@return number|nil
function M.extract_error_line(classname, stacktrace)
    -- Single implementation shared with the XML parser (nested-class aware).
    return junit_xml._extract_error_line(classname, stacktrace)
end

---@return string
function M.get_test_report_dir()
    return java_util.get_build_layout(java_util.get_buffer_project_path()).report_dir
end

return M
