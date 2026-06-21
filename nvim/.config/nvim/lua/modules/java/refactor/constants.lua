-- Shared constants and shell utilities for the Java refactor module.

local M = {}

local string_util = require("utils.string-util")
local logging = require("utils.logging-util")

-- Create logger for java refactoring
M.log = logging.new({ name = "java-refactor", filename = "java-refactor.log" })

-- Package source roots
M.main_dir = "src/main/java/"
M.test_dir = "src/test/java/"
M.main_resource_dir = "src/main/resources/"
M.test_resource_dir = "src/test/resources/"
M.package_roots = { M.main_dir, M.test_dir, M.main_resource_dir, M.test_resource_dir }

-- OS-specific shell commands
M.is_macos = vim.loop.os_uname().sysname == "Darwin"
M.sed = M.is_macos and "gsed" or "sed"
-- macOS xargs doesn't need -r (default behavior), Linux GNU xargs needs -r to skip empty input
M.xargs = M.is_macos and "xargs" or "xargs -r"

-- Sed boundary patterns for matching Java type names.
-- Leading boundary: characters that can precede a type name (includes @ for annotations, ^ for line start)
-- Trailing boundary: characters that can follow a type name
M.LEADING_BOUNDARY = "(^|[[:space:],;({}<@])"
M.TRAILING_BOUNDARY = "([[:space:],;(){}\\.<>@:\\[]|$)"

--- Helper to escape single quotes in paths for safe shell interpolation
---@param s string
---@return string
function M.shell_escape(s)
    return "'" .. s:gsub("'", "'\\''") .. "'"
end

--- Build a double-pass sed substitution command for type symbol replacement.
--- Double-pass is needed because sed's global flag doesn't handle overlapping matches:
--- e.g., "Map<OldType, OldType>" - the comma consumed by first match is lost for second match.
---@param old_name string
---@param new_name string
---@return string sed_expression
function M.build_type_replace_expr(old_name, new_name)
    local single_pass =
        string.format("s/%s%s%s/\\1%s\\2/g", M.LEADING_BOUNDARY, old_name, M.TRAILING_BOUNDARY, new_name)
    return single_pass .. "; " .. single_pass
end

--- Get project root dynamically (not at module load time).
--- This allows tests to change directory before using the module.
---@return string
function M.get_project_root()
    return vim.fn.getcwd()
end

--- Detect module path from a file path.
--- Returns the module root directory (where pom.xml/build.gradle/build.gradle.kts exists)
--- or the path up to /src/*/java if no build file is found.
---@param file_path string
---@return string|nil
function M.detect_module_path(file_path)
    M.log.debug("Detecting module path for:", file_path)

    -- First, try to find the module by looking for build files
    local current_dir = file_path:match("(.+)/[^/]+$") -- Start from file's parent directory

    while current_dir and current_dir ~= "/" do
        -- Check for Maven/Gradle build files
        if
            vim.fn.filereadable(current_dir .. "/pom.xml") == 1
            or vim.fn.filereadable(current_dir .. "/build.gradle") == 1
            or vim.fn.filereadable(current_dir .. "/build.gradle.kts") == 1
        then
            M.log.info("Detected module path via build file:", current_dir)
            return current_dir
        end

        -- Move up one directory
        current_dir = current_dir:match("(.+)/[^/]+$")
    end

    -- Fallback: use path up to /src/*/java
    local module_path = file_path:match("(.+)/src/[^/]+/java/")
    if module_path then
        M.log.info("Detected module path via src directory:", module_path)
        return module_path
    end

    M.log.warn("Could not detect module path for:", file_path)
    return nil
end

--- Extract the package-relative path from a full path given a root.
--- E.g., for "/project/src/main/java/com/example/Foo.java" with root "src/main/java/",
--- returns "com/example/Foo.java"
---@param full_path string
---@param root string
---@return string|nil
function M.get_package_path(full_path, root)
    if not string_util.contains(full_path, root) then
        return nil
    end
    return vim.split(full_path, root)[2]
end

return M
