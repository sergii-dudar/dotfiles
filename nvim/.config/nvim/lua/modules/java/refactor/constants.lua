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
M.LEADING_BOUNDARY = "(^|[[:space:],;({}<>@=!?:&|+-])"
M.TRAILING_BOUNDARY = "([[:space:],;(){}\\.<>@:\\[]|$)"

-- Boundary that must precede a fully qualified name so `old.pkg` never matches inside `other.old.pkg`
-- (ERE has no lookbehind, so build_fqn_replace_expr double-passes to handle adjacent matches)
M.FQN_LEADING_BOUNDARY = "(^|[^[:alnum:]_.])"
-- Same for slash-separated paths: `/old/pkg/Foo` must match, `xold/pkg/Foo` must not
M.PATH_LEADING_BOUNDARY = "(^|[^[:alnum:]_])"

-- ripgrep base invocation. Build output / VCS / IDE directories directly under the search root are skipped even
-- when the project has no .gitignore; the globs are anchored so a Java package named `build` or `target` is kept.
M.rg = "rg --color=never --no-messages -g '!/target' -g '!/build' -g '!/.git' -g '!/.idea' -g '!/node_modules'"

--- `rg -l` under `search_root`, printing absolute paths. Runs from inside the root because ripgrep matches
--- `-g` globs relative to the working directory: with an absolute root argument the anchored excludes above
--- would silently match nothing.
---@param pattern string regex (unescaped; shell escaping is done here)
---@param search_root string absolute directory
---@return string command Prints one absolute path per line; never fails (no match → no output)
function M.rg_list_cmd(pattern, search_root)
    local root = M.shell_escape(search_root)
    return string.format(
        "( cd %s && %s -l -- %s | while IFS= read -r f; do printf '%%s/%%s\\n' %s \"$f\"; done ) || true",
        root,
        M.rg,
        M.shell_escape(pattern),
        root
    )
end

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

--- Build a double-pass sed substitution for a fully qualified name (package or type):
--- `(^|<not alnum/_/.>)OLD(trailing)` → `\1NEW\2`, so `other.old.pkg` is never touched.
---@param old_escaped string sed-escaped old FQN (dots as `\.`, or slashes as `\/` for paths)
---@param new_value string replacement (slashes must already be escaped for paths)
---@param trailing_boundary string ERE group that must follow the FQN
---@param leading_boundary? string defaults to FQN_LEADING_BOUNDARY (use PATH_LEADING_BOUNDARY for paths)
---@return string sed_expression
function M.build_fqn_replace_expr(old_escaped, new_value, trailing_boundary, leading_boundary)
    local single_pass = string.format(
        "s/%s%s%s/\\1%s\\2/g",
        leading_boundary or M.FQN_LEADING_BOUNDARY,
        old_escaped,
        trailing_boundary,
        new_value
    )
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

--- Run a shell command and return its stdout (nil when the process could not be started).
---@param cmd string
---@return string|nil
function M.exec_and_read(cmd)
    local handle = io.popen(cmd)
    if not handle then
        M.log.error("Failed to execute command:", cmd)
        return nil
    end
    local result = handle:read("*all")
    handle:close()
    return result
end

--- `rg -q` a file; returns true when the pattern matches.
---@param file_path string
---@param rg_args string already shell-escaped pattern (and options)
---@return boolean
local function rg_matches(file_path, rg_args)
    local result = os.execute(string.format("rg -q %s %s 2>/dev/null", rg_args, M.shell_escape(file_path)))
    return result == 0 or result == true
end

--- Line after which a new import should be inserted: the last `import`, else the `package` line, else 0 (top).
---@param file_path string
---@return integer
function M.find_import_anchor(file_path)
    local escaped = M.shell_escape(file_path)
    local last_import =
        M.exec_and_read(string.format("rg -n '^import ' %s 2>/dev/null | tail -n 1 | cut -d: -f1", escaped))
    local line = tonumber(last_import)
    if line then
        return line
    end
    local package_line = M.exec_and_read(string.format("rg -n -m1 '^package ' %s 2>/dev/null | cut -d: -f1", escaped))
    return tonumber(package_line) or 0
end

--- Add an import to a Java file after its last import (or after the package line when it has none).
--- Skips when the exact import already exists, and refuses when another import already brings in a type with
--- the same simple name (the result would be a conflicting import; the user must resolve that by hand).
---@param file_path string
---@param import_line string e.g. "import com.acme.Foo;"
---@return boolean success
function M.add_import_line(file_path, import_line)
    -- Exact-line check (-F fixed string, -x whole line: no regex escaping needed)
    if rg_matches(file_path, "-F -x -- " .. M.shell_escape(import_line)) then
        M.log.debug("Import already exists, skipping:", import_line)
        return true
    end

    local simple_name = import_line:match("%.([%w_$]+);$")
    if simple_name and rg_matches(file_path, M.shell_escape("^import\\s+([\\w.]+\\.)?" .. simple_name .. ";")) then
        M.log.warn(
            "Not adding",
            import_line,
            "- a different type with the same simple name is already imported in",
            file_path
        )
        return false
    end

    local anchor = M.find_import_anchor(file_path)
    local sed_cmd
    if anchor > 0 then
        -- GNU sed append command with literal newline
        sed_cmd = string.format("%s -i '%da\\\n%s' %s", M.sed, anchor, import_line, M.shell_escape(file_path))
    else
        sed_cmd = string.format("%s -i '1i\\\n%s' %s", M.sed, import_line, M.shell_escape(file_path))
    end
    M.log.debug("Sed command:", sed_cmd)
    local result = os.execute(sed_cmd)
    if not (result == 0 or result == true) then
        M.log.warn("Failed to add import:", import_line, "to", file_path)
        return false
    end

    -- Verify the import was actually added
    if not rg_matches(file_path, "-F -x -- " .. M.shell_escape(import_line)) then
        M.log.warn("Import not found after adding:", import_line, "to", file_path)
        return false
    end
    M.log.debug("Import successfully added (verified):", import_line)
    return true
end

return M
