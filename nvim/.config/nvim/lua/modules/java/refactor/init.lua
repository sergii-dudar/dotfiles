-- INFO:: Module to batch fixing moved java file/files with external plugins, like: [ neo-tree, nvim-tree, oil.nvim, Snacks.rename ], for batch - [ fyler.nvim ]
--  currently module developed mostly for batch processing by `fyler.nvim`, for signle file change per action use method - process_single_file_change.
--  THIS MODULE IS NOT FINISHED YET, AND IN VERY EARLY DRAFT VERSION, BUT I'M USING IT HAVILY IN DAILY WORK AS JAVA DEV!

-- TODO:
--  󰱒 Batch move processing of java files from dir A to dir B with proper types usage resolving.
--  󰱒 Batch move processing of java files from dir A to dir B,C,D... with proper types usage resolving.

-- NOTE: - Dependencies: ripgrep, fd (rust-based find), sed (gsed in case macos (gnu-sed))

local M = {}

local util = require("utils.common-util")
local string_util = require("utils.string-util")
local spinner = require("utils.ui.spinner")
local list_util = require("utils.list-util")
local buffer_util = require("utils.buffer-util")
local logging = require("utils.logging-util")
local global = require("utils.global-util")
local import_fixer = require("modules.java.refactor.import-fixer")
local sibling_usage_fixer = require("modules.java.refactor.sibling-usage-fixer")

-- Create logger for java refactoring
local log = logging.new({ name = "java-refactor", filename = "java-refactor.log" })

-- Test mode: when true, executes commands directly without UI
M.test_mode = false

---@class java.rejactor.FileMove
---@field src string
---@field dst string
---@field siblings? java.rejactor.FileMove[]

local current_term_win = nil
---@param cmd_args string
local function run_cmd(cmd_args, on_success_callback)
    log.info("Starting Java refactoring command execution")
    log.debug("Command:", cmd_args)
    -- vim.notify("🚀 Java Refactoring Started", vim.log.levels.INFO)
    spinner.start("🚀 " .. "Java Refactoring...")

    vim.cmd("botright split")
    -- vim.cmd("resize 15")

    util.close_window_if_exists(current_term_win) -- close prev term win if opened
    current_term_win = vim.api.nvim_get_current_win()
    local term_buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_win_set_buf(current_term_win, term_buf)

    local job_id = vim.fn.jobstart(cmd_args, {
        term = true,
        stdout_buffered = false,
        stderr_buffered = false,
        on_exit = function(_, code)
            if code == 0 then
                log.info("Java refactoring completed successfully")
            else
                log.error("Java refactoring failed with exit code:", code)
            end
            spinner.stop(code == 0, code == 0 and "Java refactoring finished" or "Java refactoring failed")
            if code == 0 then
                util.close_window_if_exists(current_term_win)
                -- Call success callback if provided
                if on_success_callback then
                    vim.schedule(function()
                        on_success_callback()
                    end)
                end
            end
        end,
    })

    -- vim.cmd("startinsert")
    if job_id <= 0 then
        log.error("Failed to start command via jobstart()")
        vim.notify("Failed to start cmd via jobstart()", vim.log.levels.ERROR)
    else
        log.debug("Command started with job_id:", job_id)
    end
end

local main_dir = "src/main/java/"
local test_dir = "src/test/java/"
local main_resource_dir = "src/main/resources/"
local test_resource_dir = "src/test/resources/"
local package_roots = { main_dir, test_dir, main_resource_dir, test_resource_dir }
-- local test_path = "."
-- local test_path = "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem" -- TODO: change to . after finish
-- IMPORTANT: Get project root dynamically, not at module load time
-- This allows tests to change directory before using the module
local function get_project_root()
    return vim.fn.getcwd()
end

-- Detect module path from a file path
-- Returns the module root directory (where pom.xml/build.gradle/build.gradle.kts exists)
-- or the path up to /src/*/java if no build file is found
---@param file_path string
---@return string|nil
local function detect_module_path(file_path)
    log.debug("Detecting module path for:", file_path)

    -- First, try to find the module by looking for build files
    local current_dir = file_path:match("(.+)/[^/]+$") -- Start from file's parent directory

    while current_dir and current_dir ~= "/" do
        -- Check for Maven/Gradle build files
        if
            vim.fn.filereadable(current_dir .. "/pom.xml") == 1
            or vim.fn.filereadable(current_dir .. "/build.gradle") == 1
            or vim.fn.filereadable(current_dir .. "/build.gradle.kts") == 1
        then
            log.info("Detected module path via build file:", current_dir)
            return current_dir
        end

        -- Move up one directory
        current_dir = current_dir:match("(.+)/[^/]+$")
    end

    -- Fallback: use path up to /src/*/java
    local module_path = file_path:match("(.+)/src/[^/]+/java/")
    if module_path then
        log.info("Detected module path via src directory:", module_path)
        return module_path
    end

    log.warn("Could not detect module path for:", file_path)
    return nil
end

-- Use GNU sed on both platforms for consistent behavior
-- macOS: gsed (installed via brew install gnu-sed)
-- Linux: sed (already GNU sed)
local is_macos = vim.loop.os_uname().sysname == "Darwin"
local sed = is_macos and "gsed" or "sed"
-- macOS xargs doesn't need -r (default behavior), Linux GNU xargs needs -r to skip empty input
local xargs = is_macos and "xargs" or "xargs -r"
log.debug("Detected OS:", vim.loop.os_uname().sysname, "- using sed command:", sed, "xargs:", xargs)

-- Helper to escape single quotes in paths for safe shell interpolation
local function shell_escape(s)
    return "'" .. s:gsub("'", "'\\''") .. "'"
end

-- Sed boundary patterns for matching Java type names.
-- Leading boundary: characters that can precede a type name (includes @ for annotations, ^ for line start)
-- Trailing boundary: characters that can follow a type name
local LEADING_BOUNDARY = "(^|[[:space:],;(}<@])"
local TRAILING_BOUNDARY = "([[:space:],;(}\\.>@])"

-- Build a double-pass sed substitution command for type symbol replacement.
-- Double-pass is needed because sed's global flag doesn't handle overlapping matches:
-- e.g., "Map<OldType, OldType>" - the comma consumed by first match is lost for second match.
---@param old_name string
---@param new_name string
---@return string sed_expression
local function build_type_replace_expr(old_name, new_name)
    local single_pass = string.format("s/%s%s%s/\\1%s\\2/g", LEADING_BOUNDARY, old_name, TRAILING_BOUNDARY, new_name)
    return single_pass .. "; " .. single_pass
end

---@class RefactorOperation
---@field type "shell"|"lua"
---@field command? string Shell command to execute
---@field fn? function Lua function to call
---@field description string Description for logging

---@param result_cmds table
---@param root string
---@param context java.rejactor.FileMove
---@param module_path string|nil The module path to limit operations to
local build_fix_java_file_after_change_cmds = function(result_cmds, root, context, module_path)
    log.debug("Building fix commands for file move:", context.src, "->", context.dst)
    log.debug("Module path restriction:", module_path or "none (project-wide)")

    local src = context.src
    local dst = context.dst

    -- Determine the search root: module path if available, otherwise project root
    local search_root = module_path or get_project_root()

    -- com/example/EmployeeManagementSystem/service/ServiceEmployee
    local package_src_path = vim.split(src, root)[2]:gsub("%.java", "")
    local package_dst_path = vim.split(dst, root)[2]:gsub("%.java", "")

    -- com.example.EmployeeManagementSystem.service.ServiceEmployee
    local package_src_classpath = package_src_path:gsub("/", ".")
    local package_dst_classpath = package_dst_path:gsub("/", ".")

    -- com\.example\.EmployeeManagementSystem\.service\.ServiceEmployee
    local package_src_classpath_escaped = package_src_classpath:gsub("%.", "\\.")

    -- com\/example\/EmployeeManagementSystem\/service\/ServiceEmployee
    local package_src_path_escaped = package_src_path:gsub("/", "\\/")
    local package_dst_path_escaped = package_dst_path:gsub("/", "\\/")

    -- ServiceEmployee
    local old_type_name = package_src_path:match("([^/]+)$")
    local new_type_name = package_dst_path:match("([^/]+)$")

    log.debug("Type rename:", old_type_name, "->", new_type_name)

    -- com.example.EmployeeManagementSystem.service
    local package_declaration_src = package_src_classpath:match("(.+)%.%w+$")
    local package_declaration_dst = package_dst_classpath:match("(.+)%.%w+$")

    if not package_declaration_src or not package_declaration_dst then
        log.error("Failed to extract package declarations")
        log.error("package_src_classpath:", package_src_classpath)
        log.error("package_dst_classpath:", package_dst_classpath)
        log.error("package_declaration_src:", package_declaration_src)
        log.error("package_declaration_dst:", package_declaration_dst)
        return {}
    end

    -- Detect if this is a simple file rename (same package) or a package move
    local is_same_package_rename = (package_declaration_src == package_declaration_dst)

    if is_same_package_rename then
        log.info("Detected same-package file rename:", old_type_name, "->", new_type_name)
    else
        log.debug("Package change:", package_declaration_src, "->", package_declaration_dst)
    end

    -- com\.example\.EmployeeManagementSystem\.service
    local package_declaration_src_escaped = package_declaration_src:gsub("%.", "\\.")

    -- java file rename fixes (priority is very important):
    -- ==========================================================================
    -- ==========================================================================
    -- 1. fix type decration in changed file.
    local fix_type_declaration_cmd = string.format(
        "%s -i -E 's/(class|interface|enum|record) %s[[:space:]]/\\1 %s /g' %s",
        sed,
        old_type_name,
        new_type_name,
        dst
    )
    -- vim.notify(fix_type_declaration_cmd)
    table.insert(result_cmds, {
        type = "shell",
        command = fix_type_declaration_cmd,
        description = "Fix type declaration: " .. old_type_name .. " -> " .. new_type_name,
    })

    -- 1.1. fix constructor names in the moved file (needed for cross-package move+rename)
    if old_type_name ~= new_type_name then
        local fix_constructor_cmd = string.format(
            "%s -i -E 's/(public|protected|private)([[:space:]]+)%s([[:space:]]*(\\(|\\{))/\\1\\2%s\\3/g' %s",
            sed,
            old_type_name,
            new_type_name,
            dst
        )
        table.insert(result_cmds, {
            type = "shell",
            command = fix_constructor_cmd,
            description = "Fix constructor: " .. old_type_name .. " -> " .. new_type_name,
        })
    end

    -- ==========================================================================
    -- ==========================================================================
    -- 2. fix type symbols (simple java name) where type is imported or accessible
    -- This needs to handle:
    -- 1. Explicit imports: import package.ClassName;
    -- 2. Wildcard imports: import package.*;
    -- 3. Same-package usage: files in the same package don't need imports

    if is_same_package_rename then
        -- For same-package renames, update all files in the same package
        -- and files that import this package (explicitly or with wildcard)
        local package_dir = dst:match("(.+)/[^/]+$") -- Get directory of the file

        -- Also include corresponding test directory if this is a main file
        local test_package_dir = package_dir:gsub("src/main/java", "src/test/java")
        local test_dir_clause = ""
        if test_package_dir ~= package_dir and vim.fn.isdirectory(test_package_dir) == 1 then
            test_dir_clause = string.format("; fd -e java . %s", shell_escape(test_package_dir))
            log.debug("Including test directory for same-package rename:", test_package_dir)
        end

        -- Also include corresponding main directory if this is a test file
        local main_package_dir = package_dir:gsub("src/test/java", "src/main/java")
        local main_dir_clause = ""
        if main_package_dir ~= package_dir and vim.fn.isdirectory(main_package_dir) == 1 then
            main_dir_clause = string.format("; fd -e java . %s", shell_escape(main_package_dir))
            log.debug("Including main directory for same-package rename:", main_package_dir)
        end

        local fix_type_symbols_same_package = string.format(
            "(rg --color=never -l 'import\\s+%s([;.]|\\*;)' "
                .. "%s"
                .. " || true; fd -e java . %s%s%s) | sort -u | %s %s -i -E '%s' || echo 'skipped'",
            package_declaration_src:gsub("%.", "\\."), -- Match package with explicit or wildcard import
            shell_escape(search_root), -- Use module path instead of project root
            shell_escape(package_dir), -- Include all files in same directory (main)
            test_dir_clause, -- Also include test directory if it exists
            main_dir_clause, -- Also include main directory if this is a test file
            xargs,
            sed,
            build_type_replace_expr(old_type_name, new_type_name)
        )
        table.insert(result_cmds, {
            type = "shell",
            command = fix_type_symbols_same_package,
            description = "Fix type symbols in same package and imported files",
        })
    else
        -- For package moves, search for explicit imports AND wildcard imports
        -- ALSO include files from the OLD package directory — they use the type without import
        -- (same-package access) and now need updating since the type moved away
        local old_package_dir = src:match("(.+)/[^/]+$") -- Directory where the file WAS

        -- Also include corresponding test/main directory of the old package
        local old_counterpart_dir = ""
        if old_package_dir then
            local counterpart
            if string.find(old_package_dir, "src/main/java/") then
                counterpart = old_package_dir:gsub("src/main/java/", "src/test/java/")
            elseif string.find(old_package_dir, "src/test/java/") then
                counterpart = old_package_dir:gsub("src/test/java/", "src/main/java/")
            end
            if counterpart and counterpart ~= old_package_dir and vim.fn.isdirectory(counterpart) == 1 then
                old_counterpart_dir = string.format("; fd -e java . %s", shell_escape(counterpart))
            end
        end

        local fix_type_symbols_where_imported = string.format(
            "(rg --color=never -l 'import\\s+%s([;.]|\\*;)' %s || true"
                .. "; fd -e java . %s%s"
                .. ") | sort -u | %s %s -i -E '%s' || echo 'skipped'",
            package_declaration_src:gsub("%.", "\\."), -- Match package with explicit or wildcard import
            shell_escape(search_root), -- Use module path instead of project root
            shell_escape(old_package_dir), -- Include all files from old package dir (same-package access)
            old_counterpart_dir, -- Include test/main counterpart of old dir
            xargs,
            sed,
            build_type_replace_expr(old_type_name, new_type_name)
        )
        table.insert(result_cmds, {
            type = "shell",
            command = fix_type_symbols_where_imported,
            description = "Fix type symbols where imported (explicit or wildcard)",
        })
    end

    -- ==========================================================================
    -- ==========================================================================
    -- 2.1. fix imports of siblings java files (in case moved to other packages)
    if context.siblings and not vim.tbl_isempty(context.siblings) then
        log.debug("Processing", #context.siblings, "sibling files")
        for _, sibling in ipairs(context.siblings) do
            -- com/example/EmployeeManagementSystem/service/ServiceEmployee
            local sibling_package_src_path = vim.split(sibling.src, root)[2]:gsub("%.java", "")
            local sibling_package_dst_path = vim.split(sibling.dst, root)[2]:gsub("%.java", "")

            -- com.example.EmployeeManagementSystem.service.ServiceEmployee
            local sibling_package_dst_classpath = sibling_package_dst_path:gsub("/", ".")

            -- ServiceEmployee
            local sibling_old_type_name = sibling_package_src_path:match("([^/]+)$")
            local sibling_new_type_name = sibling_package_dst_path:match("([^/]+)$")

            -- com.example.EmployeeManagementSystem.service
            local sibling_package_declaration_dst = sibling_package_dst_classpath:match("(.+)%.%w+$")

            -- Call Lua function directly!
            table.insert(result_cmds, {
                type = "lua",
                description = "Fix sibling usage: " .. sibling_old_type_name,
                fn = function()
                    return sibling_usage_fixer.fix_sibling_usage({
                        file_path = dst,
                        new_package = sibling_package_declaration_dst,
                        old_type_name = sibling_old_type_name,
                        new_type_name = sibling_new_type_name,
                        file_dst_package = package_declaration_dst,
                    })
                end,
            })
        end
    end

    -- ==========================================================================
    -- ==========================================================================
    -- 3. fix type full qualified names (across all files - java,yaml,properties etc)
    local fix_type_full_qualified_names = string.format(
        "rg --color=never -l '%s' %s | %s %s -i -E 's/%s([;.$\"[:space:]()><,@]|$)/%s\\1/g' || echo 'skipped'",
        package_src_classpath_escaped,
        shell_escape(search_root),
        xargs,
        sed,
        package_src_classpath_escaped,
        package_dst_classpath
    )
    table.insert(result_cmds, {
        type = "shell",
        command = fix_type_full_qualified_names,
        description = "Fix type full qualified names",
    })

    -- ==========================================================================
    -- ==========================================================================
    -- PACKAGE-LEVEL OPERATIONS: Only execute these if the package actually changed
    -- For same-package file renames, skip these operations
    if not is_same_package_rename then
        -- 4. fix packaged decration in changed file.
        local fix_package_declaration = string.format(
            "%s -i -E 's/package[[:space:]]+%s;/package %s;/g' %s",
            sed,
            package_declaration_src_escaped,
            package_declaration_dst,
            dst
        )
        -- vim.notify(fix_package_declaration)
        table.insert(result_cmds, {
            type = "shell",
            command = fix_package_declaration,
            description = "Fix package declaration",
        })

        -- ==========================================================================
        -- ==========================================================================
        -- 4.1. Remove imports from the same package (they're unnecessary)
        -- Only match direct class imports (uppercase after last dot, no more dots before semicolon)
        -- This avoids deleting subpackage imports like: import com.example.service.impl.SomeClass;
        local package_declaration_dst_escaped = package_declaration_dst:gsub("%.", "\\.")
        local remove_same_package_imports =
            string.format("%s -i '/^import %s\\.[A-Z][^.]*;$/d' %s", sed, package_declaration_dst_escaped, dst)
        table.insert(result_cmds, {
            type = "shell",
            command = remove_same_package_imports,
            description = "Remove same-package imports",
        })
    else
        log.debug("Skipping package declaration updates (same package)")
    end

    -- ==========================================================================
    -- ==========================================================================
    -- 5. add import declarations of the new class name to the classes of the old folder
    -- OLD_DIR="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service"
    -- OLD_PACKAGE="com.example.EmployeeManagementSystem.service"
    -- NEW_FILE_PATH="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"

    -- Only execute package-move specific operations if package actually changed
    if not is_same_package_rename then
        -- Build list of sibling type names (files being moved from same directory)
        local sibling_types = {}
        if context.siblings and not vim.tbl_isempty(context.siblings) then
            for _, sibling in ipairs(context.siblings) do
                local sibling_type = sibling.src:match("([^/]+)%.java$")
                if sibling_type then
                    table.insert(sibling_types, sibling_type)
                    log.debug("Sibling type being moved:", sibling_type)
                end
            end
        end

        -- Call Lua function directly!
        table.insert(result_cmds, {
            type = "lua",
            description = "Fix old package imports for: " .. old_type_name,
            fn = function()
                return import_fixer.fix_old_package_imports({
                    old_dir = src:match("(.+)/[^/]+$"),
                    old_package = package_declaration_src,
                    new_package = package_declaration_dst,
                    new_file_path = dst,
                    old_type_name = old_type_name,
                    new_type_name = new_type_name,
                    siblings = sibling_types,
                    module_path = module_path,
                })
            end,
        })
        -- ==========================================================================
        -- ==========================================================================
        -- 6. fix file path/resources path

        local fix_file_path_declaration = string.format(
            "rg --color=never -l '%s' %s | %s %s -i -E 's/%s([;.\"[:space:])><,]|$)/%s\\1/g' || echo 'skipped'",
            package_src_path_escaped,
            shell_escape(search_root),
            xargs,
            sed,
            package_src_path_escaped,
            package_dst_path_escaped
        )
        table.insert(result_cmds, {
            type = "shell",
            command = fix_file_path_declaration,
            description = "Fix file path/resources path",
        })
    else
        log.debug("Skipping old package import fixes (same package)")
    end
end

---@param result_cmds table
---@param root string
---@param context java.rejactor.FileMove
---@param module_path string|nil The module path to limit operations to
local build_fix_java_package_after_change_cmds = function(result_cmds, root, context, module_path)
    log.debug("Building fix commands for package move:", context.src, "->", context.dst)
    log.debug("Module path restriction:", module_path or "none (project-wide)")
    local src = context.src
    local dst = context.dst

    -- Determine the search root: module path if available, otherwise project root
    local search_root = module_path or get_project_root()

    -- com/example/EmployeeManagementSystem/service
    local package_src_path = vim.split(src, root)[2]
    local package_dst_path = vim.split(dst, root)[2]

    -- Remove trailing slashes if present
    package_src_path = package_src_path:gsub("/$", "")
    package_dst_path = package_dst_path:gsub("/$", "")

    -- com.example.EmployeeManagementSystem.service
    local package_src_classpath = package_src_path:gsub("/", ".")
    local package_dst_classpath = package_dst_path:gsub("/", ".")

    log.debug("Package rename (directory):", package_src_classpath, "->", package_dst_classpath)

    -- CRITICAL: For root package changes, we need to be very specific
    -- Don't do simple replacements like "com" -> "ua" which can break things
    -- Only do full package path replacements

    -- Minimum package depth check - avoid overly broad replacements
    local src_depth = select(2, package_src_classpath:gsub("%.", "")) + 1
    if src_depth < 2 then
        log.warn(
            "Skipping overly broad package replacement:",
            package_src_classpath,
            "-> only affects depth",
            src_depth
        )
        log.warn("This would be too dangerous. Please move more specific subdirectories instead.")
        return
    end

    -- com\.example\.EmployeeManagementSystem\.service
    local package_src_classpath_escaped = package_src_classpath:gsub("%.", "\\.")

    -- com\/example\/EmployeeManagementSystem\/service\/ServiceEmployee
    local package_src_path_escaped = package_src_path:gsub("/", "\\/")
    local package_dst_path_escaped = package_dst_path:gsub("/", "\\/")

    -- java package rename fixes (priority is very important):
    -- ==========================================================================
    -- ==========================================================================
    -- 1. fix package full qualified names (across all files - java,yaml,properties etc)
    -- IMPORTANT: For directory/package moves, we need to match ALL occurrences including subpackages
    -- Pattern must match:
    --   - package ua.raiffeisen.paymentinitiation.adapter; (package declaration)
    --   - package ua.raiffeisen.paymentinitiation.adapter.cisaod.listener; (subpackage)
    --   - import ua.raiffeisen.paymentinitiation.adapter.ClassName; (import)
    --   - import ua.raiffeisen.paymentinitiation.adapter.cisaod.ClassName; (import from subpackage)
    --   - "ua.raiffeisen.paymentinitiation.adapter" (string literal)
    --   - @com.example.Annotation (annotation with FQN)
    -- Solution: Match ([;$"[:space:].,()><@]|$) - includes dot for subpackages
    --   - The dot allows matching subpackages during directory moves
    --   - The \1 in replacement preserves whatever was captured
    local fix_package_full_qualified_names = string.format(
        "rg --color=never -l '%s' %s" .. " | %s %s -i -E 's/%s([;$\"[:space:].,()><@]|$)/%s\\1/g' || echo 'skipped'",
        package_src_classpath_escaped,
        shell_escape(search_root),
        xargs,
        sed,
        package_src_classpath_escaped,
        package_dst_classpath
    )
    log.debug("Package replacement command:", fix_package_full_qualified_names)
    table.insert(result_cmds, {
        type = "shell",
        command = fix_package_full_qualified_names,
        description = "Fix package full qualified names: " .. package_src_classpath .. " -> " .. package_dst_classpath,
    })

    -- ==========================================================================
    -- ==========================================================================
    -- 2. fix package path/resources path
    local fix_file_path_declaration = string.format(
        "rg --color=never -l '%s' %s | %s %s -i -E 's/%s([;.\"\\/:space:]|$)/%s\\1/g' || echo 'skipped'",
        package_src_path_escaped,
        shell_escape(search_root),
        xargs,
        sed,
        package_src_path_escaped,
        package_dst_path_escaped
    )
    table.insert(result_cmds, {
        type = "shell",
        command = fix_file_path_declaration,
        description = "Fix package path/resources path",
    })
end

---@param context java.rejactor.FileMove
---@param module_path string|nil The module path to limit operations to
---@return table
local build_fix_java_proj_after_change_cmds = function(context, module_path)
    local result_cmds = {}
    local is_dir = util.is_dir(context.dst)
    local is_file = util.is_file(context.dst)
    for _, root in ipairs(package_roots) do
        -- TODO: after applying on relative path, rg search need apply ". root" to take in account test, main, resourses
        if string_util.contains(context.src, root) and string_util.contains(context.dst, root) then
            if is_file then
                -- Buffer management is now handled at batch level in process_registerd_changes()
                -- See lines 454-499 (tracking) and 1032-1087 (delete/reopen)
                build_fix_java_file_after_change_cmds(result_cmds, root, context, module_path)
            elseif is_dir then
                build_fix_java_package_after_change_cmds(result_cmds, root, context, module_path)
            end
        end
    end
    return result_cmds
end

--- Fix java project after remaning java file, or package name
---@param context java.rejactor.FileMove
---@param module_path string|nil The module path to limit operations to
---@return RefactorOperation[]|nil
local build_fix_java_proj_after_change_cmd = function(context, module_path)
    if not context.src:match("src/.*/java/") then
        return nil
    end
    if not context.dst:match("src/.*/java/") then
        return nil
    end
    local operations = build_fix_java_proj_after_change_cmds(context, module_path)
    return operations
end

local all_registered_changes = {}

---@param src string
---@param dst string
function M.register_change(src, dst)
    log.debug("Registering change:", src, "->", dst)
    table.insert(all_registered_changes, {
        src = src,
        dst = dst,
    })
end

---@param context java.rejactor.FileMove
---@param all_changes java.rejactor.FileMove[]
---@return java.rejactor.FileMove[]
local get_all_src_siblings = function(context, all_changes)
    local context_src_dir = context.src:match("(.+)/[^/]+$")
    -- print(context_src_dir)
    local context_src_siblings = {}
    for _, value in ipairs(all_changes) do
        local current_src_dir = value.src:match("(.+)/[^/]+$")
        if context_src_dir == current_src_dir and context.dst ~= value.dst then
            -- print(current_src_dir)
            table.insert(context_src_siblings, value)
        end
    end
    return context_src_siblings
end

function M.process_registerd_changes()
    if vim.tbl_isempty(all_registered_changes) then
        log.warn("No registered changes to process")
        if not M.test_mode then
            vim.notify("No any registered changes")
        end
        return nil
    end

    log.info("Starting processing of", #all_registered_changes, "registered changes")
    log.debug("All registered changes:", all_registered_changes)

    -- CRITICAL: Detect module path from the first registered change
    -- This ensures all refactoring operations are limited to this module only
    local module_path = nil
    if all_registered_changes[1] then
        local first_change_path = all_registered_changes[1].src
        module_path = detect_module_path(first_change_path)

        if module_path then
            log.info("==============================================")
            log.info("DETECTED MODULE SCOPE:", module_path)
            log.info("All refactoring operations will be limited to this module only")
            log.info("==============================================")
        else
            log.warn("Could not detect module path, operations will be project-wide")
        end
    end

    -- ENHANCEMENT: Track opened buffers before changes
    -- Save list of currently opened Java file buffers that will be moved
    -- After changes, we'll reopen them from new locations
    local opened_buffers_to_reopen = {}
    log.info("Tracking opened buffers before applying changes...")

    for _, change in ipairs(all_registered_changes) do
        -- Check if this is a directory move - need to find all files under it
        if vim.fn.isdirectory(change.src) == 1 then
            -- Find all Java files in this directory
            log.debug("Scanning directory for open buffers:", change.src)
            local handle = io.popen("fd -e java . " .. shell_escape(change.src))
            if handle then
                local file_count = 0
                for file_path in handle:lines() do
                    file_count = file_count + 1
                    log.debug("Checking file:", file_path)
                    local buf_id = buffer_util.find_buf_by_path(file_path)
                    if buf_id then
                        -- Calculate new path for this file
                        local new_path = file_path:gsub("^" .. vim.pesc(change.src), change.dst)
                        table.insert(opened_buffers_to_reopen, {
                            old_path = file_path,
                            new_path = new_path,
                            buf_id = buf_id,
                        })
                        log.info("Will reopen buffer:", file_path, "->", new_path)
                    else
                        log.debug("File not open in buffer:", file_path)
                    end
                end
                handle:close()
                log.debug("Scanned", file_count, "files in", change.src)
            end
        elseif change.src:match("%.java$") then
            -- Single file move
            log.debug("Checking single file:", change.src)
            local buf_id = buffer_util.find_buf_by_path(change.src)
            if buf_id then
                table.insert(opened_buffers_to_reopen, {
                    old_path = change.src,
                    new_path = change.dst,
                    buf_id = buf_id,
                })
                log.info("Will reopen buffer:", change.src, "->", change.dst)
            else
                log.debug("File not open in buffer:", change.src)
            end
        end
    end

    if #opened_buffers_to_reopen > 0 then
        log.info("Found", #opened_buffers_to_reopen, "opened buffers to reopen after changes")
    else
        log.info("No opened buffers found for files being moved")
    end

    -- ENHANCEMENT: Auto-mirror test packages
    -- When main package is moved, automatically move corresponding test package
    -- This includes PHYSICALLY MOVING the test files/directories
    local test_mirrors = {}
    local test_mirror_dirs = {} -- Track unique test directories to mirror

    -- CRITICAL: Correct destinations of directory changes using file-level changes as truth.
    -- File managers (fyler.nvim) may emit intermediate directory events with WRONG destinations
    -- (e.g., `payments → govern` instead of correct `payments → govern/test`).
    -- We use file-level moves to determine the canonical path transformation,
    -- then recompute correct destinations for all directory changes.
    local canonical_old_prefix = nil
    local canonical_new_prefix = nil
    local canonical_root = nil

    if #all_registered_changes > 1 then
        -- Step 1: Find the canonical transformation from file-level changes
        -- Compare a file's old dir vs new dir to find what prefix changed

        for _, change in ipairs(all_registered_changes) do
            if change.src:match("%.java$") then
                -- Find matching root
                local root_match = nil
                for _, root in ipairs(package_roots) do
                    if string_util.contains(change.src, root) and string_util.contains(change.dst, root) then
                        root_match = root
                        break
                    end
                end
                if root_match then
                    -- Strip root and filename to get directory parts
                    local old_pkg = vim.split(change.src, root_match)[2]
                    local new_pkg = vim.split(change.dst, root_match)[2]
                    old_pkg = old_pkg:match("(.+)/[^/]+$") or ""
                    new_pkg = new_pkg:match("(.+)/[^/]+$") or ""

                    -- Find common suffix by comparing segments from the end
                    local old_segs = vim.split(old_pkg, "/")
                    local new_segs = vim.split(new_pkg, "/")
                    local common_suffix_count = 0
                    for k = 0, math.min(#old_segs, #new_segs) - 1 do
                        if old_segs[#old_segs - k] == new_segs[#new_segs - k] then
                            common_suffix_count = common_suffix_count + 1
                        else
                            break
                        end
                    end

                    -- The changed part is everything before the common suffix
                    local old_prefix_parts = {}
                    for k = 1, #old_segs - common_suffix_count do
                        table.insert(old_prefix_parts, old_segs[k])
                    end
                    local new_prefix_parts = {}
                    for k = 1, #new_segs - common_suffix_count do
                        table.insert(new_prefix_parts, new_segs[k])
                    end

                    local old_pref = table.concat(old_prefix_parts, "/")
                    local new_pref = table.concat(new_prefix_parts, "/")

                    if old_pref ~= "" and new_pref ~= "" then
                        canonical_old_prefix = old_pref
                        canonical_new_prefix = new_pref
                        canonical_root = root_match
                        log.info("Canonical transformation detected:", canonical_old_prefix, "->", canonical_new_prefix)
                        break
                    end
                end
            end
        end

        -- Step 2: Use canonical transformation to correct directory change destinations
        if canonical_old_prefix then
            for _, change in ipairs(all_registered_changes) do
                if not change.src:match("%.java$") then
                    -- This is a directory change — verify and correct its destination
                    local root_match = nil
                    for _, root in ipairs(package_roots) do
                        if string_util.contains(change.src, root) then
                            root_match = root
                            break
                        end
                    end
                    if root_match then
                        local src_pkg = vim.split(change.src, root_match)[2]
                        if src_pkg then
                            src_pkg = src_pkg:gsub("/$", "")
                            -- Check if this directory starts with the old prefix
                            if
                                src_pkg == canonical_old_prefix
                                or src_pkg:find("^" .. vim.pesc(canonical_old_prefix) .. "/")
                            then
                                -- Compute correct destination by replacing old prefix with new
                                local suffix = ""
                                if #src_pkg > #canonical_old_prefix then
                                    suffix = src_pkg:sub(#canonical_old_prefix + 1)
                                end
                                local correct_dst_pkg = canonical_new_prefix .. suffix
                                local base_path = vim.split(change.src, root_match)[1]
                                local correct_dst = base_path .. root_match .. correct_dst_pkg

                                if change.dst ~= correct_dst then
                                    log.info(
                                        "Correcting directory change destination:",
                                        change.dst,
                                        "->",
                                        correct_dst,
                                        "(src:",
                                        change.src,
                                        ")"
                                    )
                                    change.dst = correct_dst
                                end
                            end
                        end
                    end
                end
            end
        end
    end

    -- First, detect if we're doing a structural refactoring (e.g., adapter/* -> adapter/code/*)
    -- In this case, we need to move ALL test subdirectories, not just matching ones
    local structural_refactorings = {} -- { src_parent -> dst_parent }

    -- Detect structural patterns: multiple directories moving from X/* to X/Y/*
    local dir_moves = {}
    for _, change in ipairs(all_registered_changes) do
        -- Detect structural moves in BOTH main and test trees
        if
            (string_util.contains(change.src, "src/main/java/") or string_util.contains(change.src, "src/test/java/"))
            and not change.src:match("%.java$")
        then
            -- Extract parent and subdirectory
            local src_parent = change.src:match("(.+)/[^/]+$")
            local dst_parent = change.dst:match("(.+)/[^/]+$")

            if src_parent and dst_parent and src_parent ~= dst_parent then
                if not dir_moves[src_parent] then
                    dir_moves[src_parent] = {}
                end
                table.insert(dir_moves[src_parent], { src_parent = src_parent, dst_parent = dst_parent })
            end
        end
    end

    -- If we have multiple directories moving from the same parent, it's a structural refactoring
    for src_parent, moves in pairs(dir_moves) do
        if #moves >= 2 then
            -- Check if all moves have the same destination parent
            local common_dst_parent = moves[1].dst_parent
            local all_same = true
            for _, move in ipairs(moves) do
                if move.dst_parent ~= common_dst_parent then
                    all_same = false
                    break
                end
            end

            if all_same then
                structural_refactorings[src_parent] = common_dst_parent
                log.info("Detected structural refactoring:", src_parent, "->", common_dst_parent)
            end
        end
    end

    -- For structural refactorings, find ALL counterpart subdirectories and mirror them
    -- Supports both src/main/java -> src/test/java AND src/test/java -> src/main/java
    for src_parent, dst_parent in pairs(structural_refactorings) do
        local mirror_src_parent, mirror_dst_parent
        if string_util.contains(src_parent, "src/main/java/") then
            mirror_src_parent = src_parent:gsub("src/main/java/", "src/test/java/")
            mirror_dst_parent = dst_parent:gsub("src/main/java/", "src/test/java/")
        elseif string_util.contains(src_parent, "src/test/java/") then
            mirror_src_parent = src_parent:gsub("src/test/java/", "src/main/java/")
            mirror_dst_parent = dst_parent:gsub("src/test/java/", "src/main/java/")
        else
            goto continue_structural
        end

        log.debug("Checking for counterpart subdirectories in:", mirror_src_parent)
        if vim.fn.isdirectory(mirror_src_parent) == 1 then
            -- Find ALL subdirectories in the counterpart parent
            local fd_cmd = "fd --max-depth 1 --type d . " .. shell_escape(mirror_src_parent)
            log.debug("Running fd command:", fd_cmd)
            local handle = io.popen(fd_cmd)
            if handle then
                local found_count = 0
                for subdir in handle:lines() do
                    -- Remove trailing slash if present
                    subdir = subdir:gsub("/$", "")
                    local subdir_name = subdir:match(".+/([^/]+)$")
                    log.debug("Found counterpart subdirectory:", subdir, "name:", subdir_name)

                    -- Skip the destination directory itself (e.g., adapter/code)
                    if subdir_name and not subdir:match("/" .. vim.pesc(dst_parent:match(".+/([^/]+)$")) .. "$") then
                        local dst_subdir = mirror_dst_parent .. "/" .. subdir_name

                        if not test_mirror_dirs[subdir] then
                            test_mirror_dirs[subdir] = dst_subdir
                            table.insert(test_mirrors, { src = subdir, dst = dst_subdir })
                            log.info("Auto-mirroring counterpart subdirectory (structural):", subdir, "->", dst_subdir)
                            found_count = found_count + 1
                        else
                            log.debug("Subdirectory already in mirror list, skipping:", subdir)
                        end
                    else
                        log.debug("Skipping destination directory:", subdir)
                    end
                end
                handle:close()
                log.info("Found", found_count, "counterpart subdirectories for structural refactoring")
            else
                log.error("Failed to execute fd command")
            end
        else
            log.info("Counterpart source parent does not exist:", mirror_src_parent)
        end
        ::continue_structural::
    end

    -- Also handle individual directory/file moves (existing logic)
    -- Supports BOTH directions: src/main/java <-> src/test/java
    for _, change in ipairs(all_registered_changes) do
        -- CRITICAL: Skip directory changes that are PARENTS of the canonical old prefix.
        -- These represent intermediate/partial filesystem events (e.g., `raiffeisen → obama`)
        -- when the actual full transformation is deeper (e.g., `raiffeisen/payments → obama/govern/test/other`).
        -- Mirroring a parent would only partially rename, leaving inner segments un-transformed.
        if canonical_old_prefix and not change.src:match("%.java$") then
            local root_match = nil
            for _, root in ipairs(package_roots) do
                if string_util.contains(change.src, root) then
                    root_match = root
                    break
                end
            end
            if root_match then
                local src_pkg = vim.split(change.src, root_match)[2]
                if src_pkg then
                    src_pkg = src_pkg:gsub("/$", "")
                    -- Skip if this directory's package path is a STRICT parent of the canonical prefix
                    if
                        src_pkg ~= canonical_old_prefix
                        and canonical_old_prefix:find("^" .. vim.pesc(src_pkg) .. "/")
                    then
                        log.info(
                            "Skipping parent-of-canonical directory change (partial rename):",
                            change.src,
                            "->",
                            change.dst
                        )
                        goto continue_mirror_loop
                    end
                end
            end
        end

        local mirror_src, mirror_dst

        if string_util.contains(change.src, "src/main/java/") then
            -- Main -> mirror to test
            mirror_src = change.src:gsub("src/main/java/", "src/test/java/")
            mirror_dst = change.dst:gsub("src/main/java/", "src/test/java/")
        elseif string_util.contains(change.src, "src/test/java/") then
            -- Test -> mirror to main
            mirror_src = change.src:gsub("src/test/java/", "src/main/java/")
            mirror_dst = change.dst:gsub("src/test/java/", "src/main/java/")
        end

        if mirror_src and mirror_dst then
            -- For directory moves: mirror the directory directly (if not already handled by structural refactoring)
            if vim.fn.isdirectory(mirror_src) == 1 then
                if not test_mirror_dirs[mirror_src] then
                    test_mirror_dirs[mirror_src] = mirror_dst
                    table.insert(test_mirrors, { src = mirror_src, dst = mirror_dst })
                    log.info("Auto-mirroring counterpart directory:", mirror_src, "->", mirror_dst)
                end
            -- For file moves: infer directory-level mirror
            elseif change.src:match("%.java$") then
                -- Extract source and destination directories
                local src_dir = change.src:match("(.+)/[^/]+$")
                local dst_dir = change.dst:match("(.+)/[^/]+$")

                -- Only mirror if directories actually differ (not a same-directory file rename)
                -- AND not a parent/child relationship (subdirectory move)
                if src_dir and dst_dir and src_dir ~= dst_dir then
                    -- Check if dst is a subdirectory of src or vice versa
                    local is_subdirectory_move = dst_dir:find("^" .. vim.pesc(src_dir) .. "/")
                        or src_dir:find("^" .. vim.pesc(dst_dir) .. "/")

                    if is_subdirectory_move then
                        -- For subdirectory moves (e.g., service/X.java -> service/impl/X.java),
                        -- we can't mirror the whole directory but we CAN mirror individual test/main files.
                        -- Check if the counterpart FILE exists and move it individually.
                        local mirror_file_src, mirror_file_dst
                        if string_util.contains(change.src, "src/main/java/") then
                            mirror_file_src = change.src:gsub("src/main/java/", "src/test/java/")
                            mirror_file_dst = change.dst:gsub("src/main/java/", "src/test/java/")
                        elseif string_util.contains(change.src, "src/test/java/") then
                            mirror_file_src = change.src:gsub("src/test/java/", "src/main/java/")
                            mirror_file_dst = change.dst:gsub("src/test/java/", "src/main/java/")
                        end

                        -- Mirror individual files that exist (e.g., UserServiceTest.java)
                        if mirror_file_src and vim.fn.filereadable(mirror_file_src) == 1 then
                            if not test_mirror_dirs[mirror_file_src] then
                                test_mirror_dirs[mirror_file_src] = mirror_file_dst
                                table.insert(test_mirrors, { src = mirror_file_src, dst = mirror_file_dst })
                                log.info(
                                    "Auto-mirroring counterpart file (subdirectory move):",
                                    mirror_file_src,
                                    "->",
                                    mirror_file_dst
                                )
                            end
                        else
                            log.debug("No counterpart file for subdirectory move:", mirror_file_src or "nil")
                        end
                    else
                        local mirror_src_dir, mirror_dst_dir
                        if string_util.contains(src_dir, "src/main/java/") then
                            mirror_src_dir = src_dir:gsub("src/main/java/", "src/test/java/")
                            mirror_dst_dir = dst_dir:gsub("src/main/java/", "src/test/java/")
                        elseif string_util.contains(src_dir, "src/test/java/") then
                            mirror_src_dir = src_dir:gsub("src/test/java/", "src/main/java/")
                            mirror_dst_dir = dst_dir:gsub("src/test/java/", "src/main/java/")
                        end

                        -- Only add if counterpart directory exists and not already mirrored
                        if
                            mirror_src_dir
                            and vim.fn.isdirectory(mirror_src_dir) == 1
                            and not test_mirror_dirs[mirror_src_dir]
                        then
                            test_mirror_dirs[mirror_src_dir] = mirror_dst_dir
                            table.insert(test_mirrors, { src = mirror_src_dir, dst = mirror_dst_dir })
                            log.info(
                                "Auto-mirroring counterpart directory (inferred from file move):",
                                mirror_src_dir,
                                "->",
                                mirror_dst_dir
                            )
                        end
                    end
                else
                    log.debug("Skipping mirror for same-directory file rename")
                end
            end
        end
        ::continue_mirror_loop::
    end

    -- CRITICAL: Deduplicate mirrors — keep only the SHALLOWEST mirror per branch.
    -- After destination correction (Step 2 above), even shallow mirrors have correct destinations.
    -- The shallowest mirror's physical move covers ALL files underneath in one operation.
    -- Its package update sed also covers all subpackages (because `.` is in the boundary pattern).
    -- Keeping deeper mirrors would either fail physically (parent already moved) or be redundant.
    if #test_mirrors > 1 then
        -- Sort by source path length ascending (shallowest first)
        table.sort(test_mirrors, function(a, b)
            return #a.src < #b.src
        end)

        local filtered_mirrors = {}
        for _, mirror in ipairs(test_mirrors) do
            local is_covered_by_shallower = false
            for _, kept in ipairs(filtered_mirrors) do
                -- Check if this mirror's src starts with an already-kept (shallower) mirror's src
                if mirror.src:find("^" .. vim.pesc(kept.src) .. "/") then
                    is_covered_by_shallower = true
                    log.info(
                        "Removing redundant child mirror:",
                        mirror.src,
                        "->",
                        mirror.dst,
                        "(covered by shallower:",
                        kept.src,
                        ")"
                    )
                    break
                end
            end
            if not is_covered_by_shallower then
                table.insert(filtered_mirrors, mirror)
            end
        end
        test_mirrors = filtered_mirrors
        -- Reset mirror_dirs tracking to match filtered mirrors
        test_mirror_dirs = {}
        for _, mirror in ipairs(test_mirrors) do
            test_mirror_dirs[mirror.src] = mirror.dst
        end
    end

    -- Physically move counterpart files/directories to match package structure
    if #test_mirrors > 0 then
        log.info("Physically moving", #test_mirrors, "counterpart packages to match structure")

        -- Track opened counterpart buffers before moving
        for _, mirror in ipairs(test_mirrors) do
            if vim.fn.isdirectory(mirror.src) == 1 then
                -- Find all Java files in this directory
                local handle = io.popen("fd -e java . " .. shell_escape(mirror.src))
                if handle then
                    for file_path in handle:lines() do
                        local buf_id = buffer_util.find_buf_by_path(file_path)
                        if buf_id then
                            -- Calculate new path for this file
                            local new_path = file_path:gsub("^" .. vim.pesc(mirror.src), mirror.dst)
                            table.insert(opened_buffers_to_reopen, {
                                old_path = file_path,
                                new_path = new_path,
                                buf_id = buf_id,
                            })
                            log.info("Will reopen buffer:", file_path, "->", new_path)
                        end
                    end
                    handle:close()
                end
            elseif mirror.src:match("%.java$") then
                -- Single file
                local buf_id = buffer_util.find_buf_by_path(mirror.src)
                if buf_id then
                    table.insert(opened_buffers_to_reopen, {
                        old_path = mirror.src,
                        new_path = mirror.dst,
                        buf_id = buf_id,
                    })
                    log.info("Will reopen buffer:", mirror.src, "->", mirror.dst)
                end
            end
        end

        -- Perform physical moves
        for _, mirror in ipairs(test_mirrors) do
            -- Create destination directory
            local dst_parent = mirror.dst:match("(.+)/[^/]+$")
            if dst_parent then
                vim.fn.mkdir(dst_parent, "p")
            end

            -- Move file or directory
            local is_file = vim.fn.filereadable(mirror.src) == 1
            local is_dir = vim.fn.isdirectory(mirror.src) == 1

            if is_file or is_dir then
                local move_result = os.rename(mirror.src, mirror.dst)
                if move_result then
                    log.info("Moved test:", mirror.src, "->", mirror.dst)
                else
                    -- Fallback to shell command for cross-device moves
                    local cmd = string.format("mv %s %s", shell_escape(mirror.src), shell_escape(mirror.dst))
                    local exit_code = os.execute(cmd)
                    if exit_code == 0 or exit_code == true then
                        log.info("Moved test (via shell):", mirror.src, "->", mirror.dst)
                    else
                        log.error("Failed to move test:", mirror.src, "->", mirror.dst)
                    end
                end
            end
        end

        -- Clean up ALL empty directories in the module's java source trees after mirroring.
        -- This catches not just the immediate mirror source parents, but any orphaned empty
        -- directories from this or previous operations (e.g., leftover from earlier renames).
        -- Uses `find -type d -empty -delete` which works bottom-up (removes nested empties).
        log.info("Cleaning up empty directories after mirroring...")
        local cleanup_count = 0

        -- Determine which java source roots to clean
        local java_roots_to_clean = {}
        local base_module = module_path or get_project_root()
        for _, root in ipairs({ "src/main/java", "src/test/java" }) do
            local full_root = base_module .. "/" .. root
            if vim.fn.isdirectory(full_root) == 1 then
                table.insert(java_roots_to_clean, full_root)
            end
        end

        for _, java_root in ipairs(java_roots_to_clean) do
            local handle = io.popen("find " .. shell_escape(java_root) .. " -type d -empty 2>/dev/null")
            if handle then
                local empty_dirs = {}
                for dir in handle:lines() do
                    table.insert(empty_dirs, dir)
                end
                handle:close()

                -- Remove bottom-up (sort by depth descending so deepest dirs are removed first)
                table.sort(empty_dirs, function(a, b)
                    return #a > #b
                end)
                for _, dir in ipairs(empty_dirs) do
                    vim.fn.delete(dir, "d")
                    log.info("Removed empty directory:", dir)
                    cleanup_count = cleanup_count + 1
                end
            end
        end

        if cleanup_count > 0 then
            log.info("Cleaned up", cleanup_count, "empty directories")
        else
            log.info("No empty directories to clean up")
        end
    end

    -- Add test mirrors to registered changes for package declaration updates
    for _, mirror in ipairs(test_mirrors) do
        table.insert(all_registered_changes, mirror)
        log.debug("Added test mirror for refactoring:", mirror.src, "->", mirror.dst)
    end

    if #test_mirrors > 0 then
        log.info("Added", #test_mirrors, "test package mirrors for declaration updates")
    end

    -- Strategy: Find the deepest common package path change
    -- For example, if moving com/example/... -> ua/dsm/corp/example/...
    -- We want to process com/example (depth 2), not com (depth 1)

    -- Find all directory/package moves (not files)
    local package_moves = {}
    for _, change in ipairs(all_registered_changes) do
        if not change.src:match("%.java$") then
            -- This is a directory move
            -- Skip if src == dst (no actual move, e.g., test mirror with same directory)
            if change.src == change.dst then
                log.debug("Skipping no-op directory move (src == dst):", change.src)
            else
                -- Calculate package depth
                local root_match = nil
                for _, root in ipairs(package_roots) do
                    if string_util.contains(change.src, root) then
                        root_match = root
                        break
                    end
                end

                if root_match then
                    local package_path = vim.split(change.src, root_match)[2]
                    -- Remove trailing slash if present
                    package_path = package_path:gsub("/$", "")

                    -- Skip directory changes that are PARENTS of the canonical old prefix.
                    -- These represent partial filesystem events; the correct transformation
                    -- is the canonical one at the deeper level.
                    if canonical_old_prefix then
                        if
                            package_path ~= canonical_old_prefix
                            and canonical_old_prefix:find("^" .. vim.pesc(package_path) .. "/")
                        then
                            log.info(
                                "Skipping parent-of-canonical package move (partial rename):",
                                package_path,
                                "(canonical:",
                                canonical_old_prefix,
                                ")"
                            )
                            goto continue_pkg_loop
                        end
                    end

                    -- Calculate depth: com/example = 2 levels (1 slash + 1)
                    local slash_count = select(2, package_path:gsub("/", ""))
                    local package_depth = slash_count + 1
                    table.insert(package_moves, {
                        change = change,
                        depth = package_depth,
                        package_path = package_path,
                    })
                    log.debug("Found package move:", package_path, "depth:", package_depth)
                end
            end
        end
        ::continue_pkg_loop::
    end

    -- ENHANCEMENT: Infer directory moves from file moves
    -- This is needed when using file managers like fyler.nvim that only register file moves
    -- Strategy: Find the common ancestor package directory for all file moves
    -- IMPORTANT: Only infer package moves when multiple files are being moved together
    -- Single file moves should be processed as file-level moves, not package moves
    if #package_moves == 0 then
        log.info("No explicit directory moves found, will process files individually")
    end

    -- OPTIMIZATION: Find the most efficient package move
    -- Strategy: Select the SHALLOWEST valid move (depth >= 2) that covers all file changes
    -- This is more efficient than processing many deep changes individually

    -- Filter out overly broad moves (depth < 2)
    local valid_moves = {}
    for _, move in ipairs(package_moves) do
        if move.depth >= 2 then
            table.insert(valid_moves, move)
        else
            log.warn("Skipping overly broad package move at depth", move.depth, ":", move.package_path)
        end
    end

    if #valid_moves == 0 then
        log.info("No valid package moves found (all had depth < 2)")
    end

    -- CRITICAL: Deduplicate overlapping package moves.
    -- After destination correction, the SHALLOWEST correct move per branch already covers
    -- all deeper packages (because `.` is in the sed trailing boundary pattern).
    -- E.g., sed for `payments.infra.metrics → govern.test.infra.metrics` also transforms
    -- `payments.infra.metrics.micrometer → govern.test.infra.metrics.micrometer`.
    -- Strategy: keep shallowest per branch, remove deeper ones covered by a shallower move.
    if #valid_moves > 1 then
        -- Sort by depth ascending (shallowest first)
        table.sort(valid_moves, function(a, b)
            return a.depth < b.depth
        end)

        local deduped_moves = {}
        for _, move in ipairs(valid_moves) do
            local is_covered = false
            local src_pkg = move.package_path:gsub("/", ".")
            for _, kept in ipairs(deduped_moves) do
                local kept_pkg = kept.package_path:gsub("/", ".")
                -- Check if current move is a child of an already-kept shallower move
                if src_pkg:find("^" .. vim.pesc(kept_pkg) .. "%.") or src_pkg == kept_pkg then
                    is_covered = true
                    log.info("Removing redundant child package move:", src_pkg, "(already covered by:", kept_pkg, ")")
                    break
                end
            end
            if not is_covered then
                table.insert(deduped_moves, move)
            end
        end
        valid_moves = deduped_moves
        log.info("After deduplication:", #valid_moves, "package moves remaining")
    end

    -- Sort by depth (deepest first) for processing — ensures no ordering issues
    -- between sibling moves at different levels
    table.sort(valid_moves, function(a, b)
        return a.depth > b.depth
    end)

    -- Process all valid package moves or file changes
    local global_operations = {}
    if #valid_moves > 0 then
        -- Process ALL package-level changes, not just the shallowest one
        -- This is important when moving multiple sibling directories (e.g., adapter/x, adapter/y, adapter/z -> adapter/code/x, adapter/code/y, adapter/code/z)
        log.info("Processing", #valid_moves, "package-level refactorings")

        for _, move in ipairs(valid_moves) do
            move.change.siblings = get_all_src_siblings(move.change, all_registered_changes)
            if move.change.siblings and #move.change.siblings > 0 then
                log.debug("Found", #move.change.siblings, "siblings for", move.change.src)
            end
            local operations = build_fix_java_proj_after_change_cmd(move.change, module_path)
            if operations then
                log.debug("Adding", #operations, "operations for:", move.change.dst)
                -- Flatten operations into global list
                for _, op in ipairs(operations) do
                    table.insert(global_operations, op)
                end
            end
        end
    else
        -- Process all file changes individually
        log.info("No valid package moves found, processing files individually")
        for _, value in ipairs(all_registered_changes) do
            if value.src:match("%.java$") then
                value.siblings = get_all_src_siblings(value, all_registered_changes)
                if value.siblings and #value.siblings > 0 then
                    log.debug("Found", #value.siblings, "siblings for", value.src)
                end
                local operations = build_fix_java_proj_after_change_cmd(value, module_path)
                if operations then
                    log.debug("Adding", #operations, "operations for:", value.dst)
                    -- Flatten operations into global list
                    for _, op in ipairs(operations) do
                        table.insert(global_operations, op)
                    end
                end
            end
        end
    end
    log.info("Total operations to execute:", #global_operations)

    -- Separate shell commands from Lua operations
    local shell_cmds = {}
    local lua_operations = {}
    for _, op in ipairs(global_operations) do
        if op.type == "shell" then
            table.insert(shell_cmds, op.command)
        elseif op.type == "lua" then
            table.insert(lua_operations, op)
        end
    end

    log.info("Shell commands:", #shell_cmds, "| Lua operations:", #lua_operations)
    -- Use '; ' to join commands so one failure doesn't abort all subsequent independent operations.
    -- Each rg|xargs pipeline already has '|| echo skipped' for graceful failure handling.
    local global_cmd_run = table.concat(shell_cmds, " ; ")
    log.debug("Full shell command chain length:", #global_cmd_run, "characters")

    -- In test mode, execute directly and return result
    if M.test_mode then
        log.info("Test mode: executing operations directly")

        -- Execute shell commands individually for better error reporting
        if #shell_cmds > 0 then
            local failed_cmds = 0
            for i, cmd in ipairs(shell_cmds) do
                log.debug("Executing command", i, "of", #shell_cmds, ":", cmd)
                local exit_code = os.execute(cmd)
                if not (exit_code == 0 or exit_code == true) then
                    log.error("Shell command failed (continuing):", cmd)
                    failed_cmds = failed_cmds + 1
                end
            end
            if failed_cmds > 0 then
                log.warn(failed_cmds, "of", #shell_cmds, "shell commands had non-zero exit")
            end
            log.info("Shell commands completed:", #shell_cmds - failed_cmds, "succeeded,", failed_cmds, "failed")
        end

        -- Execute Lua operations
        if #lua_operations > 0 then
            log.info("Executing", #lua_operations, "Lua operations")
            for _, op in ipairs(lua_operations) do
                log.info("Executing:", op.description)
                local success = op.fn()
                if not success then
                    log.error("Lua operation failed:", op.description)
                    all_registered_changes = {}
                    return false
                end
            end
            log.info("All Lua operations completed successfully")
        end

        all_registered_changes = {}
        return true
    end

    -- ENHANCEMENT: Handle old buffers BEFORE applying changes
    -- Strategy: For the current buffer (focused window), switch to new path immediately.
    -- For other buffers, mark them for deletion and reopening after refactoring.
    -- This ensures the user sees the renamed file in their active window.
    if #opened_buffers_to_reopen > 0 then
        log.info("Processing", #opened_buffers_to_reopen, "old buffers before applying changes...")

        local current_win = vim.api.nvim_get_current_win()
        local current_buf = vim.api.nvim_get_current_buf()
        log.debug("Current window:", current_win, "Current buffer:", current_buf)

        for i, buf_info in ipairs(opened_buffers_to_reopen) do
            log.debug("Processing buffer", i, "of", #opened_buffers_to_reopen)
            if vim.api.nvim_buf_is_valid(buf_info.buf_id) then
                -- Store window ID BEFORE deleting buffer
                buf_info.win_id = vim.fn.bufwinid(buf_info.buf_id)
                buf_info.is_current = (buf_info.buf_id == current_buf)
                log.debug("Buffer", buf_info.buf_id, "in window", buf_info.win_id, "is_current:", buf_info.is_current)

                if buf_info.is_current then
                    -- For the CURRENT buffer: switch to a scratch buffer first, then delete old
                    -- This prevents Neovim from auto-selecting a random buffer
                    local scratch = vim.api.nvim_create_buf(false, true)
                    vim.api.nvim_win_set_buf(current_win, scratch)
                    local success, err = pcall(vim.api.nvim_buf_delete, buf_info.buf_id, { force = true })
                    if success then
                        log.info("Switched current window away from moved buffer:", buf_info.old_path)
                    else
                        log.warn("Failed to delete current buffer:", buf_info.old_path, "Error:", err)
                    end
                    -- Store scratch buffer to clean up later
                    buf_info.scratch_buf = scratch
                else
                    -- For non-current buffers: just delete
                    local success, err = pcall(vim.api.nvim_buf_delete, buf_info.buf_id, { force = true })
                    if success then
                        log.info("Deleted old buffer:", buf_info.old_path, "(was in window", buf_info.win_id, ")")
                    else
                        log.warn("Failed to delete buffer:", buf_info.old_path, "Error:", err)
                    end
                end
            else
                log.debug("Buffer already invalid:", buf_info.old_path)
            end
        end
    end

    -- ENHANCEMENT: Create composite callback that executes Lua operations then reopens buffers
    -- This ensures proper execution order: shell commands -> Lua operations -> reopen buffers -> cleanup
    local composite_callback = function()
        -- Execute Lua operations after shell commands complete
        if #lua_operations > 0 then
            log.info("Executing", #lua_operations, "Lua operations after shell commands")
            for _, op in ipairs(lua_operations) do
                log.info("Executing:", op.description)
                local success = op.fn()
                if not success then
                    log.error("Lua operation failed:", op.description)
                    -- Continue with other operations even if one fails
                end
            end
            log.info("All Lua operations completed")
        end

        -- Reopen buffers from new locations
        if #opened_buffers_to_reopen > 0 then
            log.info("Reopening", #opened_buffers_to_reopen, "buffers from new locations...")

            for i, buf_info in ipairs(opened_buffers_to_reopen) do
                log.debug("Reopening buffer", i, "of", #opened_buffers_to_reopen, ":", buf_info.new_path)
                -- Open new buffer from new location
                if vim.fn.filereadable(buf_info.new_path) == 1 then
                    log.debug("File exists at new location:", buf_info.new_path)

                    if buf_info.is_current then
                        -- This was the focused buffer — open in the current window
                        local target_win = vim.api.nvim_get_current_win()
                        -- If we opened a terminal split (run_cmd), the current window might have changed.
                        -- Use the original window if it's still valid.
                        if buf_info.win_id and buf_info.win_id ~= -1 and vim.api.nvim_win_is_valid(buf_info.win_id) then
                            target_win = buf_info.win_id
                        end
                        vim.api.nvim_win_call(target_win, function()
                            vim.cmd("edit " .. vim.fn.fnameescape(buf_info.new_path))
                            vim.cmd("filetype detect")
                        end)
                        -- Clean up scratch buffer
                        if buf_info.scratch_buf and vim.api.nvim_buf_is_valid(buf_info.scratch_buf) then
                            pcall(vim.api.nvim_buf_delete, buf_info.scratch_buf, { force = true })
                        end
                        log.info("Switched current window to new file:", buf_info.new_path)
                    elseif buf_info.win_id and buf_info.win_id ~= -1 and vim.api.nvim_win_is_valid(buf_info.win_id) then
                        -- Buffer was displayed in a window, open new file in that window
                        log.debug("Reopening in window", buf_info.win_id)
                        vim.api.nvim_win_call(buf_info.win_id, function()
                            vim.cmd("edit " .. vim.fn.fnameescape(buf_info.new_path))
                            vim.cmd("filetype detect")
                        end)
                        log.info("Reopened buffer in window", buf_info.win_id, ":", buf_info.new_path)
                    else
                        -- Buffer was not displayed or window no longer valid, just load it
                        log.debug("Loading as hidden buffer (win_id:", buf_info.win_id, ")")
                        vim.cmd("badd " .. vim.fn.fnameescape(buf_info.new_path))
                        log.info("Loaded hidden buffer:", buf_info.new_path)
                    end
                else
                    log.warn("New file not found, cannot reopen:", buf_info.new_path)
                end
            end
            log.info("Buffer reopening completed")
        end

        -- ENHANCEMENT: Clean up ALL empty directories in the module's java source trees.
        -- This catches any orphaned empty directories from old packages after file moves.
        -- Uses `find -type d -empty` and removes bottom-up (deepest first).
        log.info("Cleaning up empty source directories after file moves...")
        local cleanup_count = 0
        local base_module = module_path or vim.fn.getcwd()
        for _, root in ipairs({ "src/main/java", "src/test/java" }) do
            local full_root = base_module .. "/" .. root
            if vim.fn.isdirectory(full_root) == 1 then
                local handle = io.popen("find " .. shell_escape(full_root) .. " -type d -empty 2>/dev/null")
                if handle then
                    local empty_dirs = {}
                    for dir in handle:lines() do
                        table.insert(empty_dirs, dir)
                    end
                    handle:close()

                    -- Remove deepest first (sort by path length descending)
                    table.sort(empty_dirs, function(a, b)
                        return #a > #b
                    end)
                    for _, dir in ipairs(empty_dirs) do
                        vim.fn.delete(dir, "d")
                        log.info("Removed empty source directory:", dir)
                        cleanup_count = cleanup_count + 1
                    end
                end
            end
        end
        if cleanup_count > 0 then
            log.info("Cleaned up", cleanup_count, "empty source directories")
        end
    end

    -- Execute shell commands via terminal, then run composite callback
    if #shell_cmds > 0 then
        run_cmd(global_cmd_run, composite_callback)
    else
        -- No shell commands, just execute Lua operations and reopen buffers
        log.info("No shell commands to execute, running Lua operations directly")
        vim.schedule(function()
            composite_callback()
        end)
    end

    log.info("Clearing registered changes")
    all_registered_changes = {}
    return true
end

---@param src string
---@param dst string
function M.process_single_file_change(src, dst)
    log.info("Processing single file change:", src, "->", dst)
    M.register_change(src, dst)
    M.process_registerd_changes()
end

return M
