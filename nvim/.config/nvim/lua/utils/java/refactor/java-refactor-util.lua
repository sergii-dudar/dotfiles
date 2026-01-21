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
local import_fixer = require("utils.java.refactor.import-fixer")
local sibling_usage_fixer = require("utils.java.refactor.sibling-usage-fixer")

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
            spinner.stop(code == 0, "Java refactoring")
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
        if vim.fn.filereadable(current_dir .. "/pom.xml") == 1 or
           vim.fn.filereadable(current_dir .. "/build.gradle") == 1 or
           vim.fn.filereadable(current_dir .. "/build.gradle.kts") == 1 then
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
local sed = vim.loop.os_uname().sysname == "Darwin" and "gsed" or "sed"
log.debug("Detected OS:", vim.loop.os_uname().sysname, "- using sed command:", sed)

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
            test_dir_clause = string.format("; fd -e java . '%s'", test_package_dir)
            log.debug("Including test directory for same-package rename:", test_package_dir)
        end

        local fix_type_symbols_same_package = string.format(
            "(rg --color=never -l 'import\\s+%s([;.]|\\*;)' "
                .. "'%s'"
                .. " || true; fd -e java . '%s'%s) | sort -u | xargs %s -i -E 's/([[:space:],;(}<])%s([[:space:],;(}\\.>])/\\1%s\\2/g' || echo 'skipped'",
            package_declaration_src:gsub("%.", "\\."), -- Match package with explicit or wildcard import
            search_root, -- Use module path instead of project root
            package_dir, -- Include all files in same directory (main)
            test_dir_clause, -- Also include test directory if it exists
            sed,
            old_type_name,
            new_type_name
        )
        table.insert(result_cmds, {
            type = "shell",
            command = fix_type_symbols_same_package,
            description = "Fix type symbols in same package and imported files",
        })
    else
        -- For package moves, search for explicit imports AND wildcard imports
        local fix_type_symbols_where_imported = string.format(
            "rg --color=never -l 'import\\s+%s([;.]|\\*;)' "
                .. "'%s'"
                .. " | xargs %s -i -E 's/([[:space:],;(}<])%s([[:space:],;(}\\.>])/\\1%s\\2/g' || echo 'skipped'",
            package_declaration_src:gsub("%.", "\\."), -- Match package with explicit or wildcard import
            search_root, -- Use module path instead of project root
            sed,
            old_type_name,
            new_type_name
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
    -- 3. fix type full qualified names (acroll all files - java,yaml,properties etc)
    local fix_type_full_qualified_names = string.format(
        "rg --color=never -l '%s' "
            .. "'%s'"
            .. " | xargs %s -i -E 's/%s([;.$\"]|$)/%s\\1/g' || echo 'skipped'",
        -- sed -i -E 's/ServiceEmployee([^[:alnum:]_]|$)/ServiceEmployeeUser\1/g'
        package_src_classpath_escaped,
        search_root, -- Use module path instead of project root
        sed,
        package_src_classpath_escaped,
        package_dst_classpath
    )
    -- vim.notify(fix_type_symbols_where_imported)
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
        local package_declaration_dst_escaped = package_declaration_dst:gsub("%.", "\\.")
        local remove_same_package_imports =
            string.format("%s -i '/^import %s\\./d' %s", sed, package_declaration_dst_escaped, dst)
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

        local fix_file_paht_declaration = string.format(
            "rg --color=never -l '%s' "
                .. "'%s'"
                .. " | xargs %s -i -E 's/%s([;.\"]|$)/%s\\1/g' || echo 'skipped'",
            package_src_path_escaped,
            search_root, -- Use module path instead of project root
            sed,
            package_src_path_escaped,
            package_dst_path_escaped
        )
        -- vim.notify(fix_file_paht_declaration)
        table.insert(result_cmds, {
            type = "shell",
            command = fix_file_paht_declaration,
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
    -- Solution: Match ([;$"[:space:].]|$) - includes dot for subpackages
    --   - The dot allows matching subpackages during directory moves
    --   - The \1 in replacement preserves whatever was captured
    local fix_package_full_qualified_names = string.format(
        "rg --color=never -l '%s' "
            .. "'%s'"
            .. " | xargs %s -i -E 's/%s([;$\"[:space:].]|$)/%s\\1/g' || echo 'skipped'",
        package_src_classpath_escaped,
        search_root, -- Use module path instead of project root
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
    local fix_file_paht_declaration = string.format(
        "rg --color=never -l '%s' "
            .. "'%s'"
            .. " | xargs %s -i -E 's/%s([;.\"\\/]|$)/%s\\1/g' || echo 'skipped'",
        package_src_path_escaped,
        search_root, -- Use module path instead of project root
        sed,
        package_src_path_escaped,
        package_dst_path_escaped
    )
    table.insert(result_cmds, {
        type = "shell",
        command = fix_file_paht_declaration,
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
M.register_change = function(src, dst)
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

M.process_registerd_changes = function()
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
            local handle = io.popen("fd -e java . '" .. change.src .. "'")
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

    -- First, detect if we're doing a structural refactoring (e.g., adapter/* -> adapter/code/*)
    -- In this case, we need to move ALL test subdirectories, not just matching ones
    local structural_refactorings = {} -- { src_parent -> dst_parent }

    -- Detect structural patterns: multiple directories moving from X/* to X/Y/*
    local dir_moves = {}
    for _, change in ipairs(all_registered_changes) do
        if string_util.contains(change.src, "src/main/java/") and not change.src:match("%.java$") then
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

    -- For structural refactorings, find ALL test subdirectories and mirror them
    for src_parent, dst_parent in pairs(structural_refactorings) do
        local test_src_parent = src_parent:gsub("src/main/java/", "src/test/java/")
        local test_dst_parent = dst_parent:gsub("src/main/java/", "src/test/java/")

        log.debug("Checking for test subdirectories in:", test_src_parent)
        if vim.fn.isdirectory(test_src_parent) == 1 then
            -- Find ALL subdirectories in the test parent
            local fd_cmd = "fd --max-depth 1 --type d . '" .. test_src_parent .. "'"
            log.debug("Running fd command:", fd_cmd)
            local handle = io.popen(fd_cmd)
            if handle then
                local found_count = 0
                for test_subdir in handle:lines() do
                    -- Remove trailing slash if present
                    test_subdir = test_subdir:gsub("/$", "")
                    local subdir_name = test_subdir:match(".+/([^/]+)$")
                    log.debug("Found test subdirectory:", test_subdir, "name:", subdir_name)

                    -- Skip the destination directory itself (e.g., adapter/code)
                    if subdir_name and not test_subdir:match("/" .. vim.pesc(dst_parent:match(".+/([^/]+)$")) .. "$") then
                        local test_dst_subdir = test_dst_parent .. "/" .. subdir_name

                        if not test_mirror_dirs[test_subdir] then
                            test_mirror_dirs[test_subdir] = test_dst_subdir
                            table.insert(test_mirrors, { src = test_subdir, dst = test_dst_subdir })
                            log.info("Auto-mirroring test subdirectory (structural):", test_subdir, "->", test_dst_subdir)
                            found_count = found_count + 1
                        else
                            log.debug("Test subdirectory already in mirror list, skipping:", test_subdir)
                        end
                    else
                        log.debug("Skipping destination directory:", test_subdir)
                    end
                end
                handle:close()
                log.info("Found", found_count, "test subdirectories for structural refactoring")
            else
                log.error("Failed to execute fd command")
            end
        else
            log.info("Test source parent does not exist:", test_src_parent)
        end
    end

    -- Also handle individual directory/file moves (existing logic)
    for _, change in ipairs(all_registered_changes) do
        -- Check if this is a main/java move
        if string_util.contains(change.src, "src/main/java/") then
            -- Calculate corresponding test path
            local test_src = change.src:gsub("src/main/java/", "src/test/java/")
            local test_dst = change.dst:gsub("src/main/java/", "src/test/java/")

            -- For directory moves: mirror the directory directly (if not already handled by structural refactoring)
            if vim.fn.isdirectory(test_src) == 1 then
                if not test_mirror_dirs[test_src] then
                    test_mirror_dirs[test_src] = test_dst
                    table.insert(test_mirrors, { src = test_src, dst = test_dst })
                    log.info("Auto-mirroring test directory:", test_src, "->", test_dst)
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
                    local is_subdirectory_move = dst_dir:find("^" .. vim.pesc(src_dir) .. "/") or
                                                 src_dir:find("^" .. vim.pesc(dst_dir) .. "/")

                    if is_subdirectory_move then
                        log.debug("Skipping test mirror for subdirectory move (will process files individually):", src_dir, "->", dst_dir)
                    else
                        local test_src_dir = src_dir:gsub("src/main/java/", "src/test/java/")
                        local test_dst_dir = dst_dir:gsub("src/main/java/", "src/test/java/")

                        -- Only add if test directory exists and not already mirrored
                        if vim.fn.isdirectory(test_src_dir) == 1 and not test_mirror_dirs[test_src_dir] then
                            test_mirror_dirs[test_src_dir] = test_dst_dir
                            table.insert(test_mirrors, { src = test_src_dir, dst = test_dst_dir })
                            log.info("Auto-mirroring test directory (inferred from file move):", test_src_dir, "->", test_dst_dir)
                        end
                    end
                else
                    log.debug("Skipping test mirror for same-directory file rename")
                end
            end
        end
    end

    -- Physically move test files/directories to match main package structure
    if #test_mirrors > 0 then
        log.info("Physically moving", #test_mirrors, "test packages to match main structure")

        -- Track opened test buffers before moving
        for _, mirror in ipairs(test_mirrors) do
            if vim.fn.isdirectory(mirror.src) == 1 then
                -- Find all Java files in this test directory
                local handle = io.popen("fd -e java . '" .. mirror.src .. "'")
                if handle then
                    for file_path in handle:lines() do
                        local buf_id = buffer_util.find_buf_by_path(file_path)
                        if buf_id then
                            -- Calculate new path for this test file
                            local new_path = file_path:gsub("^" .. vim.pesc(mirror.src), mirror.dst)
                            table.insert(opened_buffers_to_reopen, {
                                old_path = file_path,
                                new_path = new_path,
                                buf_id = buf_id,
                            })
                            log.info("Will reopen test buffer:", file_path, "->", new_path)
                        end
                    end
                    handle:close()
                end
            elseif mirror.src:match("%.java$") then
                -- Single test file
                local buf_id = buffer_util.find_buf_by_path(mirror.src)
                if buf_id then
                    table.insert(opened_buffers_to_reopen, {
                        old_path = mirror.src,
                        new_path = mirror.dst,
                        buf_id = buf_id,
                    })
                    log.info("Will reopen test buffer:", mirror.src, "->", mirror.dst)
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
                    local cmd = string.format("mv '%s' '%s'", mirror.src, mirror.dst)
                    local exit_code = os.execute(cmd)
                    if exit_code == 0 or exit_code == true then
                        log.info("Moved test (via shell):", mirror.src, "->", mirror.dst)
                    else
                        log.error("Failed to move test:", mirror.src, "->", mirror.dst)
                    end
                end
            end
        end

        -- Clean up empty parent directories in test
        log.info("Cleaning up empty test directories...")
        local cleanup_count = 0
        for _, mirror in ipairs(test_mirrors) do
            local src_parent = mirror.src:match("(.+)/[^/]+$")
            while src_parent and src_parent:match("src/test/java/") do
                if vim.fn.isdirectory(src_parent) == 1 then
                    -- Check if directory is empty
                    local handle = io.popen("fd --max-depth 1 . '" .. src_parent .. "' | wc -l")
                    local count = tonumber(handle:read("*all"))
                    handle:close()

                    if count == 0 then
                        vim.fn.delete(src_parent, "d")
                        log.info("Removed empty test directory:", src_parent)
                        cleanup_count = cleanup_count + 1
                        src_parent = src_parent:match("(.+)/[^/]+$")
                    else
                        break
                    end
                else
                    break
                end
            end
        end
        if cleanup_count > 0 then
            log.info("Cleaned up", cleanup_count, "empty test directories")
        else
            log.info("No empty test directories to clean up")
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

    -- Sort by depth (shallowest first) for better readability in logs
    table.sort(valid_moves, function(a, b)
        return a.depth < b.depth
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
    local global_cmd_run = table.concat(shell_cmds, " && ")
    log.debug("Full shell command chain length:", #global_cmd_run, "characters")

    -- In test mode, execute directly and return result
    if M.test_mode then
        log.info("Test mode: executing operations directly")

        -- Execute shell commands first
        if #shell_cmds > 0 then
            log.debug("Executing shell commands:", global_cmd_run)
            local exit_code = os.execute(global_cmd_run)
            if not (exit_code == 0 or exit_code == true) then
                log.error("Shell command execution failed with exit code:", exit_code)
                all_registered_changes = {}
                return false
            end
            log.info("Shell commands completed successfully")
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

    -- ENHANCEMENT: Delete old buffers BEFORE applying changes
    -- This prevents having buffers pointing to non-existent files during refactoring
    if #opened_buffers_to_reopen > 0 then
        log.info("Deleting", #opened_buffers_to_reopen, "old buffers before applying changes...")

        for i, buf_info in ipairs(opened_buffers_to_reopen) do
            log.debug("Processing buffer", i, "of", #opened_buffers_to_reopen)
            if vim.api.nvim_buf_is_valid(buf_info.buf_id) then
                -- Store window ID BEFORE deleting buffer
                buf_info.win_id = vim.fn.bufwinid(buf_info.buf_id)
                log.debug("Buffer", buf_info.buf_id, "in window", buf_info.win_id)

                -- Delete old buffer
                local success, err = pcall(vim.api.nvim_buf_delete, buf_info.buf_id, { force = false })
                if success then
                    log.info("Deleted old buffer:", buf_info.old_path, "(was in window", buf_info.win_id, ")")
                else
                    log.warn("Failed to delete buffer:", buf_info.old_path, "Error:", err)
                end
            else
                log.debug("Buffer already invalid:", buf_info.old_path)
            end
        end
    end

    -- Normal mode: use UI to apply refactoring changes

    -- ENHANCEMENT: Create composite callback that executes Lua operations then reopens buffers
    -- This ensures proper execution order: shell commands -> Lua operations -> reopen buffers
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
                    if buf_info.win_id and buf_info.win_id ~= -1 and vim.api.nvim_win_is_valid(buf_info.win_id) then
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
M.process_single_file_change = function(src, dst)
    log.info("Processing single file change:", src, "->", dst)
    M.register_change(src, dst)
    M.process_registerd_changes()
end

return M
