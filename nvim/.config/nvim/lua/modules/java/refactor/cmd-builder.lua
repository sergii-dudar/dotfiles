-- Builds shell and Lua operations for fixing Java files/packages after rename/move.
-- Handles: type declarations, type symbols, FQNs, package declarations, imports, file paths.

local M = {}

local string_util = require("utils.string-util")
local consts = require("modules.java.refactor.constants")
local import_fixer = require("modules.java.refactor.import-fixer")
local sibling_usage_fixer = require("modules.java.refactor.sibling-usage-fixer")

local log = consts.log
local sed = consts.sed
local shell_escape = consts.shell_escape
local package_roots = consts.package_roots
local build_type_replace_expr = consts.build_type_replace_expr

---@class RefactorOperation
---@field type "shell"|"lua"
---@field command? string Shell command to execute
---@field fn? function Lua function to call
---@field description string Description for logging

--- Build a safe sed command for a stream of candidate files.
---@param source_cmd string Shell command that prints one file per line
---@param sed_expr string sed expression to run for each file
---@return string command
local function build_sed_for_files_cmd(source_cmd, sed_expr)
    return string.format(
        '(%s) | sort -u | while IFS= read -r file; do [ -n "$file" ] || continue; %s -i -E %s "$file" || exit 1; done',
        source_cmd,
        sed,
        shell_escape(sed_expr)
    )
end

--- Build a no-op-safe source command for Java files under a directory.
---@param dir string
---@return string command
local function fd_java_source_cmd(dir)
    return string.format("[ -d %s ] && fd -e java . %s || true", shell_escape(dir), shell_escape(dir))
end

---@param result_cmds RefactorOperation[]
---@param root string
---@param context java.rejactor.FileMove
---@param module_path string|nil The module path to limit operations to
local build_fix_java_file_after_change_cmds = function(result_cmds, root, context, module_path)
    log.debug("Building fix commands for file move:", context.src, "->", context.dst)
    log.debug("Module path restriction:", module_path or "none (project-wide)")

    local src = context.src
    local dst = context.dst
    local escaped_dst = shell_escape(dst)

    -- Determine the search root: module path if available, otherwise project root
    local search_root = module_path or consts.get_project_root()

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
    -- 1. fix type declaration in changed file.
    local fix_type_declaration_cmd = string.format(
        "%s -i -E 's/(class|interface|enum|record)([[:space:]]+)%s([[:space:]<({])/\\1\\2%s\\3/g' %s",
        sed,
        old_type_name,
        new_type_name,
        escaped_dst
    )
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
            escaped_dst
        )
        table.insert(result_cmds, {
            type = "shell",
            command = fix_constructor_cmd,
            description = "Fix constructor: " .. old_type_name .. " -> " .. new_type_name,
        })
    end

    -- ==========================================================================
    -- 2. fix type symbols (simple java name) where type is imported or accessible
    if is_same_package_rename then
        -- For same-package renames, update all files in the same package
        -- and files that import this package (explicitly or with wildcard)
        local package_dir = dst:match("(.+)/[^/]+$") -- Get directory of the file

        -- Also include corresponding test directory if this is a main file
        local test_package_dir = package_dir:gsub("src/main/java", "src/test/java")
        local source_cmds = {
            string.format(
                "rg --color=never -l %s %s || true",
                shell_escape("import\\s+" .. package_declaration_src:gsub("%.", "\\.") .. "([;.]|\\*;)"),
                shell_escape(search_root)
            ),
            fd_java_source_cmd(package_dir),
        }
        if test_package_dir ~= package_dir and vim.fn.isdirectory(test_package_dir) == 1 then
            table.insert(source_cmds, fd_java_source_cmd(test_package_dir))
            log.debug("Including test directory for same-package rename:", test_package_dir)
        end

        -- Also include corresponding main directory if this is a test file
        local main_package_dir = package_dir:gsub("src/test/java", "src/main/java")
        if main_package_dir ~= package_dir and vim.fn.isdirectory(main_package_dir) == 1 then
            table.insert(source_cmds, fd_java_source_cmd(main_package_dir))
            log.debug("Including main directory for same-package rename:", main_package_dir)
        end

        local fix_type_symbols_same_package = build_sed_for_files_cmd(
            table.concat(source_cmds, "; "),
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
        local old_package_dir = src:match("(.+)/[^/]+$") -- Directory where the file WAS
        local source_cmds = {
            string.format(
                "rg --color=never -l %s %s || true",
                shell_escape("import\\s+" .. package_declaration_src:gsub("%.", "\\.") .. "([;.]|\\*;)"),
                shell_escape(search_root)
            ),
        }
        if old_package_dir then
            table.insert(source_cmds, fd_java_source_cmd(old_package_dir))
        end

        -- Also include corresponding test/main directory of the old package
        if old_package_dir then
            local counterpart
            if string.find(old_package_dir, "src/main/java/") then
                counterpart = old_package_dir:gsub("src/main/java/", "src/test/java/")
            elseif string.find(old_package_dir, "src/test/java/") then
                counterpart = old_package_dir:gsub("src/test/java/", "src/main/java/")
            end
            if counterpart and counterpart ~= old_package_dir and vim.fn.isdirectory(counterpart) == 1 then
                table.insert(source_cmds, fd_java_source_cmd(counterpart))
            end
        end

        local fix_type_symbols_where_imported = build_sed_for_files_cmd(
            table.concat(source_cmds, "; "),
            build_type_replace_expr(old_type_name, new_type_name)
        )
        table.insert(result_cmds, {
            type = "shell",
            command = fix_type_symbols_where_imported,
            description = "Fix type symbols where imported (explicit or wildcard)",
        })
    end

    -- ==========================================================================
    -- 2.1. fix imports of siblings java files (in case moved to other packages)
    if context.siblings and not vim.tbl_isempty(context.siblings) then
        log.debug("Processing", #context.siblings, "sibling files")
        for _, sibling in ipairs(context.siblings) do
            local sibling_package_src_path = vim.split(sibling.src, root)[2]:gsub("%.java", "")
            local sibling_package_dst_path = vim.split(sibling.dst, root)[2]:gsub("%.java", "")

            local sibling_package_dst_classpath = sibling_package_dst_path:gsub("/", ".")

            local sibling_old_type_name = sibling_package_src_path:match("([^/]+)$")
            local sibling_new_type_name = sibling_package_dst_path:match("([^/]+)$")

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
    -- 3. fix type full qualified names (across all files - java,yaml,properties etc)
    local fix_type_full_qualified_names = build_sed_for_files_cmd(
        string.format(
            "rg --color=never -l %s %s || true",
            shell_escape(package_src_classpath_escaped),
            shell_escape(search_root)
        ),
        string.format('s/%s([;.$"[:space:]()><,@]|$)/%s\\1/g', package_src_classpath_escaped, package_dst_classpath)
    )
    table.insert(result_cmds, {
        type = "shell",
        command = fix_type_full_qualified_names,
        description = "Fix type full qualified names",
    })

    -- ==========================================================================
    -- PACKAGE-LEVEL OPERATIONS: Only execute these if the package actually changed
    if not is_same_package_rename then
        -- 4. fix package declaration in changed file.
        local fix_package_declaration = string.format(
            "%s -i -E 's/package[[:space:]]+%s;/package %s;/g' %s",
            sed,
            package_declaration_src_escaped,
            package_declaration_dst,
            escaped_dst
        )
        table.insert(result_cmds, {
            type = "shell",
            command = fix_package_declaration,
            description = "Fix package declaration",
        })

        -- 4.1. Remove imports from the same package (they're unnecessary)
        local package_declaration_dst_escaped = package_declaration_dst:gsub("%.", "\\.")
        local remove_same_package_imports =
            string.format("%s -i '/^import %s\\.[A-Z][^.]*;$/d' %s", sed, package_declaration_dst_escaped, escaped_dst)
        table.insert(result_cmds, {
            type = "shell",
            command = remove_same_package_imports,
            description = "Remove same-package imports",
        })
    else
        log.debug("Skipping package declaration updates (same package)")
    end

    -- ==========================================================================
    -- 5. add import declarations of the new class name to the classes of the old folder
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

        -- 6. fix file path/resources path
        local fix_file_path_declaration = build_sed_for_files_cmd(
            string.format(
                "rg --color=never -l %s %s || true",
                shell_escape(package_src_path_escaped),
                shell_escape(search_root)
            ),
            string.format('s/%s([;."[:space:])><,]|$)/%s\\1/g', package_src_path_escaped, package_dst_path_escaped)
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

---@param result_cmds RefactorOperation[]
---@param root string
---@param context java.rejactor.FileMove
---@param module_path string|nil The module path to limit operations to
local build_fix_java_package_after_change_cmds = function(result_cmds, root, context, module_path)
    log.debug("Building fix commands for package move:", context.src, "->", context.dst)
    log.debug("Module path restriction:", module_path or "none (project-wide)")
    local src = context.src
    local dst = context.dst

    -- Determine the search root: module path if available, otherwise project root
    local search_root = module_path or consts.get_project_root()

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

    -- ==========================================================================
    -- 1. fix package full qualified names (across all files - java,yaml,properties etc)
    -- Pattern includes dot for subpackages
    local fix_package_full_qualified_names = build_sed_for_files_cmd(
        string.format(
            "rg --color=never -l %s %s || true",
            shell_escape(package_src_classpath_escaped),
            shell_escape(search_root)
        ),
        string.format('s/%s([;$"[:space:].,()><@]|$)/%s\\1/g', package_src_classpath_escaped, package_dst_classpath)
    )
    log.debug("Package replacement command:", fix_package_full_qualified_names)
    table.insert(result_cmds, {
        type = "shell",
        command = fix_package_full_qualified_names,
        description = "Fix package full qualified names: " .. package_src_classpath .. " -> " .. package_dst_classpath,
    })

    -- ==========================================================================
    -- 2. fix package path/resources path
    local fix_file_path_declaration = build_sed_for_files_cmd(
        string.format(
            "rg --color=never -l %s %s || true",
            shell_escape(package_src_path_escaped),
            shell_escape(search_root)
        ),
        string.format('s/%s([;."\\/[:space:]]|$)/%s\\1/g', package_src_path_escaped, package_dst_path_escaped)
    )
    table.insert(result_cmds, {
        type = "shell",
        command = fix_file_path_declaration,
        description = "Fix package path/resources path",
    })
end

--- Build fix commands for a single file or package move.
---@param context java.rejactor.FileMove
---@param module_path string|nil The module path to limit operations to
---@return RefactorOperation[]|nil
function M.build_fix_commands(context, module_path)
    if not context.src:match("src/.*/java/") then
        return nil
    end
    if not context.dst:match("src/.*/java/") then
        return nil
    end

    local result_cmds = {}
    local util = require("utils.common-util")
    local is_dir = util.is_dir(context.dst)
    local is_file = util.is_file(context.dst)

    for _, root in ipairs(package_roots) do
        if string_util.contains(context.src, root) and string_util.contains(context.dst, root) then
            if is_file then
                build_fix_java_file_after_change_cmds(result_cmds, root, context, module_path)
            elseif is_dir then
                build_fix_java_package_after_change_cmds(result_cmds, root, context, module_path)
            end
        end
    end
    return result_cmds
end

--- Get all sibling file moves from the same source directory.
---@param context java.rejactor.FileMove
---@param all_changes java.rejactor.FileMove[]
---@return java.rejactor.FileMove[]
function M.get_all_src_siblings(context, all_changes)
    local context_src_dir = context.src:match("(.+)/[^/]+$")
    local context_src_siblings = {}
    for _, value in ipairs(all_changes) do
        local current_src_dir = value.src:match("(.+)/[^/]+$")
        if context_src_dir == current_src_dir and context.dst ~= value.dst then
            table.insert(context_src_siblings, value)
        end
    end
    return context_src_siblings
end

return M
