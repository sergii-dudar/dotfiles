---@class ImportFixerOpts
---@field old_dir string
---@field old_package string
---@field new_package string
---@field new_file_path string
---@field old_type_name string
---@field new_type_name string
---@field siblings? string[]
---@field module_path? string The module path to limit operations to

local M = {}

local logging = require("utils.logging-util")
local log = logging.new({ name = "import-fixer", filename = "java-refactor.log" })

-- Use GNU sed on both platforms for consistent behavior
-- macOS: gsed (installed via brew install gnu-sed)
-- Linux: sed (already GNU sed)
local is_macos = vim.loop.os_uname().sysname == "Darwin"
local sed = is_macos and "gsed" or "sed"

-- Boundary patterns for matching Java type names (shared with init.lua logic)
local LEADING_BOUNDARY = "(^|[[:space:],;(}<@])"
local TRAILING_BOUNDARY = "([[:space:],;(}\\.>@])"

-- Helper to escape single quotes in paths for safe shell interpolation
local function shell_escape(s)
    return "'" .. s:gsub("'", "'\\''") .. "'"
end

-- Helper to execute command and get output
local function exec_and_read(cmd)
    local handle = io.popen(cmd)
    if not handle then
        log.error("Failed to execute command:", cmd)
        return nil
    end
    local result = handle:read("*all")
    handle:close()
    return result
end

-- Helper to check if a type is in the siblings list
local function is_sibling(type_name, siblings)
    if not siblings then
        return false
    end
    for _, sibling in ipairs(siblings) do
        if sibling == type_name then
            return true
        end
    end
    return false
end

-- Add an import line to a file at a specific line number
local function add_import_line(file_path, line_num, import_line)
    -- First check if import already exists to avoid duplicates
    local check_cmd = string.format(
        "rg -q '^%s$' %s 2>/dev/null",
        import_line:gsub("([%.%[%]%(%)%*%+%-%?%^%$])", "%%%1"), -- Escape regex special chars
        shell_escape(file_path)
    )
    local already_exists = os.execute(check_cmd)

    if already_exists == 0 or already_exists == true then
        log.debug("Import already exists, skipping:", import_line)
        return true
    end

    -- Use GNU sed append command with literal newline
    local sed_cmd = string.format("%s -i '%da\\\n%s' %s", sed, line_num, import_line, shell_escape(file_path))

    log.debug("Sed command:", sed_cmd)
    local result = os.execute(sed_cmd)
    log.debug("Sed result:", result)

    -- Verify the import was actually added
    local verify_cmd = string.format(
        "rg -q '%s' %s 2>/dev/null",
        import_line:gsub("([%.%[%]%(%)%*%+%-%?%^%$])", "%%%1"), -- Escape regex special chars
        shell_escape(file_path)
    )
    local verify_result = os.execute(verify_cmd)

    if verify_result == 0 or verify_result == true then
        log.debug("Import successfully added (verified)")
        return true
    else
        log.warn("Failed to add import:", import_line, "to", file_path)
        return false
    end
end

---Fix imports in files that reference types from the old package
---This adds imports to the moved file for types that stayed in the old directory
---@param opts ImportFixerOpts
---@return boolean success
function M.fix_old_package_imports(opts)
    log.info("Fixing old package imports for:", opts.new_file_path)
    log.debug("Old dir:", opts.old_dir)
    log.debug("Old package:", opts.old_package)
    log.debug("New package:", opts.new_package)
    log.debug("Siblings:", opts.siblings and table.concat(opts.siblings, ",") or "none")
    log.debug("Module path restriction:", opts.module_path or "none (project-wide)")

    -- Determine search root: module path if available, otherwise extract from old_dir
    local search_root = opts.module_path
    if not search_root then
        -- Fallback: Extract project root from old_dir (go up to src/main/java or src/test/java parent)
        search_root = opts.old_dir:match("(.+)/src/main/java/")
            or opts.old_dir:match("(.+)/src/test/java/")
            or opts.old_dir
    end
    log.debug("Search root for wildcard search:", search_root)

    -- Check if file exists
    if vim.fn.filereadable(opts.new_file_path) == 0 then
        log.error("File not found:", opts.new_file_path)
        return false
    end

    -- OLD_DIR might not exist if all files were moved from it - this is OK
    -- We only process files in OLD_DIR if the directory still exists
    if vim.fn.isdirectory(opts.old_dir) == 0 then
        log.debug("Old directory doesn't exist (all files moved):", opts.old_dir)
        return true
    end

    -- Find last import line using rg
    local last_import_output = exec_and_read(
        string.format("rg -n '^import ' %s 2>/dev/null | tail -n 1 | cut -d: -f1", shell_escape(opts.new_file_path))
    )
    local last_import_line = tonumber(last_import_output) or 2
    log.debug("Last import line:", last_import_line)

    -- Get the package of the file being fixed
    local file_package_output = exec_and_read(
        string.format(
            "rg -m1 '^package ' %s 2>/dev/null | sed 's/package \\(.*\\);/\\1/'",
            shell_escape(opts.new_file_path)
        )
    )
    local file_package = file_package_output and file_package_output:gsub("%s+", "") or ""
    log.debug("File package:", file_package)

    -- Get all Java files in old directory (not recursively)
    local java_files_handle = io.popen(
        string.format(
            "fd --color=never -e java --max-depth 1 . %s -x basename {} .java 2>/dev/null",
            shell_escape(opts.old_dir)
        )
    )

    if not java_files_handle then
        log.warn("Failed to list files in old directory:", opts.old_dir)
        return true
    end

    local imports_added = 0
    for filename in java_files_handle:lines() do
        log.debug("Checking if file uses type:", filename)

        -- Check if the moved file uses this type (with proper boundary matching)
        local uses_type = os.execute(
            string.format(
                "rg -q '%s%s%s' %s 2>/dev/null",
                LEADING_BOUNDARY,
                filename,
                TRAILING_BOUNDARY,
                shell_escape(opts.new_file_path)
            )
        )

        if uses_type == 0 or uses_type == true then
            log.debug("File uses type:", filename)

            -- Determine the correct package for import
            local import_package = opts.old_package

            -- If this file is a sibling (also being moved), import from NEW_PACKAGE instead
            if is_sibling(filename, opts.siblings) then
                import_package = opts.new_package
                log.debug("Type is sibling, importing from new package:", import_package)
            else
                log.debug("Type is not sibling, importing from old package:", import_package)
            end

            -- Only add import if not in the same package
            if file_package ~= import_package then
                local import_line = string.format("import %s.%s;", import_package, filename)
                log.info("Adding import:", import_line)

                if add_import_line(opts.new_file_path, last_import_line, import_line) then
                    imports_added = imports_added + 1
                end
            else
                log.debug("Skipping import (same package):", filename)
            end
        end
    end

    java_files_handle:close()
    log.info("Added", imports_added, "imports to", opts.new_file_path)

    -- Fix imports in files that stayed in old directory (both main and test)
    -- They need to import the moved type from the new package
    log.debug("Fixing imports in old directory files")
    local sibling_usage_fixer = require("modules.java.refactor.sibling-usage-fixer")

    -- Process main directory
    local old_dir_files_handle =
        io.popen(string.format("fd --color=never -e java --max-depth 1 . %s 2>/dev/null", shell_escape(opts.old_dir)))

    if old_dir_files_handle then
        local fixes_applied = 0
        for old_file in old_dir_files_handle:lines() do
            log.debug("Fixing sibling usage in old directory file:", old_file)

            local success = sibling_usage_fixer.fix_sibling_usage({
                file_path = old_file,
                new_package = opts.new_package,
                old_type_name = opts.old_type_name,
                new_type_name = opts.new_type_name,
            })

            if success then
                fixes_applied = fixes_applied + 1
            end
        end
        old_dir_files_handle:close()
        log.info("Fixed", fixes_applied, "files in old directory")
    end

    -- Also process counterpart directory (test mirror of old_dir, or main mirror if old_dir is test)
    local counterpart_old_dir
    if string.find(opts.old_dir, "src/main/java/") then
        counterpart_old_dir = opts.old_dir:gsub("src/main/java/", "src/test/java/")
    elseif string.find(opts.old_dir, "src/test/java/") then
        counterpart_old_dir = opts.old_dir:gsub("src/test/java/", "src/main/java/")
    end

    if counterpart_old_dir and counterpart_old_dir ~= opts.old_dir and vim.fn.isdirectory(counterpart_old_dir) == 1 then
        log.debug("Fixing files in counterpart directory:", counterpart_old_dir)
        local counterpart_files_handle =
            io.popen(string.format("fd --color=never -e java . %s 2>/dev/null", shell_escape(counterpart_old_dir)))

        if counterpart_files_handle then
            local counterpart_fixes = 0
            local counterpart_files_found = 0
            for counterpart_file in counterpart_files_handle:lines() do
                counterpart_files_found = counterpart_files_found + 1
                log.debug("Found counterpart file:", counterpart_file)

                -- Check if this file uses the moved type (old OR new name)
                local uses_old_name = os.execute(
                    string.format(
                        "rg -q '%s%s%s' %s 2>/dev/null",
                        LEADING_BOUNDARY,
                        opts.old_type_name,
                        TRAILING_BOUNDARY,
                        shell_escape(counterpart_file)
                    )
                )
                local uses_new_name = os.execute(
                    string.format(
                        "rg -q '%s%s%s' %s 2>/dev/null",
                        LEADING_BOUNDARY,
                        opts.new_type_name,
                        TRAILING_BOUNDARY,
                        shell_escape(counterpart_file)
                    )
                )
                log.debug("Counterpart file uses old name:", uses_old_name)
                log.debug("Counterpart file uses new name:", uses_new_name)

                if (uses_old_name == 0 or uses_old_name == true) or (uses_new_name == 0 or uses_new_name == true) then
                    log.debug("Counterpart file uses the type (old or new name):", counterpart_file)

                    -- Find last import line in this file
                    local last_imp_output = exec_and_read(
                        string.format(
                            "rg -n '^import ' %s 2>/dev/null | tail -n 1 | cut -d: -f1",
                            shell_escape(counterpart_file)
                        )
                    )
                    local last_import = tonumber(last_imp_output) or 2

                    -- Add explicit import for the moved type
                    local import_line = string.format("import %s.%s;", opts.new_package, opts.new_type_name)
                    add_import_line(counterpart_file, last_import, import_line)

                    -- Always update the usage (even if import was already there) - double-pass
                    local replace_expr = string.format(
                        "s/%s%s%s/\\1%s\\2/g",
                        LEADING_BOUNDARY,
                        opts.old_type_name,
                        TRAILING_BOUNDARY,
                        opts.new_type_name
                    )
                    local update_cmd = string.format(
                        "%s -i -E '%s; %s' %s",
                        sed,
                        replace_expr,
                        replace_expr,
                        shell_escape(counterpart_file)
                    )
                    local update_result = os.execute(update_cmd)
                    if update_result == 0 or update_result == true then
                        counterpart_fixes = counterpart_fixes + 1
                        log.info("Fixed counterpart file:", counterpart_file)
                    else
                        log.warn("Failed to update usages in counterpart file:", counterpart_file)
                    end
                else
                    log.debug("Counterpart file doesn't use the type")
                end
            end
            counterpart_files_handle:close()
            log.info("Found", counterpart_files_found, "counterpart files")
            log.info("Fixed", counterpart_fixes, "counterpart files")
        else
            log.error("Failed to open counterpart files handle")
        end
    else
        log.debug("Counterpart directory does not exist:", counterpart_old_dir or "nil")
    end

    -- Also fix files with wildcard imports that use the moved type
    log.debug("Fixing files with wildcard imports of old package")
    local wildcard_import_pattern = opts.old_package:gsub("%.", "\\.") .. "\\.\\*"
    log.debug("Wildcard import pattern:", wildcard_import_pattern)

    local wildcard_search_cmd = string.format(
        "rg --color=never -l 'import\\s+%s;' %s 2>/dev/null",
        wildcard_import_pattern,
        shell_escape(search_root)
    )
    log.debug("Wildcard search command:", wildcard_search_cmd)

    local wildcard_files_handle = io.popen(wildcard_search_cmd)

    if wildcard_files_handle then
        local wildcard_fixes = 0
        local files_found = 0
        for wildcard_file in wildcard_files_handle:lines() do
            files_found = files_found + 1
            log.debug("Found file with wildcard import:", wildcard_file)

            -- Check for BOTH old and new names because this runs after shell commands
            local uses_old_name = os.execute(
                string.format(
                    "rg -q '%s%s%s' %s 2>/dev/null",
                    LEADING_BOUNDARY,
                    opts.old_type_name,
                    TRAILING_BOUNDARY,
                    shell_escape(wildcard_file)
                )
            )
            local uses_new_name = os.execute(
                string.format(
                    "rg -q '%s%s%s' %s 2>/dev/null",
                    LEADING_BOUNDARY,
                    opts.new_type_name,
                    TRAILING_BOUNDARY,
                    shell_escape(wildcard_file)
                )
            )

            if (uses_old_name == 0 or uses_old_name == true) or (uses_new_name == 0 or uses_new_name == true) then
                log.debug("File with wildcard import uses the type (old or new name):", wildcard_file)

                -- Find last import line in this file
                local last_imp_output = exec_and_read(
                    string.format(
                        "rg -n '^import ' %s 2>/dev/null | tail -n 1 | cut -d: -f1",
                        shell_escape(wildcard_file)
                    )
                )
                local last_import = tonumber(last_imp_output) or 2

                -- Add explicit import for the moved type
                local import_line = string.format("import %s.%s;", opts.new_package, opts.new_type_name)
                if add_import_line(wildcard_file, last_import, import_line) then
                    wildcard_fixes = wildcard_fixes + 1
                    log.info("Added explicit import to file with wildcard:", wildcard_file)
                end
            else
                log.debug("File with wildcard import doesn't use the type")
            end
        end
        wildcard_files_handle:close()
        log.info("Found", files_found, "files with wildcard imports")
        log.info("Fixed", wildcard_fixes, "files with wildcard imports")
    else
        log.error("Failed to open wildcard files handle")
    end

    return true
end

return M
