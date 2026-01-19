---@class ImportFixerOpts
---@field old_dir string
---@field old_package string
---@field new_package string
---@field new_file_path string
---@field old_type_name string
---@field new_type_name string
---@field siblings? string[]

local M = {}

local logging = require("utils.logging-util")
local log = logging.new({ name = "import-fixer", filename = "java-refactor.log" })

-- Use GNU sed on both platforms for consistent behavior
-- macOS: gsed (installed via brew install gnu-sed)
-- Linux: sed (already GNU sed)
local sed = vim.loop.os_uname().sysname == "Darwin" and "gsed" or "sed"

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
    -- GNU sed (both macOS gsed and Linux sed)
    local sed_cmd = string.format(
        "%s -i '%da\\%s' '%s'",
        sed,
        line_num,
        import_line,
        file_path
    )

    local result = os.execute(sed_cmd)
    if not (result == 0 or result == true) then
        log.warn("Failed to add import:", import_line, "to", file_path)
        return false
    end
    return true
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
        string.format("rg -n '^import ' '%s' 2>/dev/null | tail -n 1 | cut -d: -f1", opts.new_file_path)
    )
    local last_import_line = tonumber(last_import_output) or 2
    log.debug("Last import line:", last_import_line)

    -- Get the package of the file being fixed
    local file_package_output = exec_and_read(
        string.format("rg -m1 '^package ' '%s' 2>/dev/null | sed 's/package \\(.*\\);/\\1/'", opts.new_file_path)
    )
    local file_package = file_package_output and file_package_output:gsub("%s+", "") or ""
    log.debug("File package:", file_package)

    -- Get all Java files in old directory (not recursively)
    local java_files_handle = io.popen(
        string.format("fd --color=never -e java --max-depth 1 . '%s' -x basename {} .java 2>/dev/null", opts.old_dir)
    )

    if not java_files_handle then
        log.warn("Failed to list files in old directory:", opts.old_dir)
        return true
    end

    local imports_added = 0
    for filename in java_files_handle:lines() do
        log.debug("Checking if file uses type:", filename)

        -- Check if the moved file uses this type
        local uses_type = os.execute(
            string.format(
                "rg -q '(^|[[:space:],;(}<])%s($|[[:space:],;(}\\.>])' '%s' 2>/dev/null",
                filename,
                opts.new_file_path
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

    -- Fix imports in files that stayed in old directory
    -- They need to import the moved type from the new package
    log.debug("Fixing imports in old directory files")
    local sibling_usage_fixer = require("utils.java.refactor.sibling-usage-fixer")

    local old_dir_files_handle = io.popen(
        string.format("fd --color=never -e java --max-depth 1 . '%s' 2>/dev/null", opts.old_dir)
    )

    if old_dir_files_handle then
        local fixes_applied = 0
        for old_file in old_dir_files_handle:lines() do
            log.debug("Fixing sibling usage in old directory file:", old_file)

            local success = sibling_usage_fixer.fix_sibling_usage({
                file_path = old_file,
                new_package = opts.new_package,
                old_type_name = opts.old_type_name,
                new_type_name = opts.new_type_name
            })

            if success then
                fixes_applied = fixes_applied + 1
            end
        end
        old_dir_files_handle:close()
        log.info("Fixed", fixes_applied, "files in old directory")
    end

    return true
end

return M
