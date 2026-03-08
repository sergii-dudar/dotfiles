---@class SiblingUsageFixerOpts
---@field file_path string The file to fix
---@field new_package string The new package of the moved type
---@field old_type_name string The old name of the type
---@field new_type_name string The new name of the type
---@field file_dst_package? string Optional: The destination package of the file being fixed

local M = {}

local logging = require("utils.logging-util")
local log = logging.new({ name = "sibling-usage-fixer", filename = "java-refactor.log" })

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

-- Add an import line to a file at a specific line number
local function add_import_line(file_path, line_num, import_line)
    -- Use GNU sed append command with literal newline
    -- The key is \\\n which creates backslash + newline in the shell command
    local sed_cmd = string.format("%s -i '%da\\\n%s' '%s'", sed, line_num, import_line, file_path)

    local result = os.execute(sed_cmd)
    if not (result == 0 or result == true) then
        log.warn("Failed to add import:", import_line, "to", file_path)
        return false
    end
    return true
end

---Fix usages of a moved type in a file that references it
---This adds the import for the moved type and updates type name if it changed
---@param opts SiblingUsageFixerOpts
---@return boolean success
function M.fix_sibling_usage(opts)
    log.debug("Fixing sibling usage in:", opts.file_path)
    log.debug("New package:", opts.new_package)
    log.debug("Old type:", opts.old_type_name, "-> New type:", opts.new_type_name)

    -- Validate inputs
    if not opts.file_path or not opts.new_package or not opts.old_type_name or not opts.new_type_name then
        log.error("Missing required arguments")
        return false
    end

    -- Check if file exists
    if vim.fn.filereadable(opts.file_path) == 0 then
        log.error("File not found:", opts.file_path)
        return false
    end

    -- Check if the file uses this type at all
    local uses_type = os.execute(
        string.format(
            "rg -q '(^|[[:space:],;(}<])%s($|[[:space:],;(}\\.>])' '%s' 2>/dev/null",
            opts.old_type_name,
            opts.file_path
        )
    )

    if not (uses_type == 0 or uses_type == true) then
        log.debug("File doesn't use type:", opts.old_type_name)
        return true
    end

    log.info("File uses type:", opts.old_type_name, "- fixing imports and references")

    -- Determine the file's destination package
    local file_package
    if opts.file_dst_package then
        file_package = opts.file_dst_package
        log.debug("Using provided file package:", file_package)
    else
        local package_output = exec_and_read(
            string.format("rg -m1 '^package ' '%s' 2>/dev/null | sed 's/package \\(.*\\);/\\1/'", opts.file_path)
        )
        file_package = package_output and package_output:gsub("%s+", "") or ""
        log.debug("Extracted file package:", file_package)
    end

    -- Only add import if not in the same package
    if file_package ~= opts.new_package then
        log.debug("Different package, adding import")

        -- Find last import line
        local last_import_output =
            exec_and_read(string.format("rg -n '^import ' '%s' 2>/dev/null | tail -n 1 | cut -d: -f1", opts.file_path))
        local last_import_line = tonumber(last_import_output) or 2
        log.debug("Last import line:", last_import_line)

        -- Add import
        local import_line = string.format("import %s.%s;", opts.new_package, opts.new_type_name)
        log.info("Adding import:", import_line, "to", opts.file_path)

        add_import_line(opts.file_path, last_import_line, import_line)
    else
        log.debug("Same package, skipping import")
    end

    -- Always update type references if the name changed
    if opts.old_type_name ~= opts.new_type_name then
        log.info("Type name changed, updating references:", opts.old_type_name, "->", opts.new_type_name)

        local sed_cmd = string.format(
            "%s -i -E 's/([[:space:],;(}<])%s([[:space:],;(}\\.>])/\\1%s\\2/g' '%s'",
            sed,
            opts.old_type_name,
            opts.new_type_name,
            opts.file_path
        )

        local result = os.execute(sed_cmd)
        if not (result == 0 or result == true) then
            log.warn("Failed to update type references in:", opts.file_path)
            return false
        end

        log.debug("Successfully updated type references")
    end

    return true
end

return M