---@class SiblingUsageFixerOpts
---@field file_path string The file to fix
---@field new_package string The new package of the moved type
---@field old_type_name string The old name of the type
---@field new_type_name string The new name of the type
---@field file_dst_package? string Optional: The destination package of the file being fixed

local M = {}

local logging = require("utils.logging-util")
local consts = require("modules.java.refactor.constants")
local log = logging.new({ name = "sibling-usage-fixer", filename = "java-refactor.log" })

-- GNU sed (gsed on macOS), shell escaping, command helpers and the import writer live in constants.lua so that
-- this file and import-fixer.lua share one implementation.
local sed = consts.sed
local shell_escape = consts.shell_escape
local exec_and_read = consts.exec_and_read
local add_import_line = consts.add_import_line

-- Boundary patterns for matching Java type names (shared with init.lua logic)
local LEADING_BOUNDARY = consts.LEADING_BOUNDARY
local TRAILING_BOUNDARY = consts.TRAILING_BOUNDARY

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

    -- Check if the file uses this type at all.
    -- Check BOTH old and new names: this runs after the shell commands, which may already have renamed usages.
    local function uses_type_name(type_name)
        local result = os.execute(
            string.format(
                "rg -q '%s%s%s' %s 2>/dev/null",
                LEADING_BOUNDARY,
                type_name,
                TRAILING_BOUNDARY,
                shell_escape(opts.file_path)
            )
        )
        return result == 0 or result == true
    end
    local uses_type = uses_type_name(opts.old_type_name)
        or (opts.old_type_name ~= opts.new_type_name and uses_type_name(opts.new_type_name))

    if not uses_type then
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
            string.format(
                "rg -m1 '^package ' %s 2>/dev/null | sed 's/package \\(.*\\);/\\1/'",
                shell_escape(opts.file_path)
            )
        )
        file_package = package_output and package_output:gsub("%s+", "") or ""
        log.debug("Extracted file package:", file_package)
    end

    -- Only add import if not in the same package
    if file_package ~= opts.new_package then
        log.debug("Different package, adding import")

        local import_line = string.format("import %s.%s;", opts.new_package, opts.new_type_name)
        log.info("Adding import:", import_line, "to", opts.file_path)

        if not add_import_line(opts.file_path, import_line) then
            return false
        end
    else
        log.debug("Same package, skipping import")
    end

    -- Always update type references if the name changed
    if opts.old_type_name ~= opts.new_type_name then
        log.info("Type name changed, updating references:", opts.old_type_name, "->", opts.new_type_name)

        -- Double-pass sed for overlapping match handling
        local replace_expr = string.format(
            "s/%s%s%s/\\1%s\\2/g",
            LEADING_BOUNDARY,
            opts.old_type_name,
            TRAILING_BOUNDARY,
            opts.new_type_name
        )
        local sed_cmd =
            string.format("%s -i -E '%s; %s' %s", sed, replace_expr, replace_expr, shell_escape(opts.file_path))

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
