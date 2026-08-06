--- Resolver for MapStruct "Unmapped target property/properties" diagnostics.
---
--- It expands selected properties into `@Mapping` annotations and inserts them
--- above the method that owns the diagnostic.

local nio_util = require("utils.nio-util")
local java_context = require("modules.java.diagnostics-resolver.java-context")

local M = {}

local MAPPING_IMPORT = "import org.mapstruct.Mapping;"
local ADD_HL = "DiagnosticOk"
local IGNORE_HL = "DiagnosticWarn"
local PROPERTY_HL = "Identifier"

--- Build one resolver picker choice.
---@param message string
---@param kind string
---@param chunks table[]
---@param property? string
---@return table
local function choice(message, kind, chunks, property)
    return {
        name = message,
        kind = kind,
        property = property,
        chunks = chunks,
    }
end

--- Parse unmapped MapStruct target properties from a diagnostic message.
---@param message string
---@return string[]
function M.parse_properties(message)
    local raw = message:match('Unmapped target properties:%s*"([^"]+)"')
        or message:match('Unmapped target property:%s*"([^"]+)"')
    if not raw then
        return {}
    end

    local properties = {}
    for property in raw:gmatch("[^,]+") do
        local trimmed = vim.trim(property)
        if trimmed ~= "" then
            properties[#properties + 1] = trimmed
        end
    end
    return properties
end

--- Build picker choices for MapStruct unmapped target properties.
---@param properties string[]
---@return table[]
function M.build_choices(properties)
    local choices = {
        choice("Add all unmapped target properties", "map_all", {
            { "Add", ADD_HL },
            { " all unmapped target properties" },
        }),
    }

    for _, property in ipairs(properties) do
        choices[#choices + 1] = choice("Add unmapped target property: " .. property, "map", {
            { "Add", ADD_HL },
            { " unmapped target property: " },
            { property, PROPERTY_HL },
        }, property)
    end

    choices[#choices + 1] = choice("Ignore all unmapped target properties", "ignore_all", {
        { "Ignore", IGNORE_HL },
        { " all unmapped target properties" },
    })

    for _, property in ipairs(properties) do
        choices[#choices + 1] = choice("Ignore unmapped target property: " .. property, "ignore", {
            { "Ignore", IGNORE_HL },
            { " unmapped target property: " },
            { property, PROPERTY_HL },
        }, property)
    end

    return choices
end

--- Render one MapStruct `@Mapping` annotation.
---@param property string
---@param kind "ignore"|"map"
---@return string
function M.annotation_line(property, kind)
    if kind == "ignore" then
        return '@Mapping(target = "' .. property .. '", ignore = true)'
    end
    return '@Mapping(target = "' .. property .. '", source = "")'
end

--- Expand selected picker choices into annotation lines.
---@param selections table[]
---@param properties string[]
---@return string[]|nil lines
local function selected_annotations(selections, properties)
    local by_property = {}
    local ordered = {}

    --- Add one requested action, rejecting conflicting actions for a property.
    ---@param property string
    ---@param kind "ignore"|"map"
    ---@return boolean
    local function add(property, kind)
        local existing = by_property[property]
        if existing and existing ~= kind then
            vim.notify("[MapStruct] Conflicting actions selected for `" .. property .. "`", vim.log.levels.WARN)
            return false
        end
        if not existing then
            by_property[property] = kind
            ordered[#ordered + 1] = { property = property, kind = kind }
        end
        return true
    end

    for _, selection in ipairs(selections) do
        if selection.kind == "ignore_all" or selection.kind == "map_all" then
            local kind = selection.kind == "ignore_all" and "ignore" or "map"
            for _, property in ipairs(properties) do
                if not add(property, kind) then
                    return nil
                end
            end
        elseif selection.property then
            if not add(selection.property, selection.kind) then
                return nil
            end
        end
    end

    local lines = {}
    for _, item in ipairs(ordered) do
        lines[#lines + 1] = M.annotation_line(item.property, item.kind)
    end
    return lines
end

--- Insert missing MapStruct import when the file does not already contain it.
---@param bufnr integer
---@return integer|nil inserted_row zero-based row where the import was inserted
local function ensure_mapping_import(bufnr)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    for _, line in ipairs(lines) do
        if line == MAPPING_IMPORT or line == "import org.mapstruct.*;" then
            return nil
        end
    end

    local last_import_row = nil
    local package_row = nil
    for index, line in ipairs(lines) do
        if line:match("^import%s+") then
            last_import_row = index
        elseif line:match("^package%s+") then
            package_row = index
        end
    end

    local insert_row = last_import_row or package_row or 0
    vim.api.nvim_buf_set_lines(bufnr, insert_row, insert_row, false, { MAPPING_IMPORT })
    return insert_row
end

--- Insert annotations above the method owning the diagnostic.
---@param bufnr integer
---@param diagnostic table
---@param lines string[]
---@return boolean
local function insert_annotations(bufnr, diagnostic, lines)
    local method = java_context.method_at_diagnostic(bufnr, diagnostic)
    if not method then
        vim.notify("[MapStruct] Could not find method for diagnostic", vim.log.levels.WARN)
        return false
    end

    local start_row = method:start()
    local import_row = ensure_mapping_import(bufnr)
    if import_row and import_row <= start_row then
        start_row = start_row + 1
    end

    local indent = java_context.line_indent(bufnr, start_row)
    local insert_lines = vim.tbl_map(function(line)
        return indent .. line
    end, lines)
    vim.api.nvim_buf_set_lines(bufnr, start_row, start_row, false, insert_lines)
    vim.notify("[MapStruct] Added " .. tostring(#insert_lines) .. " mapping annotations", vim.log.levels.INFO)
    return true
end

--- Resolve a MapStruct unmapped target diagnostic.
---@param ctx { bufnr: integer, diagnostic: table }
---@return boolean|nil
function M.resolve(ctx)
    local properties = M.parse_properties(ctx.diagnostic.message or "")
    if #properties == 0 then
        vim.notify("[MapStruct] Could not parse unmapped target properties", vim.log.levels.WARN)
        return false
    end

    nio_util.run(function()
        local selections = nio_util.multi_select(M.build_choices(properties), "MapStruct unmapped target properties")
        if not selections then
            return
        end

        local lines = selected_annotations(selections, properties)
        if not lines or #lines == 0 then
            return
        end
        insert_annotations(ctx.bufnr, ctx.diagnostic, lines)
    end)
    return true
end

return M
