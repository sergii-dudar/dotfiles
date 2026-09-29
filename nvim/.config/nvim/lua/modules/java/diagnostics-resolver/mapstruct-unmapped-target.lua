--- Resolver for MapStruct "Unmapped target property/properties" diagnostics.
---
--- It expands selected properties into `@Mapping` annotations and inserts them
--- above the method that owns the diagnostic.

local nio_util = require("utils.nio-util")
local java_context = require("modules.java.diagnostics-resolver.java-context")
local java_import_resolver = require("modules.java.diagnostics-resolver.java-import-resolver")

local M = {}

local MAPPING_IMPORT = "import org.mapstruct.Mapping;"
local MAPPING_TYPE = {
    className = "org.mapstruct.Mapping",
    packageName = "org.mapstruct",
}
local EMPTY_SOURCE = 'source = ""'
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
---@param annotation? string annotation reference, qualified when `Mapping` is shadowed
---@return string
function M.annotation_line(property, kind, annotation)
    annotation = annotation or "Mapping"
    if kind == "ignore" then
        return "@" .. annotation .. '(target = "' .. property .. '", ignore = true)'
    end
    return "@" .. annotation .. '(target = "' .. property .. '", source = "")'
end

--- Expand selected picker choices into annotation lines.
---@param selections table[]
---@param properties string[]
---@param annotation? string annotation reference, qualified when `Mapping` is shadowed
---@return string[]|nil lines
local function selected_annotations(selections, properties, annotation)
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
        lines[#lines + 1] = M.annotation_line(item.property, item.kind, annotation)
    end
    return lines
end

--- Plan how `@Mapping` is referenced and which import it needs.
--- The shared import planner keeps the regular / java / static import grouping.
---@param bufnr integer
---@return string annotation annotation reference to render
---@return string[] imports imports to apply once the annotations are inserted
local function plan_mapping_import(bufnr)
    local references, imports_or_error = java_import_resolver.plan(bufnr, { { key = "mapping", type = MAPPING_TYPE } })
    if not references then
        return "Mapping", {}
    end
    return references.mapping, imports_or_error
end

--- Insert missing MapStruct import when the file does not already contain it.
---@param bufnr integer
---@param imports? string[] planned imports, planned here when omitted
---@return integer inserted_line_count lines inserted above the mapper type
local function ensure_mapping_import(bufnr, imports)
    if not imports then
        local _, planned = plan_mapping_import(bufnr)
        imports = planned
    end
    if #imports == 0 then
        return 0
    end
    return java_import_resolver.apply(bufnr, imports)
end

--- Move the cursor into the first empty `source` so it can be completed right away.
---@param bufnr integer
---@param first_row integer zero-based row of the first inserted annotation
---@param inserted_lines string[]
local function focus_first_empty_source(bufnr, first_row, inserted_lines)
    if vim.api.nvim_get_current_buf() ~= bufnr then
        return
    end

    for offset, line in ipairs(inserted_lines) do
        -- `finish` is the closing quote; inserting there types between the quotes.
        local _, finish = line:find(EMPTY_SOURCE, 1, true)
        if finish then
            vim.api.nvim_win_set_cursor(0, { first_row + offset, finish - 1 })
            vim.cmd("startinsert")
            return
        end
    end
end

--- Insert annotations above the method owning the diagnostic.
---@param bufnr integer
---@param diagnostic table
---@param lines string[]
---@param imports? string[] planned imports for the rendered annotation
---@return boolean
local function insert_annotations(bufnr, diagnostic, lines, imports)
    local method = java_context.method_at_diagnostic(bufnr, diagnostic)
    if not method then
        vim.notify("[MapStruct] Could not find method for diagnostic", vim.log.levels.WARN)
        return false
    end

    local start_row = method:start()
    start_row = start_row + ensure_mapping_import(bufnr, imports)

    local indent = java_context.line_indent(bufnr, start_row)
    local insert_lines = vim.tbl_map(function(line)
        return indent .. line
    end, lines)
    vim.api.nvim_buf_set_lines(bufnr, start_row, start_row, false, insert_lines)
    vim.notify("[MapStruct] Added " .. tostring(#insert_lines) .. " mapping annotations", vim.log.levels.INFO)
    focus_first_empty_source(bufnr, start_row, insert_lines)
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

        local annotation, imports = plan_mapping_import(ctx.bufnr)
        local lines = selected_annotations(selections, properties, annotation)
        if not lines or #lines == 0 then
            return
        end
        insert_annotations(ctx.bufnr, ctx.diagnostic, lines, imports)
    end)
    return true
end

return M
