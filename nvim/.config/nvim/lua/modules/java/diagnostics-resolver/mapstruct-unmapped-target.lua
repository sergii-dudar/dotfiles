--- Resolver for MapStruct "Unmapped target properties" diagnostics.
---
--- It expands selected properties into `@Mapping` annotations and inserts them
--- above the method that owns the diagnostic.

local nio_util = require("utils.nio-util")

local M = {}

local MAPPING_IMPORT = "import org.mapstruct.Mapping;"

--- Parse unmapped MapStruct target properties from a diagnostic message.
---@param message string
---@return string[]
function M.parse_properties(message)
    local raw = message:match('Unmapped target properties:%s*"([^"]+)"')
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
        { name = "ignore all: mapstruct", kind = "ignore_all" },
        { name = "add mapping to all: mapstruct", kind = "map_all" },
    }

    for _, property in ipairs(properties) do
        choices[#choices + 1] = {
            name = "ignore `" .. property .. "`: mapstruct",
            kind = "ignore",
            property = property,
        }
    end

    for _, property in ipairs(properties) do
        choices[#choices + 1] = {
            name = "map `" .. property .. "`: mapstruct",
            kind = "map",
            property = property,
        }
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

--- Return the Java Tree-sitter root for a buffer.
---@param bufnr integer
---@return TSNode|nil
local function java_root(bufnr)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    return tree and tree:root() or nil
end

--- Find the method declaration owning a diagnostic position.
---@param bufnr integer
---@param diagnostic table
---@return TSNode|nil
local function diagnostic_method(bufnr, diagnostic)
    local root = java_root(bufnr)
    if not root then
        return nil
    end

    local row = diagnostic.lnum or vim.api.nvim_win_get_cursor(0)[1] - 1
    local col = diagnostic.col or 0
    local node = root:named_descendant_for_range(row, col, row, col)
    while node and node:type() ~= "method_declaration" do
        node = node:parent()
    end
    return node
end

--- Return the indentation prefix from the method declaration line.
---@param bufnr integer
---@param row integer
---@return string
local function method_indent(bufnr, row)
    local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1] or ""
    return line:match("^%s*") or ""
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
    local method = diagnostic_method(bufnr, diagnostic)
    if not method then
        vim.notify("[MapStruct] Could not find method for diagnostic", vim.log.levels.WARN)
        return false
    end

    local start_row = method:start()
    local import_row = ensure_mapping_import(bufnr)
    if import_row and import_row <= start_row then
        start_row = start_row + 1
    end

    local indent = method_indent(bufnr, start_row)
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
