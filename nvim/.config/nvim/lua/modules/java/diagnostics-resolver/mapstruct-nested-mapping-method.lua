--- Resolver for MapStruct diagnostics produced by forged nested mappings.
---
--- It turns the source and target types from the diagnostic into an explicit
--- abstract mapping method so subsequent property diagnostics can be resolved
--- directly on that method.

local java_context = require("modules.java.diagnostics-resolver.java-context")
local java_import_resolver = require("modules.java.diagnostics-resolver.java-import-resolver")
local mapstruct_method_type_resolver = require("modules.java.diagnostics-resolver.mapstruct-method-type-resolver")

local M = {}

---@class MapStructNestedMappingMethod
---@field unmapped_property string
---@field source_type string
---@field source_property string
---@field target_type string
---@field target_property string
---@field method_name string
---@field signature string

--- Parse a Java typed-property fragment such as `Account debtorAccount`.
---@param value string
---@return string|nil type_name
---@return string|nil property_name
local function parse_typed_property(value)
    local type_name, property_name = value:match("^%s*(.-)%s+([%a_$][%w_$]*)%s*$")
    if not type_name or type_name == "" then
        return nil, nil
    end
    return type_name, property_name
end

--- Return the final Java identifier from a possibly qualified or generic type.
---@param type_name string
---@return string|nil
local function simple_type_name(type_name)
    local result = nil
    for identifier in type_name:gmatch("[%a_$][%w_$]*") do
        result = identifier
    end
    return result
end

--- Parse a MapStruct forged nested-mapping diagnostic.
---@param message string
---@return MapStructNestedMappingMethod|nil
function M.parse_mapping(message)
    local unmapped_property = message:match('^Unmapped target property:%s*"([^"]+)"')
    local source, target = message:match('Mapping from property%s*"([^"]+)"%s+to%s+"([^"]+)"')
    if not unmapped_property or not source or not target then
        return nil
    end

    local source_type, source_property = parse_typed_property(source)
    local target_type, target_property = parse_typed_property(target)
    local target_simple_name = target_type and simple_type_name(target_type)
    if not source_type or not target_type or not target_simple_name then
        return nil
    end

    local method_name = "to" .. target_simple_name:sub(1, 1):upper() .. target_simple_name:sub(2)
    return {
        unmapped_property = unmapped_property,
        source_type = source_type,
        source_property = source_property,
        target_type = target_type,
        target_property = target_property,
        method_name = method_name,
        signature = target_type .. " " .. method_name .. "(" .. source_type .. " " .. source_property .. ")",
    }
end

--- Check whether the generated nested mapping signature already exists.
---@param bufnr integer
---@param signature string
---@return boolean
local function method_exists(bufnr, signature)
    for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)) do
        if line:find(signature, 1, true) then
            return true
        end
    end
    return false
end

--- Insert an abstract nested mapping declaration into the owning mapper type.
---@param bufnr integer
---@param diagnostic table
---@param mapping MapStructNestedMappingMethod
---@param resolved_types { source: JavaResolvedType, target: JavaResolvedType }
---@return boolean
local function insert_mapping_method(bufnr, diagnostic, mapping, resolved_types)
    local references, imports_or_error = java_import_resolver.plan(bufnr, {
        { key = "source", type = resolved_types.source },
        { key = "target", type = resolved_types.target },
    })
    if not references then
        vim.notify("[MapStruct] Could not plan nested mapping imports: " .. imports_or_error, vim.log.levels.WARN)
        return false
    end

    local signature = references.target
        .. " "
        .. mapping.method_name
        .. "("
        .. references.source
        .. " "
        .. mapping.source_property
        .. ")"
    if method_exists(bufnr, signature) then
        vim.notify("[MapStruct] Nested mapping method already exists: " .. signature, vim.log.levels.INFO)
        return false
    end

    local method = java_context.method_at_diagnostic(bufnr, diagnostic)
    if not method then
        vim.notify("[MapStruct] Could not find method for diagnostic", vim.log.levels.WARN)
        return false
    end

    local owner = java_context.enclosing_type(method)
    if not owner then
        vim.notify("[MapStruct] Could not find mapper type for diagnostic", vim.log.levels.WARN)
        return false
    end

    local member_indent = java_context.line_indent(bufnr, method:start())
    if member_indent == "" then
        member_indent = java_context.line_indent(bufnr, owner:start()) .. java_context.indent_unit(bufnr)
    end

    local modifier = owner:type() == "interface_declaration" and "" or "protected abstract "
    local declaration = member_indent .. modifier .. signature .. ";"
    local _, _, owner_end_row = owner:range()
    vim.api.nvim_buf_set_lines(bufnr, owner_end_row, owner_end_row, false, { "", declaration })

    local inserted_import_lines = java_import_resolver.apply(bufnr, imports_or_error)
    local method_line = owner_end_row + 2 + inserted_import_lines
    local method_name_start = declaration:find(mapping.method_name, 1, true)
    vim.api.nvim_win_set_cursor(0, { method_line, method_name_start and method_name_start - 1 or 0 })
    vim.notify("[MapStruct] Added nested mapping method: " .. signature, vim.log.levels.INFO)
    return true
end

--- Resolve a MapStruct forged nested-mapping diagnostic.
---@param ctx { bufnr: integer, diagnostic: table }
---@return boolean
function M.resolve(ctx)
    local mapping = M.parse_mapping(ctx.diagnostic.message or "")
    if not mapping then
        vim.notify("[MapStruct] Could not parse nested mapping types", vim.log.levels.WARN)
        return false
    end

    mapstruct_method_type_resolver.resolve(ctx, mapping, function(resolved_types, err)
        if not resolved_types then
            vim.notify(
                "[MapStruct] Could not resolve nested mapping types: " .. (err or "unknown error"),
                vim.log.levels.WARN
            )
            return
        end
        if vim.api.nvim_get_current_buf() ~= ctx.bufnr then
            vim.notify("[MapStruct] Mapper buffer is no longer active", vim.log.levels.WARN)
            return
        end
        insert_mapping_method(ctx.bufnr, ctx.diagnostic, mapping, resolved_types)
    end)
    return true
end

return M
