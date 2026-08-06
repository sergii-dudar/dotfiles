--- Resolver for MapStruct diagnostics requiring explicit enum value mappings.

local java_context = require("modules.java.diagnostics-resolver.java-context")
local java_import_resolver = require("modules.java.diagnostics-resolver.java-import-resolver")
local mapstruct_method_type_resolver = require("modules.java.diagnostics-resolver.mapstruct-method-type-resolver")

local M = {}

local VALUE_MAPPING_TYPE = {
    className = "org.mapstruct.ValueMapping",
    packageName = "org.mapstruct",
}

---@class MapStructEnumMappingMethod
---@field source_type string
---@field source_property string
---@field target_type string
---@field target_property string
---@field constants string[]
---@field method_name string
---@field signature string

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

---@param type_name string
---@return string|nil
local function simple_type_name(type_name)
    local result = nil
    for identifier in type_name:gmatch("[%a_$][%w_$]*") do
        result = identifier
    end
    return result
end

--- Parse a MapStruct enum constants diagnostic.
---@param message string
---@return MapStructEnumMappingMethod|nil
function M.parse_mapping(message)
    local source, target, constant_list = message:match(
        '^The following constants from the property%s+"([^"]+)"%s+enum have no corresponding constant in the%s+"([^"]+)"%s+enum and must .-additional mappings:%s*(.-)%.%s*$'
    )
    if not source or not target or not constant_list then
        return nil
    end

    local source_type, source_property = parse_typed_property(source)
    local target_type, target_property = parse_typed_property(target)
    local target_simple_name = target_type and simple_type_name(target_type)
    if not source_type or not target_type or not target_simple_name then
        return nil
    end

    local constants = {}
    for value in constant_list:gmatch("[^,]+") do
        local constant = vim.trim(value)
        if not constant:match("^[%a_$][%w_$]*$") then
            return nil
        end
        constants[#constants + 1] = constant
    end
    if #constants == 0 then
        return nil
    end

    local method_name = "to" .. target_simple_name:sub(1, 1):upper() .. target_simple_name:sub(2)
    return {
        source_type = source_type,
        source_property = source_property,
        target_type = target_type,
        target_property = target_property,
        constants = constants,
        method_name = method_name,
        signature = target_type .. " " .. method_name .. "(" .. source_type .. " " .. source_property .. ")",
    }
end

---@param bufnr integer
---@param signature string
---@return boolean
local function method_exists(bufnr, signature)
    for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)) do
        if not line:match("^%s*//") and line:find(signature, 1, true) then
            return true
        end
    end
    return false
end

---@param bufnr integer
---@param diagnostic table
---@param mapping MapStructEnumMappingMethod
---@param resolved_types { source: JavaResolvedType, target: JavaResolvedType }
---@return boolean
local function insert_mapping_method(bufnr, diagnostic, mapping, resolved_types)
    local references, imports_or_error = java_import_resolver.plan(bufnr, {
        { key = "source", type = resolved_types.source },
        { key = "target", type = resolved_types.target },
        { key = "annotation", type = VALUE_MAPPING_TYPE },
    })
    if not references then
        vim.notify("[MapStruct] Could not plan enum mapping imports: " .. imports_or_error, vim.log.levels.WARN)
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
        vim.notify("[MapStruct] Enum mapping method already exists: " .. signature, vim.log.levels.INFO)
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

    local lines = { "" }
    for _, constant in ipairs(mapping.constants) do
        lines[#lines + 1] = member_indent
            .. "@"
            .. references.annotation
            .. '(target = "", source = "'
            .. constant
            .. '")'
    end

    local modifier = owner:type() == "interface_declaration" and "" or "protected abstract "
    lines[#lines + 1] = member_indent .. modifier .. signature .. ";"

    local _, _, owner_end_row = owner:range()
    vim.api.nvim_buf_set_lines(bufnr, owner_end_row, owner_end_row, false, lines)

    local inserted_import_lines = java_import_resolver.apply(bufnr, imports_or_error)
    local first_mapping_line = owner_end_row + 2 + inserted_import_lines
    local target_value_column = #member_indent + #("@" .. references.annotation .. '(target = "')
    vim.api.nvim_win_set_cursor(0, { first_mapping_line, target_value_column })
    vim.notify(
        string.format("[MapStruct] Added enum mapping method with %d value mappings", #mapping.constants),
        vim.log.levels.INFO
    )
    vim.cmd("startinsert")
    return true
end

--- Resolve a MapStruct enum constants diagnostic.
---@param ctx { bufnr: integer, diagnostic: table }
---@return boolean
function M.resolve(ctx)
    local mapping = M.parse_mapping(ctx.diagnostic.message or "")
    if not mapping then
        vim.notify("[MapStruct] Could not parse enum mapping diagnostic", vim.log.levels.WARN)
        return false
    end

    mapstruct_method_type_resolver.resolve(ctx, mapping, function(resolved_types, err)
        if not resolved_types then
            vim.notify(
                "[MapStruct] Could not resolve enum mapping types: " .. (err or "unknown error"),
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
