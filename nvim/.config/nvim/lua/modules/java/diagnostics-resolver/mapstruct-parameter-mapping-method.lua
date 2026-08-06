--- Resolver for MapStruct diagnostics suggesting a method for a whole mapper parameter.
---
--- Collection return types are resolved as separate raw and element types so imports
--- are planned from FQNs instead of delegated to an LSP code action.

local java_context = require("modules.java.diagnostics-resolver.java-context")
local java_import_resolver = require("modules.java.diagnostics-resolver.java-import-resolver")
local mapstruct = require("modules.java.mapstruct")

local M = {}

---@class MapStructParameterMappingMethod
---@field signature string
---@field source_type string
---@field source_parameter string
---@field target_type string
---@field target_property string
---@field container_type string
---@field element_type? string
---@field method_name string
---@field parameter_type string
---@field parameter_name string

--- Parse a diagnostic fragment containing a Java type followed by a name.
---@param value string
---@return string|nil type_name
---@return string|nil property_name
local function parse_typed_value(value)
    local type_name, property_name = value:match("^%s*(.-)%s+([%a_$][%w_$]*)%s*$")
    if not type_name or type_name == "" then
        return nil, nil
    end
    return vim.trim(type_name), property_name
end

--- Split a supported single-argument generic type into its container and element.
---@param type_name string
---@return string|nil container_type
---@return string|nil element_type
local function parse_single_parameterized_type(type_name)
    local compact = type_name:gsub("%s+", "")
    return compact:match("^([%a_$][%w_$.]*)<([%a_$][%w_$.]*)>$")
end

--- Compare Java type spellings while ignoring whitespace and nested-class separators.
---@param left string
---@param right string
---@return boolean
local function same_type(left, right)
    local function normalize(value)
        return value:gsub("%s+", ""):gsub("%$", ".")
    end

    return normalize(left) == normalize(right)
end

--- Parse a whole-parameter mapping method suggested by MapStruct.
---@param message string
---@return MapStructParameterMappingMethod|nil
function M.parse_suggested_method(message)
    local source, target = message:match('Can\'t map parameter%s+"([^"]+)"%s+to%s+"([^"]+)"')
    local raw_signature = message:match('Consider to declare/implement a mapping method:%s*"([^"]+)"')
    if not source or not target or not raw_signature then
        return nil
    end

    local source_type, source_parameter = parse_typed_value(source)
    local target_type, target_property = parse_typed_value(target)
    local signature = vim.trim(raw_signature)
    local return_type, method_name, parameters = signature:match("^(.+)%s+([%a_$][%w_$]*)%s*(%b())$")
    local parameter_type, parameter_name = nil, nil
    if parameters then
        parameter_type, parameter_name = parameters:match("^%(%s*(.-)%s+([%a_$][%w_$]*)%s*%)$")
    end
    if not source_type or not target_type or not return_type or not parameter_type then
        return nil
    end

    return_type = vim.trim(return_type)
    parameter_type = vim.trim(parameter_type)
    if not same_type(return_type, target_type) or not same_type(parameter_type, source_type) then
        return nil
    end

    local container_type, element_type = parse_single_parameterized_type(return_type)
    if return_type:find("<", 1, true) and not element_type then
        return nil
    end
    container_type = container_type or return_type

    return {
        signature = signature,
        source_type = source_type,
        source_parameter = source_parameter,
        target_type = target_type,
        target_property = target_property,
        container_type = container_type,
        element_type = element_type,
        method_name = method_name,
        parameter_type = parameter_type,
        parameter_name = parameter_name,
    }
end

--- Find and validate the source parameter resolved for the owning mapper method.
---@param sources table[]
---@param mapping MapStructParameterMappingMethod
---@return JavaResolvedType|nil
---@return string|nil
local function find_source_type(sources, mapping)
    for _, source in ipairs(sources or {}) do
        if source.name == mapping.source_parameter then
            local resolved = { className = source.type }
            if java_import_resolver.matches(resolved, mapping.source_type) then
                return resolved, nil
            end
            return nil,
                string.format(
                    "Mapper parameter '%s' does not match diagnostic type '%s'",
                    mapping.source_parameter,
                    mapping.source_type
                )
        end
    end

    return nil, "Could not find mapper parameter: " .. mapping.source_parameter
end

--- Resolve a diagnostic nested-type reference from the backend-resolved target root.
---@param target_root_type string
---@param element_reference string
---@return JavaResolvedType|nil
---@return string|nil
local function resolve_nested_element_type(target_root_type, element_reference)
    local root, descriptor_error = java_import_resolver.describe({ className = target_root_type })
    if not root then
        return nil, descriptor_error
    end

    local reference = element_reference:gsub("%s+", ""):gsub("%$", ".")
    local package_prefix = root.package_name ~= "" and (root.package_name .. ".") or ""
    if package_prefix ~= "" and vim.startswith(reference, package_prefix) then
        reference = reference:sub(#package_prefix + 1)
    end

    local root_reference = root.class_reference:gsub("%$", ".")
    local root_simple_name = root_reference:match("([^%.]+)$")
    local prefixes = { root_reference }
    if root_simple_name ~= root_reference then
        prefixes[#prefixes + 1] = root_simple_name
    end

    for _, prefix in ipairs(prefixes) do
        local nested_prefix = prefix .. "."
        if vim.startswith(reference, nested_prefix) then
            local suffix = reference:sub(#nested_prefix + 1)
            local segments = vim.split(suffix, ".", { plain = true })
            for _, segment in ipairs(segments) do
                if not segment:match("^[%a_$][%w_$]*$") then
                    return nil, "Invalid nested target type: " .. element_reference
                end
            end

            local resolved = {
                className = target_root_type .. "$" .. table.concat(segments, "$"),
                packageName = root.package_name,
            }
            if java_import_resolver.matches(resolved, element_reference) then
                return resolved, nil
            end
        end
    end

    return nil,
        string.format(
            "Target element type '%s' is not nested under resolved mapper target '%s'",
            element_reference,
            target_root_type
        )
end

--- Resolve every FQN needed to render the generated method signature.
---@param ctx { bufnr: integer, diagnostic: table }
---@param mapping MapStructParameterMappingMethod
---@param callback fun(result?: { parameter: JavaResolvedType, target: JavaResolvedType, element?: JavaResolvedType }, err?: string)
local function resolve_types(ctx, mapping, callback)
    mapstruct.get_method_types({
        bufnr = ctx.bufnr,
        row = ctx.diagnostic.lnum,
        col = ctx.diagnostic.col,
    }, function(method_types, method_error)
        if not method_types then
            callback(nil, method_error)
            return
        end

        local parameter, parameter_error = find_source_type(method_types.sources, mapping)
        if not parameter then
            callback(nil, parameter_error)
            return
        end

        local element = nil
        if mapping.element_type then
            local element_error
            element, element_error = resolve_nested_element_type(method_types.target_type, mapping.element_type)
            if not element then
                callback(nil, element_error)
                return
            end
        end

        mapstruct.resolve_path_type({
            sources = { { name = "$target", type = method_types.target_type } },
            path_expression = mapping.target_property:gsub("%.$", "") .. ".",
        }, function(target, target_error)
            if not target then
                callback(nil, target_error)
                return
            end
            if not java_import_resolver.matches(target, mapping.container_type) then
                callback(
                    nil,
                    string.format(
                        "Resolved type for target property '%s' does not match diagnostic type '%s'",
                        mapping.target_property,
                        mapping.container_type
                    )
                )
                return
            end

            callback({ parameter = parameter, target = target, element = element }, nil)
        end)
    end)
end

--- Check whether the generated signature is already present in the mapper buffer.
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

--- Insert the generated method and its directly planned imports into the mapper.
---@param bufnr integer
---@param diagnostic table
---@param mapping MapStructParameterMappingMethod
---@param resolved_types { parameter: JavaResolvedType, target: JavaResolvedType, element?: JavaResolvedType }
---@return boolean
local function insert_mapping_method(bufnr, diagnostic, mapping, resolved_types)
    local imports = {
        { key = "parameter", type = resolved_types.parameter },
        { key = "target", type = resolved_types.target },
    }
    if resolved_types.element then
        imports[#imports + 1] = { key = "element", type = resolved_types.element }
    end

    local references, imports_or_error = java_import_resolver.plan(bufnr, imports)
    if not references then
        vim.notify("[MapStruct] Could not plan parameter mapping imports: " .. imports_or_error, vim.log.levels.WARN)
        return false
    end

    local return_type = references.target
    if references.element then
        return_type = return_type .. "<" .. references.element .. ">"
    end
    local signature = return_type
        .. " "
        .. mapping.method_name
        .. "("
        .. references.parameter
        .. " "
        .. mapping.parameter_name
        .. ")"
    if method_exists(bufnr, signature) then
        vim.notify("[MapStruct] Parameter mapping method already exists: " .. signature, vim.log.levels.INFO)
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

    local modifier = owner:type() == "interface_declaration" and "default" or "protected"
    local member_indent = java_context.line_indent(bufnr, method:start())
    if member_indent == "" then
        member_indent = java_context.line_indent(bufnr, owner:start()) .. java_context.indent_unit(bufnr)
    end
    local body_indent = member_indent .. java_context.indent_unit(bufnr)

    local lines = {
        member_indent .. modifier .. " " .. signature .. " {",
        body_indent .. "return ;",
        member_indent .. "}",
    }
    local insert_row = java_context.insert_after_method(bufnr, method, lines)

    local inserted_import_lines = java_import_resolver.apply(bufnr, imports_or_error)
    local return_line = insert_row + 3 + inserted_import_lines
    local return_column = #body_indent + #"return "
    vim.api.nvim_win_set_cursor(0, { return_line, return_column })
    vim.notify("[MapStruct] Added parameter mapping method: " .. signature, vim.log.levels.INFO)
    vim.cmd("startinsert")
    return true
end

--- Resolve a MapStruct whole-parameter mapping-method diagnostic.
---@param ctx { bufnr: integer, diagnostic: table }
---@return boolean
function M.resolve(ctx)
    local mapping = M.parse_suggested_method(ctx.diagnostic.message or "")
    if not mapping then
        vim.notify("[MapStruct] Could not parse parameter mapping method", vim.log.levels.WARN)
        return false
    end

    resolve_types(ctx, mapping, function(resolved_types, err)
        if not resolved_types then
            vim.notify(
                "[MapStruct] Could not resolve parameter mapping method types: " .. (err or "unknown error"),
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
