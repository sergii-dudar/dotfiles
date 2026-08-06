--- Resolve generated MapStruct method types from the owning mapper method and property paths.

local java_import_resolver = require("modules.java.diagnostics-resolver.java-import-resolver")
local mapstruct = require("modules.java.mapstruct")

local M = {}

---@class MapStructDiagnosticMappingTypes
---@field source_type string
---@field source_property string
---@field target_type string
---@field target_property string

---@class MapStructParsedTypeExpression
---@field raw_type string
---@field argument_type? string

---@class MapStructResolvedTypeExpression
---@field type JavaResolvedType
---@field arguments JavaResolvedType[]

---@param property string
---@return string
local function completed_path(property)
    return property:gsub("%.$", "") .. "."
end

--- Parse a simple or single-argument Java type from a MapStruct diagnostic.
---@param type_name string
---@return MapStructParsedTypeExpression|nil
---@return string|nil
local function parse_type_expression(type_name)
    local compact = type_name:gsub("%s+", "")
    local raw_type, argument_type = compact:match("^([%a_$][%w_$.]*)<([%a_$][%w_$.]*)>$")
    if raw_type and argument_type then
        return { raw_type = raw_type, argument_type = argument_type }, nil
    end
    if compact:find("[<>]") then
        return nil, "Unsupported generic type expression: " .. type_name
    end
    return { raw_type = compact }, nil
end

--- Resolve and validate a property type, including one collection element type.
---@param source table
---@param property string
---@param expected_type string
---@param role "source"|"target"
---@param callback fun(result?: MapStructResolvedTypeExpression, err?: string)
local function resolve_property_type(source, property, expected_type, role, callback)
    local expression, expression_error = parse_type_expression(expected_type)
    if not expression then
        callback(nil, expression_error)
        return
    end

    local property_path = completed_path(property)
    mapstruct.resolve_path_type({
        sources = { source },
        path_expression = property_path,
    }, function(result, err)
        if not result then
            callback(nil, err)
            return
        end
        if not java_import_resolver.matches(result, expression.raw_type) then
            callback(
                nil,
                string.format(
                    "Resolved type for %s property '%s' does not match diagnostic type '%s'",
                    role,
                    property,
                    expression.raw_type
                )
            )
            return
        end
        if not expression.argument_type then
            callback({ type = result, arguments = {} }, nil)
            return
        end

        mapstruct.resolve_path_type({
            sources = { source },
            path_expression = property_path .. "first.",
        }, function(argument, argument_error)
            if not argument then
                callback(nil, argument_error)
                return
            end
            if not java_import_resolver.matches(argument, expression.argument_type) then
                callback(
                    nil,
                    string.format(
                        "Resolved element type for %s property '%s' does not match diagnostic type '%s'",
                        role,
                        property,
                        expression.argument_type
                    )
                )
                return
            end
            callback({ type = result, arguments = { argument } }, nil)
        end)
    end)
end

---@param sources table[]
---@param property string
---@param expected_type string
---@param callback fun(result?: MapStructResolvedTypeExpression, err?: string)
local function resolve_source_type(sources, property, expected_type, callback)
    local index = 1
    local last_error = nil

    local function try_next_source()
        local source = sources[index]
        if not source then
            callback(nil, last_error or "No mapper source contains property: " .. property)
            return
        end
        index = index + 1

        resolve_property_type(source, property, expected_type, "source", function(result, err)
            if result then
                callback(result, nil)
                return
            end

            last_error = err
            try_next_source()
        end)
    end

    try_next_source()
end

---@param target_type string
---@param property string
---@param expected_type string
---@param callback fun(result?: MapStructResolvedTypeExpression, err?: string)
local function resolve_target_type(target_type, property, expected_type, callback)
    resolve_property_type({ name = "$target", type = target_type }, property, expected_type, "target", callback)
end

--- Resolve both generated method types before the diagnostic resolver edits the buffer.
---@param ctx { bufnr: integer, diagnostic: table }
---@param mapping MapStructDiagnosticMappingTypes
---@param callback fun(result?: { source: JavaResolvedType, source_arguments: JavaResolvedType[], target: JavaResolvedType, target_arguments: JavaResolvedType[] }, err?: string)
function M.resolve(ctx, mapping, callback)
    mapstruct.get_method_types({
        bufnr = ctx.bufnr,
        row = ctx.diagnostic.lnum,
        col = ctx.diagnostic.col,
    }, function(method_types, method_err)
        if not method_types then
            callback(nil, method_err)
            return
        end

        resolve_source_type(
            method_types.sources,
            mapping.source_property,
            mapping.source_type,
            function(source_expression, source_err)
                if not source_expression then
                    callback(nil, source_err)
                    return
                end

                resolve_target_type(
                    method_types.target_type,
                    mapping.target_property,
                    mapping.target_type,
                    function(target_expression, target_err)
                        if not target_expression then
                            callback(nil, target_err)
                            return
                        end
                        callback({
                            source = source_expression.type,
                            source_arguments = source_expression.arguments,
                            target = target_expression.type,
                            target_arguments = target_expression.arguments,
                        }, nil)
                    end
                )
            end
        )
    end)
end

return M
