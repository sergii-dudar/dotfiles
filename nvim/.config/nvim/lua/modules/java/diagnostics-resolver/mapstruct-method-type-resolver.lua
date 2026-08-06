--- Resolve generated MapStruct method types from the owning mapper method and property paths.

local java_import_resolver = require("modules.java.diagnostics-resolver.java-import-resolver")
local mapstruct = require("modules.java.mapstruct")

local M = {}

---@class MapStructDiagnosticMappingTypes
---@field source_type string
---@field source_property string
---@field target_type string
---@field target_property string

---@param property string
---@return string
local function completed_path(property)
    return property:gsub("%.$", "") .. "."
end

---@param sources table[]
---@param property string
---@param expected_type string
---@param callback fun(result?: JavaResolvedType, err?: string)
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

        mapstruct.resolve_path_type({
            sources = { source },
            path_expression = completed_path(property),
        }, function(result, err)
            if result and java_import_resolver.matches(result, expected_type) then
                callback(result, nil)
                return
            end

            last_error = err
                or string.format(
                    "Resolved type for source property '%s' does not match diagnostic type '%s'",
                    property,
                    expected_type
                )
            try_next_source()
        end)
    end

    try_next_source()
end

---@param target_type string
---@param property string
---@param expected_type string
---@param callback fun(result?: JavaResolvedType, err?: string)
local function resolve_target_type(target_type, property, expected_type, callback)
    mapstruct.resolve_path_type({
        sources = { { name = "$target", type = target_type } },
        path_expression = completed_path(property),
    }, function(result, err)
        if not result then
            callback(nil, err)
            return
        end
        if not java_import_resolver.matches(result, expected_type) then
            callback(
                nil,
                string.format(
                    "Resolved type for target property '%s' does not match diagnostic type '%s'",
                    property,
                    expected_type
                )
            )
            return
        end
        callback(result, nil)
    end)
end

--- Resolve both generated method types before the diagnostic resolver edits the buffer.
---@param ctx { bufnr: integer, diagnostic: table }
---@param mapping MapStructDiagnosticMappingTypes
---@param callback fun(result?: { source: JavaResolvedType, target: JavaResolvedType }, err?: string)
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
            function(source, source_err)
                if not source then
                    callback(nil, source_err)
                    return
                end

                resolve_target_type(
                    method_types.target_type,
                    mapping.target_property,
                    mapping.target_type,
                    function(target, target_err)
                        if not target then
                            callback(nil, target_err)
                            return
                        end
                        callback({ source = source, target = target }, nil)
                    end
                )
            end
        )
    end)
end

return M
