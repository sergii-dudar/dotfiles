--- Resolver for plural unmapped-target diagnostics produced by a forged nested mapping.
---
--- The property list belongs to the forged target type, so resolving it on the
--- owning mapper method would add annotations at the wrong level. This resolver
--- generates the explicit nested mapping declaration first.

local nested_mapping_method = require("modules.java.diagnostics-resolver.mapstruct-nested-mapping-method")

local M = {}

---@class MapStructNestedPropertiesMappingMethod: MapStructNestedTypeMapping
---@field unmapped_properties string[]

--- Parse and validate the plural unmapped-property list.
---@param message string
---@return string[]
local function parse_properties(message)
    local raw = message:match('^Unmapped target properties:%s*"([^"]+)"%.%s+Mapping from property')
    if not raw then
        return {}
    end

    local properties = {}
    for property in raw:gmatch("[^,]+") do
        local name = vim.trim(property)
        if not name:match("^[%a_$][%w_$]*$") then
            return {}
        end
        properties[#properties + 1] = name
    end
    return properties
end

--- Parse a plural MapStruct forged nested-mapping diagnostic.
---@param message string
---@return MapStructNestedPropertiesMappingMethod|nil
function M.parse_mapping(message)
    local properties = parse_properties(message)
    if #properties == 0 then
        return nil
    end

    local mapping = nested_mapping_method.parse_type_mapping(message)
    if not mapping then
        return nil
    end
    mapping.unmapped_properties = properties
    return mapping
end

--- Resolve a plural MapStruct forged nested-mapping diagnostic.
---@param ctx { bufnr: integer, diagnostic: table }
---@return boolean
function M.resolve(ctx)
    local mapping = M.parse_mapping(ctx.diagnostic.message or "")
    if not mapping then
        vim.notify("[MapStruct] Could not parse nested target properties mapping types", vim.log.levels.WARN)
        return false
    end

    return nested_mapping_method.resolve_mapping(ctx, mapping)
end

return M
