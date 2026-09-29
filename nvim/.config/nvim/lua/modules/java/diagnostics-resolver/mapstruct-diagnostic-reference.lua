--- Shared parsing for the typed references MapStruct prints in its diagnostics.
---
--- MapStruct describes a mapping endpoint as `<kind> "<Type> <path>"`. The path is
--- more than a plain identifier:
---   * nested sources are dotted and start with the parameter name (`source.config.ttl`)
---   * forged mappings carry collection markers (`box.parts[].age`)
---   * map mappings carry key/value markers (`box.byName{:value}.age`)
--- and `<kind>` is `property` or an element kind such as `Collection element`.

local M = {}

M.PROPERTY_KIND = "property"

local COLLECTION_ELEMENT_KINDS = {
    ["Collection element"] = true,
    ["Stream element"] = true,
}

local MAP_ELEMENT_KINDS = {
    ["Map key"] = true,
    ["Map value"] = true,
}

local JAVA_KEYWORDS = {
    abstract = true,
    assert = true,
    boolean = true,
    ["break"] = true,
    byte = true,
    case = true,
    catch = true,
    char = true,
    class = true,
    const = true,
    continue = true,
    default = true,
    ["do"] = true,
    double = true,
    ["else"] = true,
    enum = true,
    extends = true,
    final = true,
    finally = true,
    float = true,
    ["for"] = true,
    ["goto"] = true,
    ["if"] = true,
    implements = true,
    import = true,
    instanceof = true,
    int = true,
    interface = true,
    long = true,
    native = true,
    new = true,
    package = true,
    private = true,
    protected = true,
    public = true,
    ["return"] = true,
    short = true,
    static = true,
    strictfp = true,
    super = true,
    switch = true,
    synchronized = true,
    this = true,
    throw = true,
    throws = true,
    transient = true,
    try = true,
    void = true,
    volatile = true,
    ["while"] = true,
    ["true"] = true,
    ["false"] = true,
    null = true,
}

--- Check whether a MapStruct element kind names a collection, stream, or map element.
---@param kind string|nil
---@return boolean
function M.is_element_kind(kind)
    return COLLECTION_ELEMENT_KINDS[kind] == true or MAP_ELEMENT_KINDS[kind] == true
end

--- Normalize a captured element kind, hiding the default `property` kind.
--- Flat property diagnostics keep their original parse result because of this.
---@param kind string|nil
---@return string|nil element_kind nil for plain properties
function M.element_kind(kind)
    kind = kind and vim.trim(kind) or nil
    if not kind or kind == "" or kind == M.PROPERTY_KIND then
        return nil
    end
    return kind
end

--- Parse a typed reference such as `Account debtorAccount` or `Duration box.parts[].age`.
---@param value string
---@return string|nil type_name
---@return string|nil path
function M.parse_typed_reference(value)
    local type_name, path = value:match("^%s*(.-)%s+([%a_$][%w_$%.%[%]{}:]*)%s*$")
    if not type_name or type_name == "" then
        return nil, nil
    end
    return type_name, path
end

--- Return the final Java identifier from a possibly qualified or generic type.
---@param type_name string
---@return string|nil
function M.simple_type_name(type_name)
    local result = nil
    for identifier in type_name:gmatch("[%a_$][%w_$]*") do
        result = identifier
    end
    return result
end

--- Return the last property name of a MapStruct path.
--- `source.config.ttl` -> `ttl`, `box.parts[].age` -> `age`, `wordMap{:key}` -> `wordMap`.
---@param path string
---@return string|nil
function M.path_name(path)
    local result = nil
    for identifier in path:gsub("{:?%a*}", ""):gmatch("[%a_$][%w_$]*") do
        result = identifier
    end
    return result
end

--- Build a parameter name for one element of a collection from its type.
--- `Model.Wheel` -> `wheel`; Java keywords fall back to `value`.
---@param type_name string
---@return string
function M.element_name(type_name)
    local simple_name = M.simple_type_name(type_name)
    if not simple_name then
        return "value"
    end

    local name = simple_name:sub(1, 1):lower() .. simple_name:sub(2)
    if JAVA_KEYWORDS[name] then
        return "value"
    end
    return name
end

--- Build the `to<Type>` method name used for generated mapping methods.
---@param type_name string
---@return string|nil
function M.method_name_for(type_name)
    local simple_name = M.simple_type_name(type_name)
    if not simple_name then
        return nil
    end
    return "to" .. simple_name:sub(1, 1):upper() .. simple_name:sub(2)
end

--- Translate a MapStruct path into the path syntax understood by the MapStruct backend.
--- Collection markers become the backend's synthetic `first` element accessor; map key
--- and value markers become segments of their own (`words{:value}` -> `words.{:value}`).
---@param path string
---@param kind? string element kind printed by MapStruct
---@return string|nil backend_path
---@return string|nil error
function M.backend_path(path, kind)
    if type(path) ~= "string" or path == "" then
        return nil, "MapStruct diagnostic has no property path"
    end

    local result = path:gsub("%[%]", ".first"):gsub("{:(%a+)}", ".{:%1}")
    -- `{}` names a map as a whole: there is no single type behind it.
    if result:find("{}", 1, true) then
        return nil, "Unsupported MapStruct path: " .. path
    end

    if COLLECTION_ELEMENT_KINDS[kind] then
        result = result:gsub("%.$", "") .. ".first"
    elseif MAP_ELEMENT_KINDS[kind] then
        -- MapStruct already ends the path with the marker; add it only when it is missing.
        local marker = kind == "Map key" and "{:key}" or "{:value}"
        if result:sub(-#marker) ~= marker then
            result = result:gsub("%.$", "") .. "." .. marker
        end
    end
    return result, nil
end

--- Explain a backend result that lost the element type of a collection or map.
--- The element type comes from the generic type of the declaring field, so a raw
--- collection yields `java.lang.Object`, and so does a backend older than the one
--- that resolves sets, streams, and maps.
---@param resolved { className?: string }|nil backend result for the path
---@param backend_path string path that was resolved
---@return string|nil explanation nil when the result is not a lost element type
function M.unresolved_element(resolved, backend_path)
    if type(resolved) ~= "table" or resolved.className ~= "java.lang.Object" then
        return nil
    end
    if type(backend_path) ~= "string" then
        return nil
    end

    local container = backend_path:match("^(.-)%.first%.?$") or backend_path:match("^(.-)%.{:%a+}%.?$")
    if not container then
        return nil
    end
    return string.format(
        "MapStruct backend could not determine the element type of '%s' "
            .. "(raw collection or map, or an outdated mapstruct-path-explorer.jar)",
        container
    )
end

--- Add a hint to a backend error for a path that needs map key/value navigation.
--- A backend without that navigation reports such a path as unresolved.
---@param backend_path string path that was resolved
---@param err string|nil backend error
---@return string|nil
function M.explain_unresolved(backend_path, err)
    if type(backend_path) == "string" and backend_path:find("{:", 1, true) then
        return (err or "MapStruct did not resolve a type for path: " .. backend_path)
            .. " (map key/value paths need an up-to-date mapstruct-path-explorer.jar)"
    end
    return err
end

return M
