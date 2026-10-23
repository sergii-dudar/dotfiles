--- Resolver for MapStruct diagnostics produced by forged nested mappings.
---
--- It turns the source and target types from the diagnostic into an explicit
--- abstract mapping method so subsequent property diagnostics can be resolved
--- directly on that method. Nested target annotations of the property move into
--- the generated method, because MapStruct keeps forging while they stay on the
--- owning method. For a plain property the user may instead map or ignore the
--- unmapped properties through their nested target path.

local java_context = require("modules.java.diagnostics-resolver.java-context")
local java_import_resolver = require("modules.java.diagnostics-resolver.java-import-resolver")
local mapstruct_method_type_resolver = require("modules.java.diagnostics-resolver.mapstruct-method-type-resolver")
local mapstruct_nested_target_mappings = require("modules.java.diagnostics-resolver.mapstruct-nested-target-mappings")
local mapstruct_reference = require("modules.java.diagnostics-resolver.mapstruct-diagnostic-reference")

local M = {}

---@class MapStructNestedTypeMapping
---@field source_type string
---@field source_property string MapStruct path, e.g. `debtorAccount` or `house.roof.color`
---@field target_type string
---@field target_property string MapStruct path
---@field method_name string
---@field signature string
---@field element_kind? string MapStruct element kind when the mapping is not a plain property

---@class MapStructNestedMappingMethod: MapStructNestedTypeMapping
---@field unmapped_property string

---@class MapStructNestedMappingChoice
---@field kind "generate"|"properties"
---@field label string

--- Parse a Java typed-property fragment such as `Account debtorAccount`.
---@param value string
---@return string|nil type_name
---@return string|nil property_name
local function parse_typed_property(value)
    return mapstruct_reference.parse_typed_reference(value)
end

--- Return the final Java identifier from a possibly qualified or generic type.
---@param type_name string
---@return string|nil
local function simple_type_name(type_name)
    return mapstruct_reference.simple_type_name(type_name)
end

--- Name the generated method parameter after the mapped source.
--- A collection element is named after its type, because its path names the collection.
---@param mapping { source_type: string, source_property: string, element_kind?: string }
---@return string
local function parameter_name(mapping)
    if mapping.element_kind then
        return mapstruct_reference.element_name(mapping.source_type)
    end
    return mapstruct_reference.path_name(mapping.source_property) or mapping.source_property
end

--- Parse the source and target endpoints of a MapStruct forged nested mapping.
---@param message string
---@return MapStructNestedTypeMapping|nil
function M.parse_type_mapping(message)
    local kind, source, target = message:match('Mapping from ([%a ]-)%s*"([^"]+)"%s+to%s+"([^"]+)"')
    if not source or not target then
        return nil
    end
    if kind ~= mapstruct_reference.PROPERTY_KIND and not mapstruct_reference.is_element_kind(kind) then
        return nil
    end

    local source_type, source_property = parse_typed_property(source)
    local target_type, target_property = parse_typed_property(target)
    local target_simple_name = target_type and simple_type_name(target_type)
    if not source_type or not target_type or not target_simple_name then
        return nil
    end
    local element_kind = mapstruct_reference.element_kind(kind)

    local method_name = "to" .. target_simple_name:sub(1, 1):upper() .. target_simple_name:sub(2)
    local source_name = parameter_name({
        source_type = source_type,
        source_property = source_property,
        element_kind = element_kind,
    })
    return {
        source_type = source_type,
        source_property = source_property,
        target_type = target_type,
        target_property = target_property,
        method_name = method_name,
        signature = target_type .. " " .. method_name .. "(" .. source_type .. " " .. source_name .. ")",
        element_kind = element_kind,
    }
end

--- Parse a singular MapStruct forged nested-mapping diagnostic.
---@param message string
---@return MapStructNestedMappingMethod|nil
function M.parse_mapping(message)
    local unmapped_property = message:match('^Unmapped target property:%s*"([^"]+)"')
    if not unmapped_property then
        return nil
    end

    local mapping = M.parse_type_mapping(message)
    if not mapping then
        return nil
    end
    mapping.unmapped_property = unmapped_property
    return mapping
end

--- Check whether the generated nested mapping signature already exists.
---@param bufnr integer
---@param signature string
---@return boolean
local function method_exists(bufnr, signature)
    return java_context.method_exists(bufnr, signature)
end

--- Insert an abstract nested mapping declaration into the owning mapper type.
--- Nested target annotations of the property move into the declaration when the
--- owning method carries any.
---@param bufnr integer
---@param diagnostic table
---@param mapping MapStructNestedTypeMapping
---@param resolved_types { source: JavaResolvedType, target: JavaResolvedType }
---@param group? MapStructNestedTargetGroup `@Mapping` annotations of the owning method addressing the property
---@param source_path? string shared source prefix the owning method maps the property from
---@return boolean
local function insert_mapping_method(bufnr, diagnostic, mapping, resolved_types, group, source_path)
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
        .. parameter_name(mapping)
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

    local moving = group ~= nil and source_path ~= nil and #group.mappings > 0
    if moving then
        local movable, reason = mapstruct_nested_target_mappings.movable(bufnr, group)
        if not movable then
            vim.notify("[MapStruct] Cannot move nested mappings: " .. reason, vim.log.levels.WARN)
            return false
        end
    end

    local member_indent = java_context.line_indent(bufnr, method:start())
    if member_indent == "" then
        member_indent = java_context.line_indent(bufnr, owner:start()) .. java_context.indent_unit(bufnr)
    end

    local modifier = owner:type() == "interface_declaration" and "" or "protected abstract "
    local declaration = member_indent .. modifier .. signature .. ";"
    local lines = {}
    local moved_expressions = 0
    if moving then
        local moved =
            mapstruct_nested_target_mappings.moved_annotations(bufnr, group, source_path, parameter_name(mapping))
        for index, annotation_lines in ipairs(moved) do
            annotation_lines[1] = member_indent .. annotation_lines[1]
            vim.list_extend(lines, annotation_lines)
            if group.mappings[index].has_expression then
                moved_expressions = moved_expressions + 1
            end
        end
    end
    lines[#lines + 1] = declaration
    local insert_row = java_context.insert_after_method(bufnr, method, lines)

    local removed_lines = 0
    if moving then
        removed_lines = mapstruct_nested_target_mappings.regroup_owning_method(bufnr, group, source_path)
    end

    local inserted_import_lines = java_import_resolver.apply(bufnr, imports_or_error)
    local method_line = insert_row + 1 + #lines - removed_lines + inserted_import_lines
    local method_name_start = declaration:find(mapping.method_name, 1, true)
    vim.api.nvim_win_set_cursor(0, { method_line, method_name_start and method_name_start - 1 or 0 })

    if moving then
        vim.notify(
            string.format(
                "[MapStruct] Added nested mapping method: %s (moved %d nested @Mapping)",
                signature,
                #group.mappings
            ),
            vim.log.levels.INFO
        )
        if moved_expressions > 0 then
            vim.notify(
                "[MapStruct] Moved expression mappings still refer to the owning method's parameters; review them",
                vim.log.levels.WARN
            )
        end
    else
        vim.notify("[MapStruct] Added nested mapping method: " .. signature, vim.log.levels.INFO)
    end
    return true
end

--- Collect the owning method's `@Mapping` annotations that address the nested target property.
---@param ctx { bufnr: integer, diagnostic: table }
---@param mapping MapStructNestedTypeMapping
---@return MapStructNestedTargetGroup|nil
local function nested_target_group(ctx, mapping)
    local method = java_context.method_at_diagnostic(ctx.bufnr, ctx.diagnostic)
    if not method then
        return nil
    end
    return mapstruct_nested_target_mappings.collect(ctx.bufnr, method, mapping.target_property)
end

--- Generate the nested mapping method, moving nested target annotations into it.
--- The source type is resolved through the annotations' shared source prefix when
--- there are any, and through the owning method's own `source` of the property when
--- the diagnostic names its last segment.
---@param ctx { bufnr: integer, diagnostic: table }
---@param mapping MapStructNestedTypeMapping
---@param group? MapStructNestedTargetGroup
---@return boolean
local function generate_mapping_method(ctx, mapping, group)
    --- Insert the method once both types are known.
    ---@param resolved_types? { source: JavaResolvedType, target: JavaResolvedType }
    ---@param err? string
    ---@param source_path? string
    local function insert(resolved_types, err, source_path)
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
        insert_mapping_method(ctx.bufnr, ctx.diagnostic, mapping, resolved_types, group, source_path)
    end

    local source_paths = {}
    if group and #group.mappings > 0 then
        source_paths = mapstruct_nested_target_mappings.shared_prefixes(group.source_paths)
        if #source_paths == 0 then
            vim.notify(
                "[MapStruct] Nested mappings of '"
                    .. mapping.target_property
                    .. "' share no source path; declare the mapping method manually",
                vim.log.levels.WARN
            )
            return false
        end
    else
        local direct_source = group and mapstruct_nested_target_mappings.direct_source(group) or nil
        if direct_source and mapstruct_reference.path_name(direct_source) == mapping.source_property then
            source_paths = { direct_source }
        end
    end

    if #source_paths > 0 then
        mapstruct_method_type_resolver.resolve_nested_target(ctx, mapping, source_paths, function(resolved, err)
            insert(resolved, err, resolved and resolved.source_path or nil)
        end)
    else
        mapstruct_method_type_resolver.resolve(ctx, mapping, function(resolved_types, err)
            insert(resolved_types, err, nil)
        end)
    end
    return true
end

--- Build the choices offered for a forged nested property mapping.
---@param mapping MapStructNestedTypeMapping|MapStructNestedMappingMethod
---@param group? MapStructNestedTargetGroup
---@return MapStructNestedMappingChoice[]
function M.choices(mapping, group)
    local generate = "Generate mapping method: " .. mapping.signature
    if group and #group.mappings > 0 then
        generate = string.format("%s (moves %d nested @Mapping)", generate, #group.mappings)
    end

    local properties = mapping.unmapped_properties or { mapping.unmapped_property }
    local nested = {}
    for _, property in ipairs(properties) do
        nested[#nested + 1] = mapping.target_property .. "." .. property
    end

    return {
        { kind = "generate", label = generate },
        { kind = "properties", label = "Map or ignore nested target properties: " .. table.concat(nested, ", ") },
    }
end

--- Resolve a parsed MapStruct forged nested mapping.
--- A plain property offers the mapping method or the owning method's nested
--- annotations; element mappings cannot be annotated, so they go straight to the
--- mapping method.
---@param ctx { bufnr: integer, diagnostic: table }
---@param mapping MapStructNestedTypeMapping
---@return boolean
function M.resolve_mapping(ctx, mapping)
    if mapping.element_kind then
        return generate_mapping_method(ctx, mapping, nil)
    end

    local group = nested_target_group(ctx, mapping)
    vim.ui.select(M.choices(mapping, group), {
        prompt = "MapStruct nested mapping: " .. mapping.target_property,
        format_item = function(choice)
            return choice.label
        end,
    }, function(choice)
        if not choice then
            return
        end
        -- The picker hands focus back asynchronously; resolvers edit the current window.
        vim.schedule(function()
            if choice.kind == "generate" then
                generate_mapping_method(ctx, mapping, group)
                return
            end
            -- Required lazily: the unmapped-target resolver pulls in the async picker stack.
            require("modules.java.diagnostics-resolver.mapstruct-unmapped-target").resolve(ctx, {
                target_prefix = mapping.target_property .. ".",
            })
        end)
    end)
    return true
end

--- Resolve a singular MapStruct forged nested-mapping diagnostic.
---@param ctx { bufnr: integer, diagnostic: table }
---@return boolean
function M.resolve(ctx)
    local mapping = M.parse_mapping(ctx.diagnostic.message or "")
    if not mapping then
        vim.notify("[MapStruct] Could not parse nested mapping types", vim.log.levels.WARN)
        return false
    end

    return M.resolve_mapping(ctx, mapping)
end

return M
