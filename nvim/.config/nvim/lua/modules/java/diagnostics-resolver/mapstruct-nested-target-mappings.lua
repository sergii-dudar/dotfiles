--- Nested target `@Mapping` annotations of a mapper method.
---
--- MapStruct forges one mapping method per nested target property
--- (`@Mapping(target = "remittanceInformation.unstructured", source = "document.header.text")`)
--- and applies the nested annotations inside it. Declaring that method by hand only
--- takes effect when the nested annotations move into it and the owning method maps
--- the nested target property from the source prefix the annotations share.

local java_context = require("modules.java.diagnostics-resolver.java-context")

local M = {}

local MAPPING_ANNOTATION_NAMES = {
    Mapping = true,
    ["org.mapstruct.Mapping"] = true,
}

---@class MapStructNestedTargetMapping
---@field annotation JavaAnnotation
---@field target string full target path, e.g. `remittanceInformation.unstructured`
---@field rest string target path inside the nested type, e.g. `unstructured`
---@field source? string source path when the annotation maps a source property
---@field has_expression boolean

---@class MapStructNestedTargetGroup
---@field property string nested target property, e.g. `remittanceInformation`
---@field direct? JavaAnnotation `@Mapping(target = "<property>", ...)` of the owning method
---@field mappings MapStructNestedTargetMapping[] nested annotations in buffer order
---@field source_paths string[] distinct source paths of the nested annotations

--- Return the named argument of an annotation.
---@param annotation JavaAnnotation
---@param key string
---@return JavaAnnotationArgument|nil
local function argument(annotation, key)
    for _, item in ipairs(annotation.arguments or {}) do
        if item.key == key then
            return item
        end
    end
    return nil
end

--- Return the named argument of an annotation when it is a string literal.
---@param annotation JavaAnnotation
---@param key string
---@return JavaAnnotationArgument|nil
local function string_argument(annotation, key)
    local item = argument(annotation, key)
    if item and item.is_string then
        return item
    end
    return nil
end

--- Collect the `@Mapping` annotations of a method that address one nested target property.
---@param bufnr integer
---@param method TSNode
---@param property string target property of the forged mapping
---@return MapStructNestedTargetGroup
function M.collect(bufnr, method, property)
    local group = { property = property, mappings = {}, source_paths = {} }
    local seen_sources = {}
    local prefix = property .. "."

    for _, annotation in ipairs(java_context.method_annotations(bufnr, method)) do
        local target = MAPPING_ANNOTATION_NAMES[annotation.name] and string_argument(annotation, "target") or nil
        if target and target.value == property then
            group.direct = group.direct or annotation
        elseif target and vim.startswith(target.value, prefix) then
            local source = string_argument(annotation, "source")
            group.mappings[#group.mappings + 1] = {
                annotation = annotation,
                target = target.value,
                rest = target.value:sub(#prefix + 1),
                source = source and source.value or nil,
                has_expression = argument(annotation, "expression") ~= nil,
            }
            if source and not seen_sources[source.value] then
                seen_sources[source.value] = true
                group.source_paths[#group.source_paths + 1] = source.value
            end
        end
    end
    return group
end

--- Return the source path of the owning method's own mapping of the property.
---@param group MapStructNestedTargetGroup
---@return string|nil
function M.direct_source(group)
    local source = group.direct and string_argument(group.direct, "source") or nil
    return source and source.value or nil
end

--- Split a MapStruct path into its segments.
---@param path string
---@return string[]
local function segments(path)
    local result = {}
    for segment in path:gmatch("[^%.]+") do
        result[#result + 1] = segment
    end
    return result
end

--- List the dotted prefixes shared by every source path, longest first.
--- `{ "a.b.c", "a.b.d" }` -> `{ "a.b", "a" }`; a path may equal the prefix.
---@param source_paths string[]
---@return string[]
function M.shared_prefixes(source_paths)
    if #source_paths == 0 then
        return {}
    end

    local shared = segments(source_paths[1])
    for index = 2, #source_paths do
        local other = segments(source_paths[index])
        local count = 0
        while count < #shared and count < #other and shared[count + 1] == other[count + 1] do
            count = count + 1
        end
        for position = #shared, count + 1, -1 do
            shared[position] = nil
        end
    end

    local prefixes = {}
    for length = #shared, 1, -1 do
        prefixes[#prefixes + 1] = table.concat(shared, ".", 1, length)
    end
    return prefixes
end

--- Check that every annotation of the group occupies lines of its own.
--- Moving an annotation replaces its lines, so shared lines would lose code.
---@param bufnr integer
---@param group MapStructNestedTargetGroup
---@return boolean movable
---@return string|nil reason
function M.movable(bufnr, group)
    local annotations = {}
    if group.direct then
        annotations[#annotations + 1] = group.direct
    end
    for _, mapping in ipairs(group.mappings) do
        annotations[#annotations + 1] = mapping.annotation
    end

    for _, annotation in ipairs(annotations) do
        local lines = vim.api.nvim_buf_get_lines(bufnr, annotation.start_row, annotation.end_row + 1, false)
        local first = lines[1] or ""
        local last = lines[#lines] or ""
        if first:sub(1, annotation.start_col):match("%S") or last:sub(annotation.end_col + 1):match("%S") then
            return false,
                string.format("the annotation on line %d shares its line with other code", annotation.start_row + 1)
        end
    end
    return true, nil
end

--- Render an annotation with some of its string arguments replaced.
---@param bufnr integer
---@param annotation JavaAnnotation
---@param replacements { argument: JavaAnnotationArgument, value: string }[]
---@return string[] lines the first line without its indentation
local function render_annotation(bufnr, annotation, replacements)
    local lines = vim.api.nvim_buf_get_lines(bufnr, annotation.start_row, annotation.end_row + 1, false)
    lines[#lines] = (lines[#lines] or ""):sub(1, annotation.end_col)

    -- Later arguments are replaced first so earlier columns stay valid.
    table.sort(replacements, function(a, b)
        if a.argument.start_row ~= b.argument.start_row then
            return a.argument.start_row > b.argument.start_row
        end
        return a.argument.start_col > b.argument.start_col
    end)
    for _, replacement in ipairs(replacements) do
        local item = replacement.argument
        local index = item.start_row - annotation.start_row + 1
        local line = lines[index] or ""
        lines[index] = line:sub(1, item.start_col) .. '"' .. replacement.value .. '"' .. line:sub(item.end_col + 1)
    end

    lines[1] = (lines[1] or ""):sub(annotation.start_col + 1)
    return lines
end

--- Express a moved source path relative to the generated method's parameter.
---@param source string
---@param prefix string shared source prefix
---@param parameter_name string
---@return string
local function relative_source(source, prefix, parameter_name)
    if source == prefix then
        return parameter_name
    end
    if vim.startswith(source, prefix .. ".") then
        return source:sub(#prefix + 2)
    end
    return source
end

--- Render the nested annotations for the generated mapping method.
--- Targets lose the nested property prefix, sources lose the shared source prefix.
---@param bufnr integer
---@param group MapStructNestedTargetGroup
---@param source_prefix string
---@param parameter_name string parameter of the generated method
---@return string[][] one line list per annotation, first lines without indentation
function M.moved_annotations(bufnr, group, source_prefix, parameter_name)
    local result = {}
    for _, mapping in ipairs(group.mappings) do
        local replacements = { { argument = argument(mapping.annotation, "target"), value = mapping.rest } }
        local source = string_argument(mapping.annotation, "source")
        if source then
            replacements[#replacements + 1] = {
                argument = source,
                value = relative_source(source.value, source_prefix, parameter_name),
            }
        end
        result[#result + 1] = render_annotation(bufnr, mapping.annotation, replacements)
    end
    return result
end

--- Rewrite the owning method: the nested annotations leave and the nested target
--- property is mapped from the shared source prefix.
---@param bufnr integer
---@param group MapStructNestedTargetGroup
---@param source_prefix string
---@return integer removed number of lines the owning method lost
function M.regroup_owning_method(bufnr, group, source_prefix)
    local first_mapping = group.mappings[1]
    local annotation_name = first_mapping and first_mapping.annotation.name
        or (group.direct and group.direct.name)
        or "Mapping"
    local direct_line = "@"
        .. annotation_name
        .. '(target = "'
        .. group.property
        .. '", source = "'
        .. source_prefix
        .. '")'

    local edits = {}
    if group.direct then
        local source = string_argument(group.direct, "source")
        if source and source.value == source_prefix then
            -- The owning method already maps the property from the shared source.
        elseif source then
            edits[#edits + 1] = {
                annotation = group.direct,
                lines = render_annotation(bufnr, group.direct, { { argument = source, value = source_prefix } }),
            }
        else
            edits[#edits + 1] = { annotation = group.direct, lines = { direct_line } }
        end
    end
    for index, mapping in ipairs(group.mappings) do
        local lines = {}
        if not group.direct and index == 1 then
            lines = { direct_line }
        end
        edits[#edits + 1] = { annotation = mapping.annotation, lines = lines }
    end
    table.sort(edits, function(a, b)
        return a.annotation.start_row > b.annotation.start_row
    end)

    local removed = 0
    for _, edit in ipairs(edits) do
        local annotation = edit.annotation
        local lines = vim.list_extend({}, edit.lines)
        if lines[1] then
            lines[1] = java_context.line_indent(bufnr, annotation.start_row) .. lines[1]
        end
        vim.api.nvim_buf_set_lines(bufnr, annotation.start_row, annotation.end_row + 1, false, lines)
        removed = removed + (annotation.end_row - annotation.start_row + 1) - #lines
    end
    return removed
end

return M
