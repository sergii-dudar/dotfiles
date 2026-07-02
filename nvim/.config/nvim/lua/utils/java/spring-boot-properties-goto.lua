-- Jump from a Spring Boot Java property reference to the
-- matching key in application/bootstrap YAML or properties files.

local M = {}

local java_common = require("utils.java.java-common")
local uv = vim.uv or vim.loop

local CONFIGURATION_PROPERTIES_FQN = "org.springframework.boot.context.properties.ConfigurationProperties"
local NAME_FQN = "org.springframework.boot.context.properties.bind.Name"
local VALUE_FQN = "org.springframework.beans.factory.annotation.Value"

local RESOURCE_GLOBS = {
    "application*.yaml",
    "application*.yml",
    "application*.properties",
    "bootstrap*.yaml",
    "bootstrap*.yml",
    "bootstrap*.properties",
}

--- Parse the current buffer as Java and return its Tree-sitter root node.
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

--- Return the Tree-sitter node at the current cursor position.
---@param bufnr integer
---@return TSNode|nil
local function node_at_cursor(bufnr)
    local pos = vim.api.nvim_win_get_cursor(0)
    local row = pos[1] - 1
    local col = pos[2]
    local node = vim.treesitter.get_node({ bufnr = bufnr, pos = { row, col } })
    if node then
        return node
    end

    local root = java_root(bufnr)
    if not root then
        return nil
    end
    return root:named_descendant_for_range(row, col, row, col)
end

--- Return the text for a Tree-sitter node in the given buffer.
---@param node TSNode|nil
---@param bufnr integer
---@return string|nil
local function node_text(node, bufnr)
    if not node then
        return nil
    end
    return vim.treesitter.get_node_text(node, bufnr)
end

--- Strip quotes from a Java string literal node.
---@param node TSNode|nil
---@param bufnr integer
---@return string|nil
local function string_literal_value(node, bufnr)
    local text = node_text(node, bufnr)
    if not text then
        return nil
    end
    return text:gsub('^"', ""):gsub('"$', "")
end

--- Return whether a node type is a Java type declaration that can carry annotations.
---@param node TSNode|nil
---@return boolean
local function is_type_declaration(node)
    if not node then
        return false
    end
    local t = node:type()
    return t == "class_declaration" or t == "record_declaration" or t == "enum_declaration"
end

--- Find the nearest enclosing Java class or record declaration.
---@param node TSNode|nil
---@return TSNode|nil
local function enclosing_type(node)
    while node do
        if is_type_declaration(node) then
            return node
        end
        node = node:parent()
    end
    return nil
end

--- Return whether `child` is inside `ancestor`.
---@param child TSNode|nil
---@param ancestor TSNode|nil
---@return boolean
local function is_descendant_of(child, ancestor)
    local node = child
    while node do
        if node == ancestor then
            return true
        end
        node = node:parent()
    end
    return false
end

--- Return the field declarator under the cursor, or the only declarator in a field declaration.
---@param field_node TSNode
---@param cursor_node TSNode
---@return TSNode|nil
local function field_declarator_from_field(field_node, cursor_node)
    local node = cursor_node
    while node and node ~= field_node do
        if node:type() == "variable_declarator" then
            return node
        end
        node = node:parent()
    end

    local declarators = {}
    for child in field_node:iter_children() do
        if child:named() and child:type() == "variable_declarator" then
            table.insert(declarators, child)
        end
    end
    if #declarators == 1 then
        return declarators[1]
    end
    return nil
end

--- Find the record component or class field represented by the cursor.
---@param cursor_node TSNode|nil
---@return TSNode|nil
local function enclosing_property_member(cursor_node)
    local node = cursor_node
    while node do
        local t = node:type()
        if t == "formal_parameter" then
            local parent = node:parent()
            if parent and parent:type() == "formal_parameters" then
                local owner = parent:parent()
                if owner and owner:type() == "record_declaration" then
                    return node
                end
            end
            return nil
        end
        if t == "variable_declarator" then
            local parent = node:parent()
            if parent and parent:type() == "field_declaration" then
                return node
            end
            return nil
        end
        if t == "field_declaration" then
            return field_declarator_from_field(node, cursor_node)
        end
        if is_type_declaration(node) then
            return nil
        end
        node = node:parent()
    end
    return nil
end

--- Find the enum constant represented by the cursor.
---@param cursor_node TSNode|nil
---@return TSNode|nil
local function enclosing_enum_constant(cursor_node)
    local node = cursor_node
    while node do
        local t = node:type()
        if t == "enum_constant" then
            return node
        end
        if t == "enum_declaration" or t == "class_declaration" or t == "record_declaration" then
            return nil
        end
        node = node:parent()
    end
    return nil
end

--- Return whether the cursor is on the type header rather than inside its body.
---@param cursor_node TSNode|nil
---@param type_node TSNode
---@return boolean
local function is_type_header_context(cursor_node, type_node)
    local node = cursor_node
    while node and node ~= type_node do
        local t = node:type()
        if t == "class_body" or t == "enum_body" or t == "block" or t == "constructor_body" then
            return false
        end
        node = node:parent()
    end
    return node == type_node
end

--- Collect Java imports from the current source file.
---@param root TSNode
---@param bufnr integer
---@return table<string, boolean>
local function collect_imports(root, bufnr)
    local imports = {}
    for child in root:iter_children() do
        if child:named() and child:type() == "import_declaration" then
            local text = node_text(child, bufnr) or ""
            text = text:gsub("^import%s+", ""):gsub("^static%s+", ""):gsub(";%s*$", "")
            imports[text] = true
        end
    end
    return imports
end

--- Return whether the imports include a class directly or through its package wildcard.
---@param imports table<string, boolean>
---@param fqn string
---@return boolean
local function imports_include(imports, fqn)
    if imports[fqn] then
        return true
    end
    local package_name = fqn:match("^(.*)%.")
    return package_name ~= nil and imports[package_name .. ".*"] == true
end

--- Return an annotation node's type name.
---@param annotation TSNode
---@param bufnr integer
---@return string|nil
local function annotation_name(annotation, bufnr)
    return node_text(annotation:field("name")[1], bufnr)
end

--- Return whether an annotation resolves by syntax/imports to the expected type.
---@param annotation TSNode
---@param bufnr integer
---@param imports table<string, boolean>
---@param simple_name string
---@param fqn string
---@return boolean
local function is_annotation(annotation, bufnr, imports, simple_name, fqn)
    local name = annotation_name(annotation, bufnr)
    if name == fqn then
        return true
    end
    return name == simple_name and imports_include(imports, fqn)
end

--- Find direct annotations in a declaration's modifiers node.
---@param node TSNode
---@return TSNode[]
local function direct_annotations(node)
    local result = {}
    for child in node:iter_children() do
        if child:named() and child:type() == "modifiers" then
            for modifier in child:iter_children() do
                if modifier:named() and (modifier:type() == "annotation" or modifier:type() == "marker_annotation") then
                    table.insert(result, modifier)
                end
            end
        end
    end
    return result
end

--- Find the @ConfigurationProperties annotation on a type declaration.
---@param type_node TSNode
---@param bufnr integer
---@param imports table<string, boolean>
---@return TSNode|nil
local function find_configuration_properties_annotation(type_node, bufnr, imports)
    for _, annotation in ipairs(direct_annotations(type_node)) do
        if is_annotation(annotation, bufnr, imports, "ConfigurationProperties", CONFIGURATION_PROPERTIES_FQN) then
            return annotation
        end
    end
    return nil
end

--- Find a named element value pair inside an annotation argument list.
---@param annotation TSNode
---@param bufnr integer
---@param pair_name string
---@return string|nil
local function annotation_pair_string(annotation, bufnr, pair_name)
    local args = annotation:field("arguments")[1]
    if not args then
        return nil
    end
    for child in args:iter_children() do
        if child:named() and child:type() == "element_value_pair" then
            local text = node_text(child, bufnr) or ""
            if text:match("^%s*" .. pair_name .. "%s*=") then
                return string_literal_value(child:field("value")[1], bufnr)
            end
        end
    end
    return nil
end

--- Find the first unnamed string literal inside an annotation.
---@param annotation TSNode
---@param bufnr integer
---@return string|nil
local function annotation_unnamed_string(annotation, bufnr)
    local args = annotation:field("arguments")[1]
    if not args then
        return nil
    end
    for child in args:iter_children() do
        if child:named() and child:type() == "string_literal" then
            return string_literal_value(child, bufnr)
        end
    end
    return nil
end

--- Extract a Spring Boot configuration-properties prefix from its annotation.
---@param annotation TSNode
---@param bufnr integer
---@return string
local function configuration_prefix(annotation, bufnr)
    return annotation_pair_string(annotation, bufnr, "prefix")
        or annotation_pair_string(annotation, bufnr, "value")
        or annotation_unnamed_string(annotation, bufnr)
        or ""
end

--- Find the annotation node represented by the cursor.
---@param cursor_node TSNode|nil
---@return TSNode|nil
local function enclosing_annotation(cursor_node)
    local node = cursor_node
    while node do
        local t = node:type()
        if t == "annotation" or t == "marker_annotation" then
            return node
        end
        if is_type_declaration(node) then
            return nil
        end
        node = node:parent()
    end
    return nil
end

--- Find the string literal represented by the cursor.
---@param cursor_node TSNode|nil
---@return TSNode|nil
local function enclosing_string_literal(cursor_node)
    local node = cursor_node
    while node do
        if node:type() == "string_literal" then
            return node
        end
        if is_type_declaration(node) then
            return nil
        end
        node = node:parent()
    end
    return nil
end

--- Find a direct @Value annotation on a declaration node.
---@param node TSNode
---@param bufnr integer
---@param imports table<string, boolean>
---@return TSNode|nil
local function direct_value_annotation(node, bufnr, imports)
    for _, annotation in ipairs(direct_annotations(node)) do
        if is_annotation(annotation, bufnr, imports, "Value", VALUE_FQN) then
            return annotation
        end
    end
    return nil
end

--- Find the @Value annotation related to the current cursor.
---@param cursor_node TSNode|nil
---@param bufnr integer
---@param imports table<string, boolean>
---@return TSNode|nil
local function value_annotation_for_cursor(cursor_node, bufnr, imports)
    local annotation = enclosing_annotation(cursor_node)
    if annotation and is_annotation(annotation, bufnr, imports, "Value", VALUE_FQN) then
        return annotation
    end

    local node = cursor_node
    while node do
        local t = node:type()
        if t == "field_declaration" or t == "formal_parameter" or t == "method_declaration" then
            local value_annotation = direct_value_annotation(node, bufnr, imports)
            if value_annotation then
                return value_annotation
            end
        end
        if is_type_declaration(node) then
            return nil
        end
        node = node:parent()
    end

    return nil
end

--- Extract the string expression from a Spring @Value annotation.
---@param annotation TSNode
---@param bufnr integer
---@return string|nil
local function value_annotation_string(annotation, bufnr)
    return annotation_unnamed_string(annotation, bufnr) or annotation_pair_string(annotation, bufnr, "value")
end

--- Find the matching closing brace for a Spring ${...} placeholder.
---@param text string
---@param start_index integer
---@return integer|nil
local function placeholder_end_index(text, start_index)
    local depth = 1
    local index = start_index + 2
    while index <= #text do
        if text:sub(index, index + 1) == "${" then
            depth = depth + 1
            index = index + 2
        elseif text:sub(index, index) == "}" then
            depth = depth - 1
            if depth == 0 then
                return index
            end
            index = index + 1
        else
            index = index + 1
        end
    end
    return nil
end

--- Find the first top-level default-value colon in placeholder content.
---@param content string
---@return integer|nil
local function placeholder_default_colon(content)
    local depth = 0
    local index = 1
    while index <= #content do
        if content:sub(index, index + 1) == "${" then
            depth = depth + 1
            index = index + 2
        else
            local char = content:sub(index, index)
            if char == "}" and depth > 0 then
                depth = depth - 1
            elseif char == ":" and depth == 0 then
                return index
            end
            index = index + 1
        end
    end
    return nil
end

--- Add one Spring placeholder key candidate to a list.
---@param keys string[]
---@param seen table<string, boolean>
---@param content string
local function add_placeholder_key(keys, seen, content)
    local colon = placeholder_default_colon(content)
    local key = vim.trim(colon and content:sub(1, colon - 1) or content)
    if key ~= "" and not key:find("%$") and not key:match("^#") and not seen[key] then
        table.insert(keys, key)
        seen[key] = true
    end
end

--- Extract property keys from every Spring ${...} placeholder in a string.
---@param value string
---@return string[]
local function spring_placeholder_keys(value)
    local keys = {}
    local seen = {}

    local function scan(text)
        local index = 1
        while index <= #text do
            local start_index = text:find("${", index, true)
            if not start_index then
                return
            end

            local end_index = placeholder_end_index(text, start_index)
            if not end_index then
                return
            end

            local content = text:sub(start_index + 2, end_index - 1)
            add_placeholder_key(keys, seen, content)
            scan(content)
            index = end_index + 1
        end
    end

    scan(value)
    return keys
end

--- Add placeholder keys from a string literal node.
---@param keys string[]
---@param seen table<string, boolean>
---@param literal TSNode
---@param bufnr integer
local function add_string_literal_placeholder_keys(keys, seen, literal, bufnr)
    local value = string_literal_value(literal, bufnr)
    if not value then
        return
    end

    for _, key in ipairs(spring_placeholder_keys(value)) do
        if not seen[key] then
            table.insert(keys, key)
            seen[key] = true
        end
    end
end

--- Extract Spring placeholder keys from all string literals below a node.
---@param node TSNode
---@param bufnr integer
---@return string[]
local function placeholder_keys_in_node(node, bufnr)
    local keys = {}
    local seen = {}

    local function visit(current)
        if current:type() == "string_literal" then
            add_string_literal_placeholder_keys(keys, seen, current, bufnr)
            return
        end

        for child in current:iter_children() do
            if child:named() then
                visit(child)
            end
        end
    end

    visit(node)
    return keys
end

--- Resolve Spring placeholder keys around the cursor, independent of annotation type.
---@param cursor_node TSNode|nil
---@param bufnr integer
---@return string[]
local function literal_placeholder_keys(cursor_node, bufnr)
    local literal = enclosing_string_literal(cursor_node)
    if literal then
        return placeholder_keys_in_node(literal, bufnr)
    end

    local annotation = enclosing_annotation(cursor_node)
    if annotation then
        return placeholder_keys_in_node(annotation, bufnr)
    end

    return {}
end

--- Resolve @Value placeholder property keys around the cursor.
---@param cursor_node TSNode|nil
---@param bufnr integer
---@param imports table<string, boolean>
---@return string[]
local function value_property_keys(cursor_node, bufnr, imports)
    local annotation = value_annotation_for_cursor(cursor_node, bufnr, imports)
    if not annotation then
        return {}
    end

    local value = value_annotation_string(annotation, bufnr)
    if not value then
        return {}
    end

    return spring_placeholder_keys(value)
end

--- Extract a Spring Boot @Name override from a property member.
---@param member_node TSNode
---@param bufnr integer
---@param imports table<string, boolean>
---@return string|nil
local function name_override(member_node, bufnr, imports)
    local annotation_owner = member_node
    if member_node:type() == "variable_declarator" then
        local parent = member_node:parent()
        if parent and parent:type() == "field_declaration" then
            annotation_owner = parent
        end
    end

    for _, annotation in ipairs(direct_annotations(annotation_owner)) do
        if is_annotation(annotation, bufnr, imports, "Name", NAME_FQN) then
            return annotation_unnamed_string(annotation, bufnr) or annotation_pair_string(annotation, bufnr, "value")
        end
    end
    return nil
end

--- Convert a Java member name to the usual Spring Boot kebab-case property segment.
---@param name string
---@return string
local function camel_to_kebab(name)
    return name:gsub("_", "-"):gsub("([A-Z]+)([A-Z][a-z])", "%1-%2"):gsub("([a-z0-9])([A-Z])", "%1-%2"):lower()
end

--- Extract the Java name from a record component or field declarator node.
---@param member_node TSNode
---@param bufnr integer
---@return string|nil
local function member_name(member_node, bufnr)
    return node_text(member_node:field("name")[1], bufnr)
end

--- Extract the Java type name from a class or record declaration.
---@param type_node TSNode
---@param bufnr integer
---@return string|nil
local function type_declaration_name(type_node, bufnr)
    return node_text(type_node:field("name")[1], bufnr)
end

--- Build the Spring property segment for a member, respecting @Name when present.
---@param member_node TSNode
---@param bufnr integer
---@param imports table<string, boolean>
---@return string|nil
local function member_property_segment(member_node, bufnr, imports)
    local override = name_override(member_node, bufnr, imports)
    if override and override ~= "" then
        return override
    end

    local name = member_name(member_node, bufnr)
    if not name or name == "" then
        return nil
    end
    return camel_to_kebab(name)
end

--- Collect string literal constants declared inside a Java type.
---@param type_node TSNode
---@param bufnr integer
---@return table<string, string>
local function string_constants_in_type(type_node, bufnr)
    local constants = {}

    local function visit(node)
        if node:type() == "variable_declarator" then
            local name = node_text(node:field("name")[1], bufnr)
            local value = string_literal_value(node:field("value")[1], bufnr)
            if name and value then
                constants[name] = value
            end
        end

        for child in node:iter_children() do
            if child:named() then
                visit(child)
            end
        end
    end

    visit(type_node)
    return constants
end

--- Resolve a node that may refer to a string constant.
---@param node TSNode
---@param bufnr integer
---@param constants table<string, string>
---@return string|nil
local function resolve_string_constant_reference(node, bufnr, constants)
    if node:type() == "string_literal" then
        return string_literal_value(node, bufnr)
    end

    if node:type() == "identifier" then
        return constants[node_text(node, bufnr) or ""]
    end

    if node:type() == "field_access" then
        return constants[node_text(node:field("field")[1], bufnr) or ""]
    end

    return nil
end

--- Extract possible Spring map keys from an enum constant.
---@param enum_constant TSNode
---@param enum_node TSNode
---@param bufnr integer
---@return string[]
local function enum_constant_property_keys(enum_constant, enum_node, bufnr)
    local keys = {}
    local seen = {}

    local function add(value)
        if value and value ~= "" and not seen[value] then
            table.insert(keys, value)
            seen[value] = true
        end
    end

    add(node_text(enum_constant:field("name")[1], bufnr))

    local constants = string_constants_in_type(enum_node, bufnr)
    local args = enum_constant:field("arguments")[1]
    if args then
        for child in args:iter_children() do
            if child:named() then
                add(resolve_string_constant_reference(child, bufnr, constants))
            end
        end
    end

    return keys
end

--- Join non-empty property path parts with dots.
---@param parts string[]
---@return string
local function join_property_path(parts)
    local result = {}
    for _, part in ipairs(parts) do
        if part and part ~= "" then
            table.insert(result, part)
        end
    end
    return table.concat(result, ".")
end

--- Find the nearest enclosing @ConfigurationProperties type declaration.
---@param type_node TSNode
---@param bufnr integer
---@param imports table<string, boolean>
---@return TSNode|nil type_node, TSNode|nil annotation
local function enclosing_configuration_type(type_node, bufnr, imports)
    local node = type_node
    while node do
        local annotation = find_configuration_properties_annotation(node, bufnr, imports)
        if annotation then
            return node, annotation
        end
        node = enclosing_type(node:parent())
    end
    return nil, nil
end

--- Return direct field/record-component property members declared by a type.
---@param type_node TSNode
---@return TSNode[]
local function direct_property_members(type_node)
    local members = {}
    for child in type_node:iter_children() do
        if child:named() and child:type() == "formal_parameters" then
            for parameter in child:iter_children() do
                if parameter:named() and parameter:type() == "formal_parameter" then
                    table.insert(members, parameter)
                end
            end
        elseif child:named() and child:type() == "class_body" then
            for body_child in child:iter_children() do
                if body_child:named() and body_child:type() == "field_declaration" then
                    local declarator = field_declarator_from_field(body_child, body_child)
                    if declarator then
                        table.insert(members, declarator)
                    end
                end
            end
        end
    end
    return members
end

--- Extract the declared Java type text for a field or record component.
---@param member_node TSNode
---@param bufnr integer
---@return string|nil
local function member_type_text(member_node, bufnr)
    if member_node:type() == "formal_parameter" then
        return node_text(member_node:field("type")[1], bufnr)
    end

    if member_node:type() == "variable_declarator" then
        local parent = member_node:parent()
        if parent and parent:type() == "field_declaration" then
            return node_text(parent:field("type")[1], bufnr)
        end
    end

    return nil
end

--- Return whether a Java type text references a simple type name.
---@param type_text string|nil
---@param simple_name string
---@return boolean
local function type_text_references_name(type_text, simple_name)
    if not type_text then
        return false
    end
    for identifier in type_text:gmatch("[%a_][%w_]*") do
        if identifier == simple_name then
            return true
        end
    end
    return false
end

--- Return the first top-level generic argument list from a Java type text.
---@param type_text string
---@return string|nil
local function type_arguments_text(type_text)
    local start_index = type_text:find("<", 1, true)
    if not start_index then
        return nil
    end

    local depth = 0
    for index = start_index, #type_text do
        local char = type_text:sub(index, index)
        if char == "<" then
            depth = depth + 1
        elseif char == ">" then
            depth = depth - 1
            if depth == 0 then
                return type_text:sub(start_index + 1, index - 1)
            end
        end
    end

    return nil
end

--- Split comma-separated Java generic arguments at top level.
---@param args_text string
---@return string[]
local function split_type_arguments(args_text)
    local args = {}
    local depth = 0
    local start_index = 1

    for index = 1, #args_text do
        local char = args_text:sub(index, index)
        if char == "<" then
            depth = depth + 1
        elseif char == ">" then
            depth = depth - 1
        elseif char == "," and depth == 0 then
            table.insert(args, vim.trim(args_text:sub(start_index, index - 1)))
            start_index = index + 1
        end
    end

    table.insert(args, vim.trim(args_text:sub(start_index)))
    return args
end

--- Remove type-use annotations from Java type text.
---@param type_text string
---@return string
local function strip_type_annotations(type_text)
    return type_text:gsub("@[%w_%.]+%s*", "")
end

--- Return key/value argument text when a type is a Map.
---@param type_text string|nil
---@return string|nil key_type, string|nil value_type
local function map_type_arguments(type_text)
    if not type_text then
        return nil, nil
    end

    local normalized = strip_type_annotations(type_text)
    if not normalized:match("[%w_%.]*Map%s*<") then
        return nil, nil
    end

    local args_text = type_arguments_text(normalized)
    if not args_text then
        return nil, nil
    end

    local args = split_type_arguments(args_text)
    return args[1], args[2]
end

--- Collect class/record/enum declarations contained inside a configuration type.
---@param root_type TSNode
---@param bufnr integer
---@return { node: TSNode, name: string }[], table<string, { node: TSNode, name: string }[]>
local function collect_type_declarations(root_type, bufnr)
    local types = {}
    local by_name = {}

    local function visit(node)
        if is_type_declaration(node) then
            local name = type_declaration_name(node, bufnr)
            if name then
                local item = { node = node, name = name }
                table.insert(types, item)
                by_name[name] = by_name[name] or {}
                table.insert(by_name[name], item)
            end
        end

        for child in node:iter_children() do
            if child:named() then
                visit(child)
            end
        end
    end

    visit(root_type)
    return types, by_name
end

--- Extract all Spring property key candidates from an enum declaration.
---@param enum_node TSNode
---@param bufnr integer
---@return string[]
local function enum_property_key_values(enum_node, bufnr)
    local keys = {}
    local seen = {}

    local function add(value)
        if value and value ~= "" and not seen[value] then
            table.insert(keys, value)
            seen[value] = true
        end
    end

    for child in enum_node:iter_children() do
        if child:named() and child:type() == "enum_body" then
            for body_child in child:iter_children() do
                if body_child:named() and body_child:type() == "enum_constant" then
                    for _, key in ipairs(enum_constant_property_keys(body_child, enum_node, bufnr)) do
                        add(key)
                    end
                end
            end
        end
    end

    return keys
end

--- Resolve map-value type declarations to their enum key candidates.
---@param type_text string
---@param by_name table<string, { node: TSNode, name: string }[]>
---@param bufnr integer
---@return table<TSNode, string[]>
local function map_value_key_values(type_text, by_name, bufnr)
    local key_type, value_type = map_type_arguments(type_text)
    if not key_type or not value_type then
        return {}
    end

    local key_values = {}
    for key_name, key_targets in pairs(by_name) do
        if type_text_references_name(key_type, key_name) then
            for _, key_target in ipairs(key_targets) do
                if key_target.node:type() == "enum_declaration" then
                    vim.list_extend(key_values, enum_property_key_values(key_target.node, bufnr))
                end
            end
        end
    end

    if #key_values == 0 then
        return {}
    end

    local result = {}
    for value_name, value_targets in pairs(by_name) do
        if type_text_references_name(value_type, value_name) then
            for _, value_target in ipairs(value_targets) do
                result[value_target.node] = key_values
            end
        end
    end

    return result
end

--- Build property-member edges between nested configuration value types.
---@param config_type TSNode
---@param bufnr integer
---@param imports table<string, boolean>
---@return table<TSNode, { to: TSNode, segment: string, map_key_values: string[]|nil }[]>
local function property_type_edges(config_type, bufnr, imports)
    local types, by_name = collect_type_declarations(config_type, bufnr)
    local edges = {}

    for _, source in ipairs(types) do
        edges[source.node] = {}
        for _, member in ipairs(direct_property_members(source.node)) do
            local segment = member_property_segment(member, bufnr, imports)
            local type_text = member_type_text(member, bufnr)
            if segment and type_text then
                local value_key_values = map_value_key_values(type_text, by_name, bufnr)
                for referenced_name, targets in pairs(by_name) do
                    if type_text_references_name(type_text, referenced_name) then
                        for _, target in ipairs(targets) do
                            if target.node ~= source.node then
                                table.insert(edges[source.node], {
                                    to = target.node,
                                    segment = segment,
                                    map_key_values = value_key_values[target.node],
                                })
                            end
                        end
                    end
                end
            end
        end
    end

    return edges
end

--- Return a shallow copy of a list.
---@param list any[]
---@return any[]
local function list_copy(list)
    local copy = {}
    for _, item in ipairs(list) do
        table.insert(copy, item)
    end
    return copy
end

--- Return a shallow copy of a map keyed by Tree-sitter nodes.
---@param map table<TSNode, boolean>
---@return table<TSNode, boolean>
local function node_set_copy(map)
    local copy = {}
    for key, value in pairs(map) do
        copy[key] = value
    end
    return copy
end

--- Expand one type edge into one or more property segment lists.
---@param edge { segment: string, map_key_values: string[]|nil }
---@return string[][]
local function edge_segment_paths(edge)
    if not edge.map_key_values or #edge.map_key_values == 0 then
        return { { edge.segment } }
    end

    local paths = {}
    for _, key in ipairs(edge.map_key_values) do
        table.insert(paths, { edge.segment, key })
    end
    return paths
end

--- Resolve property segment chains from the annotated type to another nested type.
---@param config_type TSNode
---@param target_type TSNode
---@param bufnr integer
---@param imports table<string, boolean>
---@return string[][]
local function property_paths_to_type(config_type, target_type, bufnr, imports)
    if config_type == target_type then
        return { {} }
    end

    local edges = property_type_edges(config_type, bufnr, imports)
    local paths = {}

    local function visit(type_node, segments, seen)
        if type_node == target_type then
            table.insert(paths, list_copy(segments))
            return
        end
        if seen[type_node] then
            return
        end

        seen[type_node] = true
        for _, edge in ipairs(edges[type_node] or {}) do
            for _, edge_segments in ipairs(edge_segment_paths(edge)) do
                local next_segments = list_copy(segments)
                vim.list_extend(next_segments, edge_segments)
                visit(edge.to, next_segments, node_set_copy(seen))
            end
        end
    end

    visit(config_type, {}, {})
    return paths
end

--- Prefix resolved property segment chains with the annotation prefix.
---@param prefix string
---@param paths string[][]
---@param suffix string|nil
---@return string[]
local function property_keys_from_paths(prefix, paths, suffix)
    local keys = {}
    for _, path in ipairs(paths) do
        local parts = list_copy(path)
        if suffix and suffix ~= "" then
            table.insert(parts, suffix)
        end
        table.insert(parts, 1, prefix)
        table.insert(keys, join_property_path(parts))
    end
    return keys
end

--- Build a navigation target from one or more candidate property keys.
---@param keys string[]
---@param kind "type"|"member"|"enum"|"value"|"placeholder"
---@return { key: string, keys: string[], kind: "type"|"member"|"enum"|"value"|"placeholder" }|nil
local function property_target(keys, kind)
    local unique = {}
    local seen = {}
    for _, key in ipairs(keys) do
        if key and key ~= "" and not seen[key] then
            table.insert(unique, key)
            seen[key] = true
        end
    end
    if #unique == 0 then
        return nil
    end
    return {
        key = unique[1],
        keys = unique,
        kind = kind,
    }
end

--- Resolve the Spring property key implied by the current Java cursor position.
---@param bufnr? integer
---@return { key: string, keys: string[], kind: "type"|"member"|"enum"|"value"|"placeholder" }|nil
function M.resolve_target(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()
    if vim.bo[bufnr].filetype ~= "java" then
        return nil
    end

    local root = java_root(bufnr)
    local cursor_node = node_at_cursor(bufnr)
    if not root or not cursor_node then
        return nil
    end

    local enum_constant = enclosing_enum_constant(cursor_node)
    local member_node = enclosing_property_member(cursor_node)
    local type_node = enclosing_type(enum_constant or member_node or cursor_node)
    if not type_node then
        return nil
    end

    local imports = collect_imports(root, bufnr)
    local literal_keys = literal_placeholder_keys(cursor_node, bufnr)
    if #literal_keys > 0 then
        return property_target(literal_keys, "placeholder")
    end

    local value_keys = value_property_keys(cursor_node, bufnr, imports)
    if #value_keys > 0 then
        return property_target(value_keys, "value")
    end

    local config_type, annotation = enclosing_configuration_type(type_node, bufnr, imports)
    if not config_type or not annotation then
        return nil
    end

    local prefix = configuration_prefix(annotation, bufnr)
    local paths = property_paths_to_type(config_type, type_node, bufnr, imports)

    if enum_constant and type_node:type() == "enum_declaration" and #paths > 0 then
        local keys = {}
        for _, enum_key in ipairs(enum_constant_property_keys(enum_constant, type_node, bufnr)) do
            vim.list_extend(keys, property_keys_from_paths(prefix, paths, enum_key))
        end
        return property_target(keys, "enum")
    end

    if member_node and is_descendant_of(member_node, type_node) and #paths > 0 then
        local segment = member_property_segment(member_node, bufnr, imports)
        if segment then
            return property_target(property_keys_from_paths(prefix, paths, segment), "member")
        end
    end

    if not is_type_header_context(cursor_node, type_node) then
        return nil
    end

    if prefix == "" or #paths == 0 then
        return nil
    end
    return property_target(property_keys_from_paths(prefix, paths, nil), "type")
end

--- Return whether a path exists and is a directory.
---@param path string
---@return boolean
local function is_dir(path)
    local stat = uv.fs_stat(path)
    return stat ~= nil and stat.type == "directory"
end

--- Insert a value into a list only once.
---@param list string[]
---@param seen table<string, boolean>
---@param value string|nil
local function add_unique(list, seen, value)
    if value and value ~= "" and not seen[value] then
        table.insert(list, value)
        seen[value] = true
    end
end

--- Resolve resource directories for the current Java module.
---@param bufnr integer
---@return string[]
local function resource_dirs(bufnr)
    local dirs = {}
    local seen = {}

    local resolved = require("utils.resource-cwd-resolver").resolve(bufnr)
    if resolved and resolved.dirs then
        for _, dir in ipairs(resolved.dirs) do
            add_unique(dirs, seen, dir)
        end
    end

    local module_root = java_common.get_buffer_project_path(bufnr)
    if module_root then
        local main_resources = module_root .. "/src/main/resources"
        local test_resources = module_root .. "/src/test/resources"
        if is_dir(main_resources) then
            add_unique(dirs, seen, main_resources)
        end
        if vim.api.nvim_buf_get_name(bufnr):match("/src/test/") and is_dir(test_resources) then
            add_unique(dirs, seen, test_resources)
        end
    end

    return dirs
end

--- Resolve application/bootstrap resource files for the current Java module.
---@param bufnr integer
---@return string[]
local function resource_files(bufnr)
    local files = {}
    local seen = {}
    for _, dir in ipairs(resource_dirs(bufnr)) do
        for _, glob in ipairs(RESOURCE_GLOBS) do
            for _, file in ipairs(vim.fn.globpath(dir, glob, false, true)) do
                add_unique(files, seen, vim.fn.fnamemodify(file, ":p"))
            end
        end
    end
    table.sort(files)
    return files
end

--- Normalize a property path for relaxed-name comparison.
---@param path string
---@return string
local function canonical_property_path(path)
    local segments = {}
    for segment in path:gmatch("[^.]+") do
        local normalized = segment:lower():gsub("[-_]", "")
        table.insert(segments, normalized)
    end
    return table.concat(segments, ".")
end

--- Return whether `path` is an exact or child match for `target`.
---@param path string
---@param target string
---@param allow_child boolean
---@return boolean, boolean
local function path_matches(path, target, allow_child)
    local canonical_path = canonical_property_path(path)
    local canonical_target = canonical_property_path(target)
    if canonical_path == canonical_target then
        return true, false
    end
    if allow_child and vim.startswith(canonical_path, canonical_target .. ".") then
        return true, true
    end
    if allow_child and vim.startswith(canonical_path, canonical_target .. "[") then
        return true, true
    end
    return false, false
end

--- Parse a YAML mapping key from one line.
---@param line string
---@return integer|nil indent, string|nil key, integer|nil col
local function parse_yaml_key(line)
    if line:match("^%s*$") or line:match("^%s*#") then
        return nil, nil, nil
    end

    local list_indent_text, quoted_list_key = line:match("^(%s*)%-%s+[\"']([^\"']+)[\"']%s*:")
    local list_key = quoted_list_key
    if not list_key then
        list_indent_text, list_key = line:match("^(%s*)%-%s+([^:#]+)%s*:")
    end
    if list_key then
        list_key = vim.trim(list_key)
        if list_key == "" then
            return nil, nil, nil
        end

        local col = line:find(list_key, 1, true)
        return col and (col - 1) or (#list_indent_text + 2), list_key, col and (col - 1) or (#list_indent_text + 2)
    end

    local indent_text, quoted_key = line:match("^(%s*)[\"']([^\"']+)[\"']%s*:")
    local key = quoted_key
    if not key then
        indent_text, key = line:match("^(%s*)([^:#]+)%s*:")
    end
    if not key then
        return nil, nil, nil
    end

    key = vim.trim(key)
    if key == "" or key:match("^%-") then
        return nil, nil, nil
    end

    local col = line:find(key, 1, true)
    return #indent_text, key, col and (col - 1) or #indent_text
end

--- Search one YAML file for a matching property path.
---@param file string
---@param target string
---@param allow_child boolean
---@return table[]
local function find_in_yaml(file, target, allow_child)
    local matches = {}
    local lines = vim.fn.readfile(file)
    local stack = {}

    for lnum, line in ipairs(lines) do
        local indent, key, col = parse_yaml_key(line)
        if indent and key and col then
            while #stack > 0 and stack[#stack].indent >= indent do
                table.remove(stack)
            end
            table.insert(stack, { indent = indent, key = key })

            local path_parts = {}
            for _, item in ipairs(stack) do
                table.insert(path_parts, item.key)
            end
            local path = table.concat(path_parts, ".")
            local ok, child = path_matches(path, target, allow_child)
            if ok then
                table.insert(matches, {
                    file = file,
                    lnum = lnum - 1,
                    col = col,
                    path = path,
                    child = child,
                })
            end
        end
    end

    return matches
end

--- Parse a .properties key from one line.
---@param line string
---@return string|nil key, integer|nil col
local function parse_properties_key(line)
    if line:match("^%s*$") or line:match("^%s*[#!]") then
        return nil, nil
    end

    local sep = line:find("[=:]")
    if not sep then
        return nil, nil
    end

    local key = vim.trim(line:sub(1, sep - 1))
    if key == "" then
        return nil, nil
    end

    local col = line:find(key, 1, true)
    return key, col and (col - 1) or 0
end

--- Search one .properties file for a matching property path.
---@param file string
---@param target string
---@param allow_child boolean
---@return table[]
local function find_in_properties(file, target, allow_child)
    local matches = {}
    for lnum, line in ipairs(vim.fn.readfile(file)) do
        local key, col = parse_properties_key(line)
        if key and col then
            local ok, child = path_matches(key, target, allow_child)
            if ok then
                table.insert(matches, {
                    file = file,
                    lnum = lnum - 1,
                    col = col,
                    path = key,
                    child = child,
                })
            end
        end
    end
    return matches
end

--- Search one Spring application resource file for a matching property path.
---@param file string
---@param target string
---@param allow_child boolean
---@return table[]
local function find_in_resource_file(file, target, allow_child)
    if file:match("%.ya?ml$") then
        return find_in_yaml(file, target, allow_child)
    end
    if file:match("%.properties$") then
        return find_in_properties(file, target, allow_child)
    end
    return {}
end

--- Sort exact property matches before child matches.
---@param matches table[]
local function sort_matches(matches)
    table.sort(matches, function(a, b)
        if a.child ~= b.child then
            return not a.child
        end
        if a.file ~= b.file then
            return a.file < b.file
        end
        return a.lnum < b.lnum
    end)
end

--- Return only exact property matches.
---@param matches table[]
---@return table[]
local function exact_matches(matches)
    local exact = {}
    for _, match in ipairs(matches) do
        if not match.child then
            table.insert(exact, match)
        end
    end
    return exact
end

--- Find application/bootstrap resource locations for a property path.
---@param bufnr integer
---@param target string
---@param allow_child boolean
---@return table[]
local function find_property_matches(bufnr, target, allow_child)
    local matches = {}
    for _, file in ipairs(resource_files(bufnr)) do
        vim.list_extend(matches, find_in_resource_file(file, target, allow_child))
    end
    sort_matches(matches)
    local exact = exact_matches(matches)
    if #exact > 0 then
        return exact
    end
    return matches
end

--- Find property matches for every candidate key in a resolved target.
---@param bufnr integer
---@param target { key: string, keys: string[] }
---@return table[]
local function find_target_matches(bufnr, target)
    local matches = {}
    local seen = {}
    for _, key in ipairs(target.keys or { target.key }) do
        for _, match in ipairs(find_property_matches(bufnr, key, true)) do
            local id = table.concat({ match.file, match.lnum, match.col, match.path }, "\t")
            if not seen[id] then
                table.insert(matches, match)
                seen[id] = true
            end
        end
    end
    sort_matches(matches)
    local exact = exact_matches(matches)
    if #exact > 0 then
        return exact
    end
    return matches
end

--- Add the current Java source location to the window jumplist before opening a property file.
local function add_origin_to_jumplist()
    vim.cmd("normal! m'")
end

--- Open a matched property location in the current window.
---@param match table
local function open_match(match)
    add_origin_to_jumplist()
    local bufnr = vim.fn.bufadd(match.file)
    vim.fn.bufload(bufnr)
    vim.bo[bufnr].buflisted = true
    vim.api.nvim_win_set_buf(0, bufnr)
    vim.api.nvim_win_set_cursor(0, { match.lnum + 1, match.col })
    vim.cmd("normal! zv")
end

--- Render a property match for vim.ui.select.
---@param match table
---@return string
local function format_match(match)
    local rel = vim.fn.fnamemodify(match.file, ":~:.")
    return string.format("%s:%d %s", rel, match.lnum + 1, match.path)
end

--- Open one match, or ask the user to select when multiple matches exist.
---@param matches table[]
local function open_or_select_match(matches)
    if #matches == 1 then
        open_match(matches[1])
        return
    end

    vim.ui.select(matches, {
        prompt = "Spring property target",
        format_item = format_match,
    }, function(item)
        if item then
            open_match(item)
        end
    end)
end

--- Jump from the current Java cursor position to a matching Spring property key.
---@return boolean handled whether this helper handled the cursor context
function M.goto_property_definition()
    local bufnr = vim.api.nvim_get_current_buf()
    local target = M.resolve_target(bufnr)
    if not target then
        return false
    end

    local matches = find_target_matches(bufnr, target)
    if #matches == 0 then
        vim.notify("Spring property not found: " .. target.key, vim.log.levels.WARN)
        return true
    end

    open_or_select_match(matches)
    return true
end

return M
