-- Java class <-> record converter.
--
-- `toggle()` detects the type declaration under the cursor with tree-sitter and
-- converts it in place:
--
--   class  -> record : instance fields become record components, the canonical
--                      constructor becomes a compact constructor, trivial
--                      accessors are dropped (records generate them), and Lombok
--                      annotations made redundant by records are stripped.
--   record -> class  : components become fields on a Lombok `@Value` class, the
--                      compact constructor becomes an explicit canonical
--                      constructor.
--
-- Everything not owned by the conversion (static fields, other constructors,
-- methods, nested types, initializers) is copied verbatim together with its
-- leading comments/javadoc, so nothing is lost.
--
-- Note: `@Value` generates `getX()` accessors, while a record generates `x()`.
-- record -> class therefore changes accessor names at call sites by design.
--
-- Public API:
--   toggle()          convert the type under the cursor to the other kind
--   class_to_record() force class -> record
--   record_to_class() force record -> class

local M = {}

--- Lombok annotations that a record cannot carry (Lombok rejects them on records)
--- and that a record makes redundant anyway. Stripped on class -> record and
--- re-added on record -> class only in the case of `@Value`.
---
--- Everything NOT listed here round-trips untouched in both directions:
--- `@Builder`, `@With`, `@Jacksonized`, `@Slf4j`, `@FieldNameConstants`, plus all
--- Spring / Jackson / validation / custom annotations.
local RECORD_INCOMPATIBLE_LOMBOK = {
    Value = true,
    Data = true,
    Getter = true,
    Setter = true,
    AllArgsConstructor = true,
    RequiredArgsConstructor = true,
    NoArgsConstructor = true,
    EqualsAndHashCode = true,
    ToString = true,
    FieldDefaults = true,
}

--- Class modifiers that a record declares implicitly.
--- A nested record is always `static` and every record is `final`, so these are
--- dropped on class -> record and `static` is restored on record -> class.
local IMPLICIT_RECORD_MODIFIERS = {
    final = true,
    static = true,
}

--- Body kinds whose member types are nested and therefore may be `static`.
--- A type declared inside a method `block` is a local type and cannot be static.
local NESTING_BODIES = {
    class_body = true,
    interface_body = true,
    enum_body = true,
    enum_body_declarations = true,
    annotation_type_body = true,
}

--- Access modifiers `static` must come after, keeping conventional Java order.
local ACCESS_MODIFIERS = {
    public = true,
    protected = true,
    private = true,
}

local VALUE_ANNOTATION = "@Value"
local VALUE_IMPORT = "lombok.Value"

-- ============================================================================
-- Tree-sitter helpers
-- ============================================================================

---@param node TSNode
---@param bufnr integer
---@return string
local function node_text(node, bufnr)
    return vim.treesitter.get_node_text(node, bufnr)
end

---@param node TSNode|nil
---@param field string
---@return TSNode|nil
local function field(node, field_name)
    if not node then
        return nil
    end
    local nodes = node:field(field_name)
    return nodes and nodes[1] or nil
end

---@param node TSNode
---@param type_name string
---@return TSNode|nil
local function child_of_type(node, type_name)
    for child in node:iter_children() do
        if child:type() == type_name then
            return child
        end
    end
    return nil
end

---@param bufnr integer
---@return TSNode|nil root
local function parse_root(bufnr)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    return tree and tree:root() or nil
end

local COMMENT_TYPES = {
    line_comment = true,
    block_comment = true,
    comment = true,
}

--- First row of a member including any comment/javadoc lines directly above it.
--- Tree-sitter keeps comments as siblings, not as children of the member.
---@param member TSNode
---@return integer row 0-indexed
local function start_row_with_comments(member)
    local row = member:start()
    local prev = member:prev_named_sibling()
    while prev and COMMENT_TYPES[prev:type()] do
        local prev_start, _, prev_end = prev:range()
        -- Only attach the comment when it is not on the same line as previous code
        -- and touches the member (no blank line between them).
        if prev_end + 1 < row - 1 then
            break
        end
        row = prev_start
        prev = prev:prev_named_sibling()
    end
    return row
end

--- Raw buffer lines of a member, including its leading comments.
---@param member TSNode
---@param bufnr integer
---@return string[]
local function member_lines(member, bufnr)
    local start_row = start_row_with_comments(member)
    local end_row = member:end_()
    return vim.api.nvim_buf_get_lines(bufnr, start_row, end_row + 1, false)
end

-- ============================================================================
-- Modifiers / annotations
-- ============================================================================

---@param ann_node TSNode
---@param bufnr integer
---@return string
local function annotation_simple_name(ann_node, bufnr)
    local name_node = field(ann_node, "name")
    if not name_node then
        return ""
    end
    return node_text(name_node, bufnr):match("([%w_]+)%s*$") or ""
end

---@class java_conv.Modifiers
---@field annotations { text: string, name: string }[]
---@field keywords string[]

--- Split a `modifiers` node into annotations and plain keywords.
---@param owner TSNode
---@param bufnr integer
---@return java_conv.Modifiers
local function parse_modifiers(owner, bufnr)
    local result = { annotations = {}, keywords = {} }
    local modifiers = child_of_type(owner, "modifiers")
    if not modifiers then
        return result
    end

    for child in modifiers:iter_children() do
        local kind = child:type()
        if kind == "marker_annotation" or kind == "annotation" then
            table.insert(result.annotations, {
                text = node_text(child, bufnr),
                name = annotation_simple_name(child, bufnr),
            })
        elseif child:named() or kind:match("^%a+$") then
            local text = node_text(child, bufnr)
            if text:match("^%a+$") then
                table.insert(result.keywords, text)
            end
        end
    end
    return result
end

---@param keywords string[]
---@return boolean
local function has_keyword(keywords, wanted)
    return vim.tbl_contains(keywords, wanted)
end

--- Insert `static` right after the access modifier, so the result reads
--- `public static final` rather than `static public`.
---@param keywords string[]
---@return string[]
local function with_static(keywords)
    if has_keyword(keywords, "static") then
        return keywords
    end

    local result = vim.deepcopy(keywords)
    local position = 1
    for index, keyword in ipairs(result) do
        if ACCESS_MODIFIERS[keyword] then
            position = index + 1
        end
    end
    table.insert(result, position, "static")
    return result
end

-- ============================================================================
-- Members
-- ============================================================================

---@class java_conv.Field
---@field node TSNode
---@field is_static boolean
---@field is_final boolean
---@field annotations string[]
---@field type string
---@field name string
---@field value string|nil

--- Expand a `field_declaration` into one entry per declarator.
---@param node TSNode
---@param bufnr integer
---@return java_conv.Field[]
local function parse_field(node, bufnr)
    local mods = parse_modifiers(node, bufnr)
    local type_node = field(node, "type")
    if not type_node then
        return {}
    end

    local annotations = {}
    for _, ann in ipairs(mods.annotations) do
        table.insert(annotations, ann.text)
    end

    local fields = {}
    for child in node:iter_children() do
        if child:type() == "variable_declarator" then
            local name_node = field(child, "name")
            local value_node = field(child, "value")
            local dimensions = field(child, "dimensions")
            if name_node then
                table.insert(fields, {
                    node = node,
                    is_static = has_keyword(mods.keywords, "static"),
                    is_final = has_keyword(mods.keywords, "final"),
                    annotations = annotations,
                    type = node_text(type_node, bufnr) .. (dimensions and node_text(dimensions, bufnr) or ""),
                    name = node_text(name_node, bufnr),
                    value = value_node and node_text(value_node, bufnr) or nil,
                })
            end
        end
    end
    return fields
end

---@class java_conv.Param
---@field annotations string[]
---@field type string
---@field name string

---@param params_node TSNode|nil
---@param bufnr integer
---@return java_conv.Param[]
local function parse_params(params_node, bufnr)
    local params = {}
    if not params_node then
        return params
    end

    for child in params_node:iter_children() do
        if child:type() == "formal_parameter" or child:type() == "spread_parameter" then
            local type_node = field(child, "type")
            local name_node = field(child, "name")
            local mods = parse_modifiers(child, bufnr)
            local annotations = {}
            for _, ann in ipairs(mods.annotations) do
                table.insert(annotations, ann.text)
            end
            if type_node and name_node then
                table.insert(params, {
                    annotations = annotations,
                    type = node_text(type_node, bufnr) .. (child:type() == "spread_parameter" and "..." or ""),
                    name = node_text(name_node, bufnr),
                })
            end
        end
    end
    return params
end

--- Statements of a constructor/compact-constructor body, without the braces.
---@param body_node TSNode|nil
---@return TSNode[]
local function body_statements(body_node)
    local statements = {}
    if not body_node then
        return statements
    end
    for child in body_node:iter_children() do
        if child:named() and not COMMENT_TYPES[child:type()] then
            table.insert(statements, child)
        end
    end
    return statements
end

--- Detect `this.x = <expr>;` / `x = <expr>;` and report the assigned field plus
--- whether the right-hand side is the bare parameter (an identity assignment).
---@param statement TSNode
---@param bufnr integer
---@return string|nil field_name, boolean is_identity, TSNode|nil rhs
local function parse_field_assignment(statement, bufnr)
    if statement:type() ~= "expression_statement" then
        return nil, false, nil
    end
    local expr = statement:named_child(0)
    if not expr or expr:type() ~= "assignment_expression" then
        return nil, false, nil
    end

    local left = field(expr, "left")
    local right = field(expr, "right")
    if not left or not right then
        return nil, false, nil
    end

    local left_text = node_text(left, bufnr)
    local name = left_text:match("^this%s*%.%s*([%w_$]+)$") or left_text:match("^([%w_$]+)$")
    if not name then
        return nil, false, nil
    end

    local is_identity = node_text(right, bufnr) == name
    return name, is_identity, right
end

--- A method that only returns a field, i.e. an accessor a record generates itself.
---@param method TSNode
---@param bufnr integer
---@param field_names table<string, boolean>
---@return string|nil backing_field
local function trivial_accessor_field(method, bufnr, field_names)
    local params = field(method, "parameters")
    if params and #parse_params(params, bufnr) > 0 then
        return nil
    end

    local body = field(method, "body")
    if not body then
        return nil
    end

    local statements = body_statements(body)
    if #statements ~= 1 or statements[1]:type() ~= "return_statement" then
        return nil
    end

    local returned = statements[1]:named_child(0)
    if not returned then
        return nil
    end

    local text = node_text(returned, bufnr)
    local name = text:match("^this%s*%.%s*([%w_$]+)$") or text:match("^([%w_$]+)$")
    if not name or not field_names[name] then
        return nil
    end

    -- Accept `x()`, `getX()` and `isX()` as the accessor shape.
    local method_name_node = field(method, "name")
    local method_name = method_name_node and node_text(method_name_node, bufnr) or ""
    local capitalized = name:sub(1, 1):upper() .. name:sub(2)
    if method_name == name or method_name == "get" .. capitalized or method_name == "is" .. capitalized then
        return name
    end
    return nil
end

-- ============================================================================
-- Buffer / text helpers
-- ============================================================================

---@param bufnr integer
---@return string
local function indent_unit(bufnr)
    if not vim.bo[bufnr].expandtab then
        return "\t"
    end
    local width = vim.bo[bufnr].shiftwidth
    if width == 0 then
        width = vim.bo[bufnr].tabstop
    end
    return string.rep(" ", width > 0 and width or 4)
end

---@param bufnr integer
---@param row integer
---@return string
local function line_indent(bufnr, row)
    local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1] or ""
    return line:match("^%s*") or ""
end

--- Whether the source body starts with a blank line right after its `{`.
--- The converted type reproduces the author's spacing instead of forcing one.
---@param body TSNode|nil
---@param bufnr integer
---@return boolean
local function body_starts_blank(body, bufnr)
    if not body then
        return false
    end
    local row = body:start() + 1
    local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1]
    return line ~= nil and vim.trim(line) == ""
end

--- Build a block appender that separates blocks with a blank line, and only
--- opens the body with a blank line when the source did.
---@param out string[]
---@param leading_blank boolean
---@return fun(block: string[])
local function block_appender(out, leading_blank)
    local first = true
    return function(block)
        if #block == 0 then
            return
        end
        if first then
            first = false
            if leading_blank then
                table.insert(out, "")
            end
        elseif out[#out] ~= "" then
            table.insert(out, "")
        end
        vim.list_extend(out, block)
    end
end

--- Re-indent a statement node to `target_indent`, preserving the relative
--- indentation of its continuation lines. `node_text` returns the first line
--- without its leading whitespace, so the original indent has to be read from
--- the buffer and stripped from every following line.
---@param statement TSNode
---@param bufnr integer
---@param target_indent string
---@return string[]
local function reindent_statement(statement, bufnr, target_indent)
    local original = line_indent(bufnr, statement:start())
    local lines = vim.split(node_text(statement, bufnr), "\n", { plain = true })

    local out = {}
    for index, text in ipairs(lines) do
        if index == 1 then
            table.insert(out, target_indent .. text)
        elseif vim.trim(text) == "" then
            table.insert(out, "")
        else
            local whitespace = text:match("^%s*") or ""
            local strip = math.min(#whitespace, #original)
            table.insert(out, target_indent .. text:sub(strip + 1))
        end
    end
    return out
end

--- Insert an import right after the package declaration when missing.
---@param bufnr integer
---@param fqn string
---@return integer inserted_lines
local function ensure_import(bufnr, fqn)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local statement = "import " .. fqn .. ";"
    local insert_at = 0

    for index, line in ipairs(lines) do
        if line == statement then
            return 0
        end
        if line:match("^package%s") or line:match("^import%s") then
            insert_at = index
        end
        if line:match("^%s*[@%w]") and not line:match("^package%s") and not line:match("^import%s") then
            break
        end
    end

    local block = { statement }
    if insert_at > 0 and not (lines[insert_at] or ""):match("^import%s") then
        table.insert(block, 1, "")
    end

    vim.api.nvim_buf_set_lines(bufnr, insert_at, insert_at, false, block)
    return #block
end

--- Drop `import ...<Name>;` lines for annotations that are no longer used.
---@param bufnr integer
---@param names string[]
---@return integer removed number of lines deleted above the declaration
local function prune_unused_imports(bufnr, names)
    local removed = 0
    if #names == 0 then
        return removed
    end

    for _, name in ipairs(names) do
        local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
        local import_row, used = nil, false

        for index, line in ipairs(lines) do
            if line:match("^import%s+[%w%.]*%." .. name .. "%s*;") then
                import_row = index - 1
            elseif line:find("@" .. name, 1, true) or line:find("%f[%w]" .. name .. "%f[^%w]") then
                used = true
            end
        end

        if import_row and not used then
            vim.api.nvim_buf_set_lines(bufnr, import_row, import_row + 1, false, {})
            removed = removed + 1

            -- Collapse the blank line an inserted import block may leave behind.
            local before = vim.api.nvim_buf_get_lines(bufnr, import_row - 1, import_row, false)[1]
            local after = vim.api.nvim_buf_get_lines(bufnr, import_row, import_row + 1, false)[1]
            if before == "" and after == "" then
                vim.api.nvim_buf_set_lines(bufnr, import_row, import_row + 1, false, {})
                removed = removed + 1
            end
        end
    end
    return removed
end

-- ============================================================================
-- Type declaration model
-- ============================================================================

---@class java_conv.TypeDecl
---@field node TSNode
---@field kind "class"|"record"
---@field bufnr integer
---@field indent string
---@field unit string
---@field annotations { text: string, name: string }[]
---@field keywords string[]
---@field name string
---@field type_params string
---@field superclass string|nil
---@field interfaces string|nil
---@field body TSNode|nil
---@field components java_conv.Param[]
---@field is_nested boolean

---@param node TSNode
---@param bufnr integer
---@return java_conv.TypeDecl
local function build_decl(node, bufnr)
    local mods = parse_modifiers(node, bufnr)
    local name_node = field(node, "name")
    local type_params = field(node, "type_parameters")
    local superclass = field(node, "superclass")
    local interfaces = field(node, "interfaces")
    local parent = node:parent()

    return {
        node = node,
        kind = node:type() == "record_declaration" and "record" or "class",
        bufnr = bufnr,
        indent = line_indent(bufnr, node:start()),
        unit = indent_unit(bufnr),
        annotations = mods.annotations,
        keywords = mods.keywords,
        name = name_node and node_text(name_node, bufnr) or "",
        type_params = type_params and node_text(type_params, bufnr) or "",
        superclass = superclass and node_text(superclass, bufnr) or nil,
        interfaces = interfaces and node_text(interfaces, bufnr) or nil,
        body = field(node, "body"),
        components = parse_params(field(node, "parameters"), bufnr),
        is_nested = parent ~= nil and NESTING_BODIES[parent:type()] == true,
    }
end

--- Move the cursor onto the `class`/`record` keyword line of a freshly written
--- declaration, so an immediate second toggle targets the same type again.
---@param bufnr integer
---@param start_row integer 0-indexed first row of the written block
---@param lines string[] the block that was written
local function focus_declaration(bufnr, start_row, lines)
    for index, line in ipairs(lines) do
        local column = line:find("%f[%w]class%f[^%w]") or line:find("%f[%w]record%f[^%w]")
        if column then
            local row = start_row + index
            if row <= vim.api.nvim_buf_line_count(bufnr) then
                pcall(vim.api.nvim_win_set_cursor, 0, { row, column - 1 })
            end
            return
        end
    end
end

--- Nearest enclosing class/record declaration, falling back to the first
--- top-level type in the file when the cursor sits outside any declaration.
---@param bufnr integer
---@return TSNode|nil
function M.type_node_at_cursor(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()

    local root = parse_root(bufnr)
    if not root then
        return nil
    end

    local cursor = vim.api.nvim_win_get_cursor(0)
    local node = root:named_descendant_for_range(cursor[1] - 1, cursor[2], cursor[1] - 1, cursor[2])
    while node do
        local kind = node:type()
        if kind == "class_declaration" or kind == "record_declaration" then
            return node
        end
        node = node:parent()
    end

    for child in root:iter_children() do
        local kind = child:type()
        if kind == "class_declaration" or kind == "record_declaration" then
            return child
        end
    end
    return nil
end

-- ============================================================================
-- Rendering
-- ============================================================================

---@param decl java_conv.TypeDecl
---@param annotations string[]
---@param keywords string[]
---@param header string
---@return string[]
local function render_header(decl, annotations, keywords, header)
    local lines = {}
    for _, annotation in ipairs(annotations) do
        for _, part in ipairs(vim.split(annotation, "\n", { plain = true })) do
            table.insert(lines, decl.indent .. part)
        end
    end

    local prefix = #keywords > 0 and (table.concat(keywords, " ") .. " ") or ""
    table.insert(lines, decl.indent .. prefix .. header)
    return lines
end

--- Render record components on one line, wrapping to one per line when long.
---@param decl java_conv.TypeDecl
---@param components java_conv.Param[]
---@param prefix string  text before the `(`
---@param suffix string  text after the `)`
---@return string[]
local function render_components(decl, components, prefix, suffix)
    local parts = {}
    for _, component in ipairs(components) do
        local annotations = #component.annotations > 0 and (table.concat(component.annotations, " ") .. " ") or ""
        table.insert(parts, annotations .. component.type .. " " .. component.name)
    end

    local single = prefix .. "(" .. table.concat(parts, ", ") .. ")" .. suffix
    if #decl.indent + #single <= 120 or #parts == 0 then
        return { single }
    end

    local continuation = decl.indent .. decl.unit .. decl.unit
    local lines = { prefix .. "(" }
    for index, part in ipairs(parts) do
        local separator = index < #parts and "," or (")" .. suffix)
        table.insert(lines, continuation:sub(#decl.indent + 1) .. part .. separator)
    end
    return lines
end

-- ============================================================================
-- class -> record
-- ============================================================================

---@param decl java_conv.TypeDecl
---@param instance_fields java_conv.Field[]
---@return string|nil error
local function class_to_record_blocker(decl, instance_fields)
    if decl.superclass then
        return "class extends another type; records implicitly extend java.lang.Record"
    end
    if has_keyword(decl.keywords, "abstract") then
        return "abstract classes cannot be records"
    end
    if #instance_fields == 0 then
        return "class has no instance fields to turn into record components"
    end
    return nil
end

---@param decl java_conv.TypeDecl
---@return boolean ok
local function class_to_record(decl)
    local bufnr = decl.bufnr
    local body = decl.body
    if not body then
        vim.notify("class->record: class has no body", vim.log.levels.WARN)
        return false
    end

    -- Lombok can make fields final without the keyword, so an explicit `final`
    -- is not required to consider the class immutable.
    local implicitly_final = false
    for _, annotation in ipairs(decl.annotations) do
        if annotation.name == "Value" or annotation.text:match("makeFinal%s*=%s*true") then
            implicitly_final = true
        end
    end

    local instance_fields, field_names, mutable = {}, {}, {}
    for child in body:iter_children() do
        if child:type() == "field_declaration" then
            for _, parsed in ipairs(parse_field(child, bufnr)) do
                if not parsed.is_static then
                    table.insert(instance_fields, parsed)
                    field_names[parsed.name] = true
                    if not parsed.is_final and not implicitly_final then
                        table.insert(mutable, parsed.name)
                    end
                end
            end
        end
    end

    local blocker = class_to_record_blocker(decl, instance_fields)
    if blocker then
        vim.notify("class->record: " .. blocker, vim.log.levels.WARN)
        return false
    end

    ---@type java_conv.Param[]
    local components = {}
    for _, parsed in ipairs(instance_fields) do
        table.insert(components, { annotations = parsed.annotations, type = parsed.type, name = parsed.name })
    end

    -- Body members, in their original order.
    local members = {}
    for child in body:iter_children() do
        local kind = child:type()

        if kind == "field_declaration" then
            local parsed = parse_field(child, bufnr)
            if #parsed > 0 and parsed[1].is_static then
                table.insert(members, member_lines(child, bufnr))
            end
        elseif kind == "constructor_declaration" then
            local params = parse_params(field(child, "parameters"), bufnr)
            local is_canonical = #params == #components
            if is_canonical then
                for index, param in ipairs(params) do
                    if param.name ~= components[index].name then
                        is_canonical = false
                        break
                    end
                end
            end

            if is_canonical then
                local compact = M.build_compact_constructor(decl, child, field_names)
                if compact then
                    table.insert(members, compact)
                end
            else
                table.insert(members, member_lines(child, bufnr))
            end
        elseif kind == "method_declaration" then
            if not trivial_accessor_field(child, bufnr, field_names) then
                table.insert(members, member_lines(child, bufnr))
            end
        elseif child:named() and not COMMENT_TYPES[kind] then
            table.insert(members, member_lines(child, bufnr))
        end
    end

    local stripped = {}
    local annotations = {}
    for _, annotation in ipairs(decl.annotations) do
        if RECORD_INCOMPATIBLE_LOMBOK[annotation.name] then
            table.insert(stripped, annotation.name)
        else
            -- Everything else (@Builder, @Slf4j, @Jacksonized, Spring, Jackson, ...)
            -- is valid on a record and is carried over unchanged.
            table.insert(annotations, annotation.text)
        end
    end

    local keywords = {}
    for _, keyword in ipairs(decl.keywords) do
        if not IMPLICIT_RECORD_MODIFIERS[keyword] then
            table.insert(keywords, keyword)
        end
    end

    -- A nested record is always static, so an inner (non-static) class loses its
    -- implicit reference to the enclosing instance.
    local became_static = decl.is_nested and not has_keyword(decl.keywords, "static")

    local suffix = (decl.interfaces and (" " .. decl.interfaces) or "") .. " {"
    local header_lines = render_components(decl, components, "record " .. decl.name .. decl.type_params, suffix)

    local lines = render_header(decl, annotations, keywords, header_lines[1])
    for index = 2, #header_lines do
        table.insert(lines, decl.indent .. header_lines[index])
    end

    local append = block_appender(lines, body_starts_blank(body, bufnr))
    for _, member in ipairs(members) do
        append(member)
    end
    table.insert(lines, decl.indent .. "}")

    vim.api.nvim_buf_set_lines(bufnr, decl.node:start(), decl.node:end_() + 1, false, lines)
    local removed = prune_unused_imports(bufnr, stripped)
    focus_declaration(bufnr, decl.node:start() - removed, lines)

    if #mutable > 0 then
        vim.notify(
            "class->record: non-final fields became immutable components: " .. table.concat(mutable, ", "),
            vim.log.levels.WARN
        )
    end
    if became_static then
        vim.notify(
            "class->record: inner class became an implicitly static record; it can no longer reference the enclosing instance",
            vim.log.levels.WARN
        )
    end
    return true
end

--- Turn a canonical constructor into a record compact constructor:
--- drop the parameter list, remove identity assignments and rewrite
--- `this.x = expr;` into `x = expr;` (the record assigns components implicitly).
---@param decl java_conv.TypeDecl
---@param ctor TSNode
---@param field_names table<string, boolean>
---@return string[]|nil
function M.build_compact_constructor(decl, ctor, field_names)
    local bufnr = decl.bufnr
    local mods = parse_modifiers(ctor, bufnr)
    local body = field(ctor, "body")
    local body_indent = decl.indent .. decl.unit
    local statement_indent = body_indent .. decl.unit

    local kept = {}
    for _, statement in ipairs(body_statements(body)) do
        local name, is_identity = parse_field_assignment(statement, bufnr)
        local text_lines = reindent_statement(statement, bufnr, statement_indent)

        if name and field_names[name] and is_identity then
            -- implicit in a compact constructor
        elseif name and field_names[name] then
            text_lines[1] = text_lines[1]:gsub("^(%s*)this%s*%.%s*", "%1")
            table.insert(kept, text_lines)
        else
            table.insert(kept, text_lines)
        end
    end

    if #kept == 0 then
        return nil
    end

    local lines = {}
    for _, annotation in ipairs(mods.annotations) do
        table.insert(lines, body_indent .. annotation.text)
    end

    local prefix = #mods.keywords > 0 and (table.concat(mods.keywords, " ") .. " ") or ""
    table.insert(lines, body_indent .. prefix .. decl.name .. " {")

    for _, statement_lines in ipairs(kept) do
        vim.list_extend(lines, statement_lines)
    end

    table.insert(lines, body_indent .. "}")
    return lines
end

-- ============================================================================
-- record -> class
-- ============================================================================

---@param decl java_conv.TypeDecl
---@return boolean ok
local function record_to_class(decl)
    local bufnr = decl.bufnr
    local body = decl.body
    if #decl.components == 0 then
        vim.notify("record->class: record has no components", vim.log.levels.WARN)
        return false
    end

    local body_indent = decl.indent .. decl.unit
    local statement_indent = body_indent .. decl.unit

    -- Components become fields; `@Value` makes them private final.
    -- Component annotations move onto their own line, matching field style.
    local field_lines = {}
    for _, component in ipairs(decl.components) do
        for _, annotation in ipairs(component.annotations) do
            table.insert(field_lines, body_indent .. annotation)
        end
        table.insert(field_lines, body_indent .. component.type .. " " .. component.name .. ";")
    end

    local static_fields, members = {}, {}
    local canonical_body, has_constructor = nil, false
    if body then
        for child in body:iter_children() do
            local kind = child:type()

            if kind == "field_declaration" then
                table.insert(static_fields, member_lines(child, bufnr))
            elseif kind == "compact_constructor_declaration" then
                has_constructor = true
                canonical_body = body_statements(field(child, "body"))
            elseif kind == "constructor_declaration" then
                has_constructor = true
                local params = parse_params(field(child, "parameters"), bufnr)
                local is_canonical = #params == #decl.components
                if is_canonical then
                    for index, param in ipairs(params) do
                        if param.name ~= decl.components[index].name then
                            is_canonical = false
                            break
                        end
                    end
                end

                if is_canonical then
                    -- Already explicit and canonical: keep verbatim, nothing to synthesize.
                    canonical_body = false
                end
                table.insert(members, member_lines(child, bufnr))
            elseif child:named() and not COMMENT_TYPES[kind] then
                table.insert(members, member_lines(child, bufnr))
            end
        end
    end

    -- Lombok skips the generated all-args constructor as soon as any explicit
    -- constructor exists, so a record with extra constructors needs an explicit
    -- canonical one to keep `this(...)` delegation compiling.
    local constructor_lines = {}
    if canonical_body ~= false and has_constructor then
        local params = {}
        for _, component in ipairs(decl.components) do
            table.insert(params, component.type .. " " .. component.name)
        end

        local signature = "public " .. decl.name .. "(" .. table.concat(params, ", ") .. ") {"
        table.insert(constructor_lines, body_indent .. signature)
        for _, statement in ipairs(canonical_body or {}) do
            vim.list_extend(constructor_lines, reindent_statement(statement, bufnr, statement_indent))
        end
        -- A compact constructor assigns its components implicitly, at the end.
        for _, component in ipairs(decl.components) do
            table.insert(
                constructor_lines,
                statement_indent .. "this." .. component.name .. " = " .. component.name .. ";"
            )
        end
        table.insert(constructor_lines, body_indent .. "}")
    end

    local annotations, has_value = {}, false
    for _, annotation in ipairs(decl.annotations) do
        if annotation.name == "Value" then
            has_value = true
        end
        -- All record annotations (@Builder, @Slf4j, @Jacksonized, Spring, ...)
        -- are equally valid on the class and are carried over unchanged.
        table.insert(annotations, annotation.text)
    end
    if not has_value then
        table.insert(annotations, 1, VALUE_ANNOTATION)
    end

    local header = "class "
        .. decl.name
        .. decl.type_params
        .. (decl.interfaces and (" " .. decl.interfaces) or "")
        .. " {"

    -- A nested record is implicitly static; the class has to say so explicitly,
    -- otherwise it becomes an inner class holding a reference to the enclosing
    -- instance (and could not be converted back).
    local keywords = decl.is_nested and with_static(decl.keywords) or decl.keywords
    local lines = render_header(decl, annotations, keywords, header)

    local append = block_appender(lines, body_starts_blank(body, bufnr))
    for _, static_field in ipairs(static_fields) do
        append(static_field)
    end
    append(field_lines)
    append(constructor_lines)
    for _, member in ipairs(members) do
        append(member)
    end
    table.insert(lines, decl.indent .. "}")

    local start_row = decl.node:start()
    vim.api.nvim_buf_set_lines(bufnr, start_row, decl.node:end_() + 1, false, lines)

    local added = 0
    if not has_value then
        added = ensure_import(bufnr, VALUE_IMPORT)
    end
    focus_declaration(bufnr, start_row + added, lines)
    return true
end

-- ============================================================================
-- Public API
-- ============================================================================

---@param expected "class"|"record"|nil
---@return java_conv.TypeDecl|nil
local function resolve_decl(expected)
    local bufnr = vim.api.nvim_get_current_buf()
    if vim.bo[bufnr].filetype ~= "java" then
        vim.notify("class<->record: not a Java buffer", vim.log.levels.WARN)
        return nil
    end

    local node = M.type_node_at_cursor(bufnr)
    if not node then
        vim.notify("class<->record: no class or record declaration found", vim.log.levels.WARN)
        return nil
    end

    local decl = build_decl(node, bufnr)
    if expected and decl.kind ~= expected then
        vim.notify("class<->record: type under cursor is a " .. decl.kind, vim.log.levels.WARN)
        return nil
    end
    return decl
end

---@param decl java_conv.TypeDecl
---@param ok boolean
local function report(decl, ok)
    if not ok then
        return
    end
    local target = decl.kind == "class" and "record" or "class"
    vim.notify(("Converted %s %s -> %s"):format(decl.kind, decl.name, target), vim.log.levels.INFO)
end

--- Convert the class under the cursor into a record.
function M.class_to_record()
    local decl = resolve_decl("class")
    if decl then
        report(decl, class_to_record(decl))
    end
end

--- Convert the record under the cursor into a Lombok `@Value` class.
function M.record_to_class()
    local decl = resolve_decl("record")
    if decl then
        report(decl, record_to_class(decl))
    end
end

--- Convert the type under the cursor to the other kind.
function M.toggle()
    local decl = resolve_decl()
    if not decl then
        return
    end
    if decl.kind == "class" then
        report(decl, class_to_record(decl))
    else
        report(decl, record_to_class(decl))
    end
end

return M
