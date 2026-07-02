-- MapStruct method-scoped reference resolution (pure, no LSP / no async).
--
-- Turns a location inside a generated `*MapperImpl.java` into the exact interface
-- `@Mapping` / `@ValueMapping` declarations that produced the referenced field access or
-- enum-constant use. Used by `reference_finder.lua` to replace the old whole-file textual
-- scan (which matched any path segment with the same name, regardless of its owning type).
--
-- Model (from the generated impl):
--   • Public mapping methods carry `@Override`; generated helpers never do (they are
--     `private`/`protected` and override nothing). -> `@Override` is the discriminator.
--   • Simple/inline access lives in the `@Override` method: `dto.setter( person.getX() )`.
--   • Nested paths compile to helpers named after the path
--     (`personOrdersFirstItemsFirstProductName`) that `return chain;`, and are called
--     from the mapping method: `dto.firstProductName( helper( person ) )`.
--   • Helpers may call helpers (nested-target builders), so caller-resolution recurses.
--   • The sink (the target being written) pins the exact `@Mapping`: builder
--     `x.firstProductName(`, JavaBean `x.setName(`, or field `x.iban = `.
--   • `@ValueMapping` (enum) compiles to a `switch` whose arms are `case NEW: … =
--     Type.PENDING;`; the enum constant under the cursor keys the `@ValueMapping` instead
--     of a setter sink.
--
-- Given a reference we walk up the call graph to the enclosing `@Override` mapping
-- method(s) and pick, at each, the target-setter "sink" (or the enum constant for
-- @ValueMapping). Matching the interface `@Mapping` by its *target* (not source path) is
-- what makes `gr` type-precise.
--
-- All functions are exposed for headless unit testing against real generated files.

local M = {}

-- Statement leaders that can never start a builder/JavaBean sink statement.
local KEYWORDS = {
    ["return"] = true,
    ["if"] = true,
    ["for"] = true,
    ["while"] = true,
    ["switch"] = true,
    ["else"] = true,
    ["throw"] = true,
    ["new"] = true,
    ["this"] = true,
    ["super"] = true,
    ["do"] = true,
    ["try"] = true,
    ["catch"] = true,
    ["synchronized"] = true,
}

-- Guard against pathological (or cyclic) helper graphs.
local MAX_DEPTH = 8

--- Parse a buffer with the Java tree-sitter grammar and return its root node.
---@param buf integer
---@return TSNode|nil
local function get_root(buf)
    local ok, parser = pcall(vim.treesitter.get_parser, buf, "java")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return nil
    end
    return tree:root()
end

--- Text of a node's `name` field (annotation / method / …), falling back to the first
--- identifier-like child. Handles both simple (`Mapping`) and scoped (`org.….Mapping`).
---@param node TSNode
---@param buf integer
---@return string|nil
local function name_text(node, buf)
    local ok, fields = pcall(function()
        return node:field("name")
    end)
    if ok and fields and fields[1] then
        return vim.treesitter.get_node_text(fields[1], buf)
    end
    for c in node:iter_children() do
        local t = c:type()
        if t == "identifier" or t == "scoped_identifier" then
            return vim.treesitter.get_node_text(c, buf)
        end
    end
    return nil
end

--- Count formal parameters (including `@MappingTarget` and varargs) of a method node.
---@param method_node TSNode
---@return integer
local function count_params(method_node)
    local count = 0
    for c in method_node:iter_children() do
        if c:type() == "formal_parameters" then
            for p in c:iter_children() do
                local pt = p:type()
                if pt == "formal_parameter" or pt == "spread_parameter" or pt == "receiver_parameter" then
                    count = count + 1
                end
            end
        end
    end
    return count
end

--- Whether a method node's modifiers contain `@Override`.
---@param method_node TSNode
---@param buf integer
---@return boolean
local function has_override(method_node, buf)
    for c in method_node:iter_children() do
        if c:type() == "modifiers" then
            for m in c:iter_children() do
                local mt = m:type()
                if mt == "marker_annotation" or mt == "annotation" then
                    local nm = name_text(m, buf)
                    if nm == "Override" or (nm and nm:match("%.Override$")) then
                        return true
                    end
                end
            end
        end
    end
    return false
end

--- Info about the `method_declaration` enclosing a position in a (loaded) buffer.
---@param buf integer
---@param row integer 0-indexed
---@param col integer 0-indexed
---@return { name: string, is_override: boolean, param_count: integer, method_row: integer }|nil
function M.enclosing_method_info(buf, row, col)
    local root = get_root(buf)
    if not root then
        return nil
    end
    local node = root:named_descendant_for_range(row, col, row, col)
    while node and node:type() ~= "method_declaration" do
        node = node:parent()
    end
    if not node then
        return nil
    end
    local name = name_text(node, buf)
    if not name then
        return nil
    end
    return {
        name = name,
        is_override = has_override(node, buf),
        param_count = count_params(node),
        method_row = node:start(),
    }
end

--- JavaBeans property name for a setter/builder method or field identifier.
--- `setName`->`name`, `setIBAN`->`IBAN`, `setSPerson`->`SPerson` (two leading capitals
--- stay), `setDetailName`->`detailName`; builder/field names pass through (`firstProductName`,
--- `iban`).
---@param ident string
---@return string
function M.bean_property(ident)
    local rest = ident:match("^set(%u[%w_$]*)$")
    if not rest then
        return ident
    end
    if #rest > 1 and rest:sub(2, 2):match("%u") then
        return rest
    end
    return rest:sub(1, 1):lower() .. rest:sub(2)
end

--- Target property written by a sink statement, or nil if the line is not a sink.
--- Recognises `builderVar.prop( … )`, `builderVar.setProp( … )` and `builderVar.field = …`
--- (single `=`, not `==`). The leftmost `var.member` on the statement is the sink; any
--- getter chain / helper call sits to its right.
---@param line string
---@return string|nil
function M.extract_sink_property(line)
    local trimmed = line:gsub("^%s+", "")
    local var, prop = trimmed:match("^([%w_]+)%.([%w_]+)%s*%(")
    if not var then
        var, prop = trimmed:match("^([%w_]+)%.([%w_]+)%s*=%s*[^=]")
    end
    if not var or not prop or KEYWORDS[var] then
        return nil
    end
    return M.bean_property(prop)
end

--- The enum constant a `@ValueMapping` reference points at, or nil when the line is not a
--- generated switch arm. `@ValueMapping(target=…, source=…)` compiles to
--- `case NEW: … = Type.PENDING;` — both the source (`case NEW`) and an enum target
--- (`Type.PENDING`) sit on a line that starts with `case`. The identifier at the reference
--- column is the constant (`NEW` when the cursor searched the source enum, `PENDING` when
--- it searched the target enum); it keys the @ValueMapping match. String source/target
--- arms are literals, correctly not surfaced by an enum `gr`.
---@param line string
---@param col integer 0-indexed reference column
---@return string|nil
function M.extract_value_constant(line, col)
    if not line:match("^%s*case%f[%W]") then
        return nil
    end
    return line:sub(col + 1):match("^([%w_]+)")
end

--- Rows (0-indexed) where `helper_name` is *called* (its declaration excluded, since the
--- declaration's enclosing method is the helper itself).
---@param buf integer
---@param buf_lines string[]
---@param helper_name string
---@return { row: integer, col: integer }[]
function M.find_call_sites(buf, buf_lines, helper_name)
    local results = {}
    local pat = "%f[%w_]" .. vim.pesc(helper_name) .. "%s*%("
    for i = 1, #buf_lines do
        local c = buf_lines[i]:find(pat)
        if c then
            local row = i - 1
            local info = M.enclosing_method_info(buf, row, c - 1)
            if info and info.name ~= helper_name then
                results[#results + 1] = { row = row, col = c - 1 }
            end
        end
    end
    return results
end

--- Walk from a reference/call-site up to the enclosing `@Override` mapping method(s),
--- collecting descriptors. A property `@Mapping` reference yields `{ …, sink }`; a
--- `@ValueMapping` enum-switch reference yields `{ …, value }`. A helper defers to its
--- callers; when the current statement is itself a sink (nested-target sub-helper), its
--- (leaf) property overrides the sink carried up from the caller.
---@param buf integer
---@param buf_lines string[]
---@param row integer
---@param col integer
---@param visited table<string, boolean>
---@param depth integer
---@return { method_name: string, param_count: integer, sink?: string, value?: string }[]
local function resolve(buf, buf_lines, row, col, visited, depth)
    local info = M.enclosing_method_info(buf, row, col)
    if not info then
        return {}
    end

    local line = buf_lines[row + 1] or ""
    local own_sink = M.extract_sink_property(line)

    if info.is_override then
        if own_sink then
            return { { method_name = info.name, param_count = info.param_count, sink = own_sink } }
        end
        -- @ValueMapping: enum switch arms (`case NEW: … = Type.PENDING;`) have no setter
        -- sink; the enum constant under the cursor keys the @ValueMapping instead.
        local value = M.extract_value_constant(line, col)
        if value then
            return { { method_name = info.name, param_count = info.param_count, value = value } }
        end
        return {}
    end

    if depth <= 0 or visited[info.name] then
        return {}
    end
    visited[info.name] = true

    local collected = {}
    for _, cs in ipairs(M.find_call_sites(buf, buf_lines, info.name)) do
        for _, s in ipairs(resolve(buf, buf_lines, cs.row, cs.col, visited, depth - 1)) do
            collected[#collected + 1] = {
                method_name = s.method_name,
                param_count = s.param_count,
                sink = own_sink or s.sink,
            }
        end
    end

    visited[info.name] = nil
    return collected
end

--- Resolve a reference position in a generated impl to the deduplicated set of
--- descriptors (property `sink` for @Mapping, or enum `value` for @ValueMapping) that
--- consumed the referenced field / enum constant.
---@param buf integer
---@param row integer 0-indexed
---@param col integer 0-indexed
---@return { method_name: string, param_count: integer, sink?: string, value?: string }[]
function M.resolve_sinks(buf, row, col)
    local buf_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local raw = resolve(buf, buf_lines, row, col, {}, MAX_DEPTH)

    local seen, out = {}, {}
    for _, s in ipairs(raw) do
        local key = s.method_name .. "#" .. s.param_count .. "#" .. (s.sink or "") .. "#" .. (s.value or "")
        if not seen[key] then
            seen[key] = true
            out[#out + 1] = s
        end
    end
    return out
end

--- Whether a MapStruct target path's first or last segment equals the sink property.
--- (First covers a top-level setter feeding a nested target; last covers a nested-leaf
--- setter — e.g. `item.details.detailName` matched by `detailName`.)
---@param target string
---@param sink string
---@return boolean
function M.target_matches(target, sink)
    local first, last
    for seg in target:gmatch("[%w_$]+") do
        first = first or seg
        last = seg
    end
    return first == sink or last == sink
end

--- All `method_declaration` nodes in a buffer (interface or abstract-class mapper).
---@param buf integer
---@return TSNode[]
local function all_methods(buf)
    local root = get_root(buf)
    if not root then
        return {}
    end
    local out = {}
    local function walk(node)
        if node:type() == "method_declaration" then
            out[#out + 1] = node
        end
        for c in node:iter_children() do
            walk(c)
        end
    end
    walk(root)
    return out
end

--- Interface `@Mapping` declarations for a resolved sink: only real (non-commented,
--- tree-sitter parsed) `@Mapping` annotations on the method `method_name` with
--- `param_count` parameters whose target first/last segment equals `sink`.
---@param iface_buf integer
---@param method_name string
---@param param_count integer
---@param sink string
---@return { lnum: integer, col: integer, text: string }[]
function M.find_method_mappings(iface_buf, method_name, param_count, sink)
    local results = {}

    for _, method in ipairs(all_methods(iface_buf)) do
        if name_text(method, iface_buf) == method_name and count_params(method) == param_count then
            for c in method:iter_children() do
                if c:type() == "modifiers" then
                    for ann in c:iter_children() do
                        if ann:type() == "annotation" and name_text(ann, iface_buf) == "Mapping" then
                            local text = vim.treesitter.get_node_text(ann, iface_buf)
                            local target = text:match('target%s*=%s*"([^"]*)"')
                            if target and M.target_matches(target, sink) then
                                local srow, scol = ann:start()
                                local line = vim.api.nvim_buf_get_lines(iface_buf, srow, srow + 1, false)[1]
                                results[#results + 1] = {
                                    lnum = srow + 1,
                                    col = scol,
                                    text = (line or text):gsub("^%s+", ""),
                                }
                            end
                        end
                    end
                end
            end
        end
    end

    return results
end

--- Interface `@ValueMapping` declarations for a resolved enum constant: real (non-
--- commented, tree-sitter parsed) `@ValueMapping` annotations on the method `method_name`
--- with `param_count` parameters whose source or target string equals `value`. Mirrors
--- find_method_mappings for @Mapping. Constant-reference forms (`MappingConstants.*`) are
--- unquoted and intentionally not matched (they are not user enum constants).
---@param iface_buf integer
---@param method_name string
---@param param_count integer
---@param value string
---@return { lnum: integer, col: integer, text: string }[]
function M.find_method_value_mappings(iface_buf, method_name, param_count, value)
    local results = {}

    for _, method in ipairs(all_methods(iface_buf)) do
        if name_text(method, iface_buf) == method_name and count_params(method) == param_count then
            for c in method:iter_children() do
                if c:type() == "modifiers" then
                    for ann in c:iter_children() do
                        if ann:type() == "annotation" and name_text(ann, iface_buf) == "ValueMapping" then
                            local text = vim.treesitter.get_node_text(ann, iface_buf)
                            local source = text:match('source%s*=%s*"([^"]*)"')
                            local target = text:match('target%s*=%s*"([^"]*)"')
                            if source == value or target == value then
                                local srow, scol = ann:start()
                                local line = vim.api.nvim_buf_get_lines(iface_buf, srow, srow + 1, false)[1]
                                results[#results + 1] = {
                                    lnum = srow + 1,
                                    col = scol,
                                    text = (line or text):gsub("^%s+", ""),
                                }
                            end
                        end
                    end
                end
            end
        end
    end

    return results
end

--- Convenience: all interface `@Mapping` / `@ValueMapping` items for a list of resolved
--- descriptors, deduplicated by line. Dispatches on the descriptor shape: a property
--- `sink` matches `@Mapping(target ~ sink)`; an enum `value` matches
--- `@ValueMapping(source|target == value)`.
---@param iface_buf integer
---@param sinks { method_name: string, param_count: integer, sink?: string, value?: string }[]
---@return { lnum: integer, col: integer, text: string }[]
function M.mappings_for_sinks(iface_buf, sinks)
    local seen, out = {}, {}
    for _, s in ipairs(sinks) do
        local items
        if s.sink then
            items = M.find_method_mappings(iface_buf, s.method_name, s.param_count, s.sink)
        elseif s.value then
            items = M.find_method_value_mappings(iface_buf, s.method_name, s.param_count, s.value)
        else
            items = {}
        end
        for _, item in ipairs(items) do
            if not seen[item.lnum] then
                seen[item.lnum] = true
                out[#out + 1] = item
            end
        end
    end
    return out
end

--- Property + accessor identifier names a `gr` on this symbol should also gather
--- references for. From `name` / `getName` / `setName` / `isName` (or a record accessor
--- `name`) we derive the property `name`, then the JavaBeans `get/set/is<Name>`. Simple
--- first-char decapitalization (the common camelCase case); unusual all-caps acronym
--- accessors like `getIBAN` are not auto-derived, but the symbol under the cursor is
--- always queried directly regardless (see reference_finder).
---@param cword string
---@return table<string, boolean>
function M.property_sibling_names(cword)
    local prop = cword:match("^get(%u[%w_$]*)$") or cword:match("^set(%u[%w_$]*)$") or cword:match("^is(%u[%w_$]*)$")
    if prop then
        prop = prop:sub(1, 1):lower() .. prop:sub(2)
    else
        prop = cword
    end
    local upper = prop:sub(1, 1):upper() .. prop:sub(2)
    return {
        [cword] = true,
        [prop] = true,
        ["get" .. upper] = true,
        ["set" .. upper] = true,
        ["is" .. upper] = true,
    }
end

--- The `class` / `record` / `interface` / `enum` declaration enclosing a position.
---@param root TSNode
---@param row integer
---@param col integer
---@return TSNode|nil
local function enclosing_type_node(root, row, col)
    local node = root:named_descendant_for_range(row, col, row, col)
    while node do
        local t = node:type()
        if
            t == "class_declaration"
            or t == "record_declaration"
            or t == "interface_declaration"
            or t == "enum_declaration"
        then
            return node
        end
        node = node:parent()
    end
    return nil
end

--- Declaration positions (0-indexed) of a property's field + accessors WITHIN THE SAME
--- type as the cursor. Scoping to the enclosing type is essential: several model classes
--- can share one file (e.g. nested `TestClasses.*`), each with its own `name`/`getName`
--- — searching the whole buffer would re-introduce the cross-type over-matching we just
--- fixed. Only the record-header components and DIRECT body members are inspected (nested
--- type declarations are not descended into).
---@param buf integer
---@param row integer 0-indexed
---@param col integer 0-indexed
---@param names table<string, boolean>
---@return { line: integer, character: integer }[]
function M.sibling_declaration_positions(buf, row, col, names)
    local root = get_root(buf)
    if not root then
        return {}
    end
    local type_node = enclosing_type_node(root, row, col)
    if not type_node then
        return {}
    end

    local positions, seen = {}, {}
    local function add(id_node)
        if not id_node then
            return
        end
        if names[vim.treesitter.get_node_text(id_node, buf)] then
            local r, c = id_node:start()
            local key = r .. ":" .. c
            if not seen[key] then
                seen[key] = true
                positions[#positions + 1] = { line = r, character = c }
            end
        end
    end

    -- Record components live in the record header, not the body.
    for child in type_node:iter_children() do
        if child:type() == "formal_parameters" then
            for p in child:iter_children() do
                if p:type() == "formal_parameter" then
                    add(p:field("name")[1])
                end
            end
        end
    end

    -- Direct body members only (do not descend into nested type declarations).
    local body = type_node:field("body")[1]
    if not body then
        for child in type_node:iter_children() do
            local ct = child:type()
            if ct == "class_body" or ct == "interface_body" or ct == "enum_body" then
                body = child
                break
            end
        end
    end
    if body then
        for member in body:iter_children() do
            local mt = member:type()
            if mt == "method_declaration" then
                add(member:field("name")[1])
            elseif mt == "field_declaration" or mt == "constant_declaration" then
                for d in member:iter_children() do
                    if d:type() == "variable_declarator" then
                        add(d:field("name")[1])
                    end
                end
            end
        end
    end

    return positions
end

return M
