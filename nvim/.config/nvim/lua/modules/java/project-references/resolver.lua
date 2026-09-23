--- Treesitter-side of `modules.java.project-references`: decides, without any
--- LSP round-trip, whether a textual `<method>(` / `::<method>` hit is a call on
--- the target type.
---
--- Per file it parses the source once (`M.load`), indexes every typed
--- declaration (fields, locals, parameters, for/try/catch variables) with its
--- scope node, and then classifies each hit (`M.classify`) by resolving the
--- receiver expression to a declared simple type name:
---
--- - `foo.m()` / `foo::m`         -> declared type of `foo` in the innermost enclosing scope
--- - `this.foo.m()`               -> declared type of field `foo`
--- - `m()` / `this.m()`           -> enclosing type chain (or its superclass, or a static import)
--- - `super.m()`                  -> superclass of the enclosing type
--- - `Type.m()` / `Type::m`       -> the type itself (static call), by UpperCamel heuristic
--- - `new Type().m()`             -> the created type
--- - `((Type) x).m()`             -> the cast type
--- - `verify(x).m()` / `when(x).m()` / `given(x)…` / `then(x).should()…` -> declared type of the mock `x`
--- - anything else (call chains, untyped lambda params, `var` without `new`) -> unresolved
---
--- Types are compared by *simple* name only (no import resolution); that is
--- deliberate — it keeps the whole thing a few milliseconds per file and is
--- accurate enough for one project tree.
---
--- Public API:
--- - `M.load(path, cache)`                   parse + index one file (main loop only)
--- - `M.load_buffer(bufnr)`                  same for a loaded buffer's current text
--- - `M.classify(entry, row, col, target)`   verdict for the identifier at a position
--- - `M.resolve_target(bufnr, row, col)`     `{ class, method }` for the method under the cursor
--- - `M.type_simple_name(node, src)`         simple name of a treesitter type node

local M = {}

---@class java.ProjectRefs.Target
---@field class string simple name of the declaring / receiver type
---@field method string method name
---@field origin "declaration"|"lsp"|"receiver" how the class was determined

---@class java.ProjectRefs.Decl
---@field type TSNode declared type node
---@field value TSNode|nil initializer (for `var` inference)
---@field scope TSNode node whose range delimits where the declaration is visible

---@class java.ProjectRefs.Entry
---@field path string
---@field src string full file text
---@field root TSNode
---@field decls table<string, java.ProjectRefs.Decl[]> declarations by variable name
---@field parser vim.treesitter.LanguageTree kept alive with the tree
---@field tree TSTree

---@alias java.ProjectRefs.Verdict
---| "match"      # receiver resolves to the target type (or it is the target declaration itself)
---| "other"      # receiver resolves to a different type (info = that type's simple name)
---| "candidate"  # receiver could not be resolved (call chain, untyped lambda, ...)
---| "drop"       # not a call of that method at all (declaration elsewhere, field, comment...)

-- Node kinds whose `name` field declares a Java type.
local CLASS_LIKE = {
    class_declaration = true,
    interface_declaration = true,
    enum_declaration = true,
    record_declaration = true,
    annotation_type_declaration = true,
}

-- Declaration shapes indexed per file. Each fragment is parsed on its own first so
-- an older java grammar that lacks one node kind only loses that shape, not all of them.
local QUERY_FRAGMENTS = {
    [[(field_declaration type: (_) @type declarator: (variable_declarator name: (identifier) @name)) @decl]],
    [[(local_variable_declaration type: (_) @type declarator: (variable_declarator name: (identifier) @name)) @decl]],
    [[(formal_parameter type: (_) @type name: (identifier) @name) @decl]],
    [[(spread_parameter (_) @type (variable_declarator name: (identifier) @name)) @decl]],
    [[(enhanced_for_statement type: (_) @type name: (identifier) @name) @decl]],
    [[(resource type: (_) @type name: (identifier) @name) @decl]],
    [[(catch_formal_parameter (catch_type) @type name: (identifier) @name) @decl]],
}

local decl_query ---@type vim.treesitter.Query|false|nil

--- Node text against a string or buffer source.
---@param node TSNode
---@param src string|integer
---@return string
local function text(node, src)
    return vim.treesitter.get_node_text(node, src)
end

--- Escape a literal for use inside a Lua pattern.
---@param s string
---@return string
local function escape_lua_pattern(s)
    return (s:gsub("[%^%$%(%)%%%.%[%]%*%+%-%?]", "%%%0"))
end

--- Build (once) the combined declaration query from every fragment the installed grammar accepts.
---@return vim.treesitter.Query|nil
local function get_decl_query()
    if decl_query ~= nil then
        return decl_query or nil
    end
    local parts = {}
    for _, fragment in ipairs(QUERY_FRAGMENTS) do
        if pcall(vim.treesitter.query.parse, "java", fragment) then
            parts[#parts + 1] = fragment
        end
    end
    local ok, query = pcall(vim.treesitter.query.parse, "java", table.concat(parts, "\n"))
    decl_query = ok and query or false
    return decl_query or nil
end

--- Simple (unqualified, non-generic) name of a treesitter type node, or nil for
--- primitives / arrays / anything that cannot carry the target method.
---@param node TSNode|nil
---@param src string|integer
---@return string|nil
function M.type_simple_name(node, src)
    if not node then
        return nil
    end
    local t = node:type()
    if t == "type_identifier" then
        return text(node, src)
    elseif t == "scoped_type_identifier" then
        -- `a.b.Outer.Inner` -> last type_identifier
        local last
        for child in node:iter_children() do
            if child:type() == "type_identifier" then
                last = child
            end
        end
        return last and text(last, src) or nil
    elseif t == "generic_type" or t == "catch_type" or t == "annotated_type" then
        -- raw type is the first named child that resolves (skips annotations / type_arguments)
        for child in node:iter_children() do
            if child:named() then
                local name = M.type_simple_name(child, src)
                if name then
                    return name
                end
            end
        end
    end
    return nil
end

--- Node whose byte range delimits where a declaration is visible.
---@param decl TSNode
---@return TSNode
local function scope_of(decl)
    local t = decl:type()
    if t == "formal_parameter" or t == "spread_parameter" then
        -- formal_parameters -> method / constructor / lambda / record / catch
        local params = decl:parent()
        return params and params:parent() or params or decl
    elseif t == "resource" then
        -- resource_specification -> try_with_resources_statement
        local spec = decl:parent()
        return spec and spec:parent() or spec or decl
    elseif t == "enhanced_for_statement" then
        return decl
    end
    -- field: class_body; local: enclosing block; catch parameter: catch_clause
    return decl:parent() or decl
end

--- Index every typed declaration of a parsed file by variable name.
---@param root TSNode
---@param src string
---@return table<string, java.ProjectRefs.Decl[]>
local function index_declarations(root, src)
    local decls = {} ---@type table<string, java.ProjectRefs.Decl[]>
    local query = get_decl_query()
    if not query then
        return decls
    end
    local caps = {}
    for i, name in ipairs(query.captures) do
        caps[name] = i
    end
    for _, match in query:iter_matches(root, src, 0, -1, { all = true }) do
        local function node_of(cap)
            local n = match[caps[cap]]
            if type(n) == "table" then
                return n[1]
            end
            return n
        end
        local name_node, type_node, decl_node = node_of("name"), node_of("type"), node_of("decl")
        -- `(spread_parameter (_) @type ...)` also matches with `modifiers` bound to @type
        if name_node and type_node and decl_node and type_node:type() ~= "modifiers" then
            local name = text(name_node, src)
            local declarator = name_node:parent()
            local value = declarator and declarator:type() == "variable_declarator" and declarator:field("value")[1]
                or nil
            local list = decls[name] or {}
            list[#list + 1] = { type = type_node, value = value, scope = scope_of(decl_node) }
            decls[name] = list
        end
    end
    return decls
end

--- Parse Java source text into an entry. Must run on the main loop (treesitter
--- may need to load the language / query files through the API).
---@param path string identity of the source (file path or `buffer:<n>`)
---@param src string
---@return java.ProjectRefs.Entry|nil
local function parse_source(path, src)
    local ok, parser = pcall(vim.treesitter.get_string_parser, src, "java")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return nil
    end
    local root = tree:root()
    return {
        path = path,
        src = src,
        root = root,
        decls = index_declarations(root, src),
        parser = parser,
        tree = tree,
    }
end

--- Parse + index a file from disk, memoised in `cache` (false = unreadable / unparsable).
--- Main loop only — see `parse_source`.
---@param path string absolute path
---@param cache table<string, java.ProjectRefs.Entry|false>
---@return java.ProjectRefs.Entry|nil
function M.load(path, cache)
    local cached = cache[path]
    if cached ~= nil then
        return cached or nil
    end
    local fh = io.open(path, "rb")
    if not fh then
        cache[path] = false
        return nil
    end
    local src = fh:read("*a")
    fh:close()
    local entry = src and parse_source(path, src) or nil
    cache[path] = entry or false
    return entry
end

--- Parse + index the *current text* of a loaded buffer (unsaved edits included).
---@param bufnr integer
---@return java.ProjectRefs.Entry|nil
function M.load_buffer(bufnr)
    local src = table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), "\n")
    return parse_source("buffer:" .. bufnr, src)
end

--- Enclosing type declarations of a node, innermost first, each with its
--- superclass simple name when it is a class.
---@param node TSNode
---@param src string|integer
---@return { name: string|nil, superclass: string|nil, node: TSNode }[]
local function enclosing_classes(node, src)
    local result = {}
    local cur = node:parent()
    while cur do
        if CLASS_LIKE[cur:type()] then
            local name = cur:field("name")[1]
            local superclass
            local sc = cur:field("superclass")[1]
            if sc then
                for child in sc:iter_children() do
                    if child:named() then
                        superclass = M.type_simple_name(child, src)
                        break
                    end
                end
            end
            result[#result + 1] = { name = name and text(name, src) or nil, superclass = superclass, node = cur }
        end
        cur = cur:parent()
    end
    return result
end

--- Declared simple type of a variable name as seen from `at_node` (innermost
--- enclosing declaration scope wins). `var x = new Foo()` resolves to `Foo`.
---@param entry java.ProjectRefs.Entry
---@param name string
---@param at_node TSNode
---@return string|nil type simple name
---@return "resolved"|"unresolved"|"none" status `none` = no in-scope declaration at all
local function declared_type(entry, name, at_node)
    local list = entry.decls[name]
    if not list then
        return nil, "none"
    end
    local _, _, pos = at_node:start()
    local best, best_len
    for _, decl in ipairs(list) do
        local _, _, s, _, _, e = decl.scope:range(true)
        if pos >= s and pos <= e then
            local len = e - s
            if not best or len < best_len then
                best, best_len = decl, len
            end
        end
    end
    if not best then
        return nil, "none"
    end
    local tname = M.type_simple_name(best.type, entry.src)
    if tname == "var" then
        tname = nil
        if best.value and best.value:type() == "object_creation_expression" then
            tname = M.type_simple_name(best.value:field("type")[1], entry.src)
        end
    end
    if not tname then
        return nil, "unresolved"
    end
    return tname, "resolved"
end

--- Whether an identifier reads like a type name (UpperCamel, not an ALL_CAPS constant).
---@param name string
---@return boolean
local function looks_like_type(name)
    return name:match("^%u") ~= nil and name:match("%l") ~= nil
end

--- Declared type of a plain variable expression: `x` or `this.x`.
---@param entry java.ProjectRefs.Entry
---@param node TSNode
---@param at_node TSNode scope anchor
---@return string|nil type simple name
---@return "resolved"|"unresolved"|"none" status
local function variable_type(entry, node, at_node)
    local nt = node:type()
    if nt == "identifier" then
        return declared_type(entry, text(node, entry.src), at_node)
    elseif nt == "field_access" then
        local obj, field = node:field("object")[1], node:field("field")[1]
        if obj and field and obj:type() == "this" then
            return declared_type(entry, text(field, entry.src), at_node)
        end
    end
    return nil, "none"
end

-- Mockito-style wrappers whose first argument is the mock the chained call targets:
-- `verify(mock).m()`, `when(mock).m()`, `given(mock).m()`, `then(mock).should().m()`,
-- `doX().when(mock).m()`, `inOrder.verify(mock, times(1)).m()`.
local MOCK_WRAPPERS = { verify = true, when = true, given = true, ["then"] = true }

--- Type of the mock handed to a Mockito wrapper anywhere down a call chain.
---@param entry java.ProjectRefs.Entry
---@param receiver TSNode method_invocation receiver
---@param at_node TSNode scope anchor
---@return string|nil type simple name
local function mock_wrapper_type(entry, receiver, at_node)
    local node = receiver
    local depth = 0
    while node and node:type() == "method_invocation" and depth < 8 do
        local name = node:field("name")[1]
        if name and MOCK_WRAPPERS[text(name, entry.src)] then
            local args = node:field("arguments")[1]
            local first = args and args:named_child(0)
            local tname = first and variable_type(entry, first, at_node)
            if tname then
                return tname
            end
        end
        node = node:field("object")[1]
        depth = depth + 1
    end
    return nil
end

--- Resolve what a call's receiver expression denotes.
---@param entry java.ProjectRefs.Entry
---@param receiver TSNode|nil `object` of a method_invocation / first child of a method_reference
---@param at_node TSNode the method-name identifier (scope anchor)
---@return "type"|"self"|"this"|"super"|"unresolved" kind
---@return string|nil name simple type name when kind == "type"
---@return "mock"|nil via set when the type came from a Mockito wrapper argument
local function receiver_type(entry, receiver, at_node)
    if not receiver then
        return "self"
    end
    local src = entry.src
    local rt = receiver:type()
    if rt == "this" then
        return "this"
    elseif rt == "super" then
        return "super"
    elseif rt == "identifier" then
        local name = text(receiver, src)
        local declared, status = declared_type(entry, name, at_node)
        if declared then
            return "type", declared
        end
        if status == "none" and looks_like_type(name) then
            return "type", name -- static call / method reference on a type
        end
        return "unresolved"
    elseif rt == "field_access" then
        local obj, field = receiver:field("object")[1], receiver:field("field")[1]
        if not field then
            return "unresolved"
        end
        local fname = text(field, src)
        if obj and obj:type() == "this" then
            local declared = declared_type(entry, fname, at_node)
            if declared then
                return "type", declared
            end
            return "unresolved"
        end
        if looks_like_type(fname) then
            return "type", fname -- qualified static call `a.b.Type.m()`
        end
        return "unresolved"
    elseif rt == "method_invocation" then
        local mocked = mock_wrapper_type(entry, receiver, at_node)
        if mocked then
            return "type", mocked, "mock"
        end
        return "unresolved"
    elseif rt == "scoped_identifier" then
        local name = receiver:field("name")[1]
        if name then
            return "type", text(name, src)
        end
        return "unresolved"
    elseif rt == "type_identifier" or rt == "scoped_type_identifier" or rt == "generic_type" then
        local name = M.type_simple_name(receiver, src)
        if name then
            return "type", name
        end
        return "unresolved"
    elseif rt == "object_creation_expression" then
        local name = M.type_simple_name(receiver:field("type")[1], src)
        if name then
            return "type", name
        end
        return "unresolved"
    elseif rt == "parenthesized_expression" then
        local inner = receiver:named_child(0)
        if inner and inner:type() == "cast_expression" then
            local name = M.type_simple_name(inner:field("type")[1], src)
            if name then
                return "type", name
            end
        end
        return "unresolved"
    end
    return "unresolved"
end

--- Whether the file statically imports `Class.method` or `Class.*`.
---@param entry java.ProjectRefs.Entry
---@param class string
---@param method string
---@return boolean
local function has_static_import(entry, class, method)
    local prefix = "import%s+static%s+[%w_$.]*%f[%w_$]" .. escape_lua_pattern(class) .. "%."
    return entry.src:find(prefix .. escape_lua_pattern(method) .. "%s*;") ~= nil
        or entry.src:find(prefix .. "%*%s*;") ~= nil
end

--- Split a call-site node into its receiver, or nil when the identifier is not
--- the *name* of a method_invocation / method_reference.
---@param node TSNode identifier
---@param parent TSNode
---@return boolean is_call
---@return TSNode|nil receiver
local function call_receiver(node, parent)
    local ptype = parent:type()
    if ptype == "method_invocation" then
        local name = parent:field("name")[1]
        if not (name and name:equal(node)) then
            return false
        end
        return true, parent:field("object")[1]
    elseif ptype == "method_reference" then
        local count = parent:named_child_count()
        local last = parent:named_child(count - 1)
        if not (last and last:equal(node)) then
            return false
        end
        return true, count > 1 and parent:named_child(0) or nil
    end
    return false
end

--- Classify the identifier at a position against the target.
---@param entry java.ProjectRefs.Entry
---@param row integer 0-indexed
---@param col integer 0-indexed byte column of the method name
---@param target java.ProjectRefs.Target
---@return java.ProjectRefs.Verdict verdict
---@return string|nil info "other": the resolved type; "match": "declaration" for the target's own
--- declaration, "mock" for a Mockito `verify/when/given/then(mock)` chain, nil for a plain call
function M.classify(entry, row, col, target)
    local src = entry.src
    local node = entry.root:named_descendant_for_range(row, col, row, col)
    if not node or node:type() ~= "identifier" or text(node, src) ~= target.method then
        return "drop"
    end
    local parent = node:parent()
    if not parent then
        return "drop"
    end

    if parent:type() == "method_declaration" then
        local name = parent:field("name")[1]
        if name and name:equal(node) then
            local cls = enclosing_classes(parent, src)[1]
            if cls and cls.name == target.class then
                return "match", "declaration"
            end
        end
        return "drop"
    end

    local is_call, receiver = call_receiver(node, parent)
    if not is_call then
        return "drop"
    end

    local kind, name, via = receiver_type(entry, receiver, node)
    if kind == "type" then
        if name == target.class then
            return "match", via
        end
        return "other", name
    elseif kind == "self" then
        local chain = enclosing_classes(node, src)
        for _, cls in ipairs(chain) do
            if cls.name == target.class or cls.superclass == target.class then
                return "match"
            end
        end
        if has_static_import(entry, target.class, target.method) then
            return "match"
        end
        return "other", chain[1] and chain[1].name or nil
    elseif kind == "this" then
        local cls = enclosing_classes(node, src)[1]
        if cls and (cls.name == target.class or cls.superclass == target.class) then
            return "match"
        end
        return "other", cls and cls.name or nil
    elseif kind == "super" then
        local cls = enclosing_classes(node, src)[1]
        if cls and cls.superclass == target.class then
            return "match"
        end
        return "other", cls and cls.superclass or nil
    end
    return "candidate"
end

--- Simple class name from a jdtls `jdt://contents/<jar>/<pkg>/<Outer$Inner>.class?...` URI.
---@param uri string
---@return string|nil
local function class_from_jdt_uri(uri)
    local name = uri:match("/([^/?]+)%.class")
    if not name then
        return nil
    end
    return name:match("([^$]+)$")
end

--- Declaring type of the symbol under the cursor via a synchronous LSP definition
--- request (`textDocument/definition`, first client / first location). Uses the
--- current window's cursor, so `row`/`col` must be that cursor.
---@param bufnr integer
---@param cache table<string, java.ProjectRefs.Entry|false>
---@param timeout_ms integer
---@return string|nil class simple name
local function lsp_declaring_class(bufnr, cache, timeout_ms)
    local clients = vim.lsp.get_clients({ bufnr = bufnr, method = "textDocument/definition" })
    local client = clients[1]
    if not client then
        return nil
    end
    local params = vim.lsp.util.make_position_params(0, client.offset_encoding)
    local ok, results = pcall(vim.lsp.buf_request_sync, bufnr, "textDocument/definition", params, timeout_ms)
    if not ok or not results then
        return nil
    end
    for _, res in pairs(results) do
        local result = res.result
        local loc = result and (vim.islist(result) and result[1] or result) or nil
        local uri = loc and (loc.uri or loc.targetUri)
        local range = loc and (loc.range or loc.targetSelectionRange or loc.targetRange)
        if uri and range then
            if uri:match("^jdt://") then
                return class_from_jdt_uri(uri)
            end
            local path = vim.uri_to_fname(uri)
            local entry
            local def_bufnr = vim.fn.bufnr(path)
            if def_bufnr ~= -1 and vim.api.nvim_buf_is_loaded(def_bufnr) then
                entry = M.load_buffer(def_bufnr)
            else
                entry = M.load(path, cache)
            end
            if entry then
                local r, c = range.start.line, range.start.character
                local node = entry.root:named_descendant_for_range(r, c, r, c)
                local cls = node and enclosing_classes(node, entry.src)[1]
                if cls and cls.name then
                    return cls.name
                end
            end
        end
    end
    return nil
end

--- Work out which `<Type>#<method>` the cursor is on.
---
--- On a method *declaration* the enclosing type is the target — no LSP needed,
--- so it is instant even before jdtls is ready. On a *call site* the declaring
--- type comes from a sync LSP definition lookup, falling back to the receiver's
--- declared type in the current buffer when no client answers.
---@param bufnr integer
---@param row integer 0-indexed cursor row
---@param col integer 0-indexed cursor column
---@param opts? { cache?: table, lsp_timeout_ms?: integer }
---@return java.ProjectRefs.Target|nil target
---@return string|nil err
function M.resolve_target(bufnr, row, col, opts)
    opts = opts or {}
    local entry = M.load_buffer(bufnr)
    if not entry then
        return nil, "no Java treesitter parser available"
    end
    local node = entry.root:named_descendant_for_range(row, col, row, col)
    if not node or node:type() ~= "identifier" then
        return nil, "put the cursor on a method name"
    end
    local parent = node:parent()
    if not parent then
        return nil, "put the cursor on a method name"
    end
    local method = text(node, entry.src)

    if parent:type() == "method_declaration" then
        local cls = enclosing_classes(parent, entry.src)[1]
        if not (cls and cls.name) then
            return nil, "no enclosing type for method " .. method
        end
        return { class = cls.name, method = method, origin = "declaration" }
    end

    local is_call, receiver = call_receiver(node, parent)
    if not is_call then
        return nil, "put the cursor on a method name"
    end

    local class = lsp_declaring_class(bufnr, opts.cache or {}, opts.lsp_timeout_ms or 2000)
    if class then
        return { class = class, method = method, origin = "lsp" }
    end

    local kind, name = receiver_type(entry, receiver, node)
    if kind == "type" then
        class = name
    elseif kind == "self" or kind == "this" then
        local cls = enclosing_classes(node, entry.src)[1]
        class = cls and cls.name
    elseif kind == "super" then
        local cls = enclosing_classes(node, entry.src)[1]
        class = cls and cls.superclass
    end
    if not class then
        return nil, ("cannot resolve the receiver type of %s() — is jdtls attached?"):format(method)
    end
    return { class = class, method = method, origin = "receiver" }
end

return M
