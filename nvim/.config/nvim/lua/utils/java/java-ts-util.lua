-- Java treesitter utilities: extract class/method/package info from AST.
--
-- • get_class_name — simple class name at cursor
-- • get_class_name_with_abstract — class name + abstract flag
-- • get_class_package — full package name
-- • get_root_class_with_abstract — top-level class name + abstract
-- • get_method_name_only — method name at cursor
-- • get_enclosing_method_name_pos — method name pos + start row for a bufnr/position
-- • is_type_symbol_at — symbol at a bufnr/position is a type name (not a member)
-- • get_method_signature — method name with descriptor
-- • get_full_method — package.Class.method
-- • get_full_method_with_params — full method with parameter types
-- • get_full_method_with_params_and_abstract — full method + abstract class info
-- • is_test_method_at_cursor — cursor method carries a JUnit test annotation
-- • is_main_method_at_cursor — cursor method is a `public static void main(String[])`
-- • class_has_test_methods — file contains any JUnit test-annotated method
-- • has_main_method — file contains a `public static void main(String[])`

local M = {}

-- JUnit/Jupiter method-level annotations that mark a method as a runnable test.
-- Used by the run/test guards to tell a test method from a `main`/production one.
local TEST_ANNOTATIONS = {
    Test = true,
    ParameterizedTest = true,
    RepeatedTest = true,
    TestFactory = true,
    TestTemplate = true,
}

--- Return the Tree-sitter node at the cursor, parsing Java on demand if needed.
---@return TSNode|nil
local function node_at_cursor()
    local pos = vim.api.nvim_win_get_cursor(0)
    local row = pos[1] - 1
    local col = pos[2]
    local node = vim.treesitter.get_node({ pos = { row, col } })
    if node then
        return node
    end

    local ok, parser = pcall(vim.treesitter.get_parser, 0, "java")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return nil
    end
    return tree:root():named_descendant_for_range(row, col, row, col)
end

-- ---------------------------------------------------------
--  GET PACKAGE NAME
-- ---------------------------------------------------------
local function get_package_name(node)
    local root = node:tree():root()
    for child in root:iter_children() do
        if child:type() == "package_declaration" then
            local text = vim.treesitter.get_node_text(child, 0)
            return text:gsub("^package%s+", ""):gsub(";$", "")
        end
    end
    return nil
end

-- ---------------------------------------------------------
--  GET NESTED BINARY CLASS NAME
-- ---------------------------------------------------------
--- Build the JUnit/JVM binary class name for a class_declaration node by
--- walking up every enclosing class_declaration and joining the simple names
--- with "$" (e.g. "NestedTest$WhenDepositing"). For a top-level class this is
--- just the simple name, so callers stay backward compatible.
---@param class_node TSNode
---@return string|nil
local function nested_binary_class_name(class_node)
    local names = {}
    local node = class_node
    while node do
        if node:type() == "class_declaration" then
            for c in node:iter_children() do
                if c:type() == "identifier" then
                    table.insert(names, 1, vim.treesitter.get_node_text(c, 0))
                    break
                end
            end
        end
        node = node:parent()
    end
    if #names == 0 then
        return nil
    end
    return table.concat(names, "$")
end

-- ---------------------------------------------------------
--  GET CLASS NAME
-- ---------------------------------------------------------
--- Get the fully qualified class name at the cursor.
function M.get_class_name()
    local node = node_at_cursor()
    if not node then
        return nil
    end

    -- Climb to class_declaration
    while node and node:type() ~= "class_declaration" do
        node = node:parent()
    end
    if not node then
        return nil
    end

    local cls_name = nested_binary_class_name(node)
    if not cls_name then
        return nil
    end

    local pkg = get_package_name(node)

    if pkg then
        return pkg .. "." .. cls_name
    end
    return cls_name
end

-- ---------------------------------------------------------
--  GET CLASS NAME WITH ABSTRACT FLAG
-- ---------------------------------------------------------
--- Get the class name at the cursor with abstract status.
function M.get_class_name_with_abstract()
    local node = node_at_cursor()
    if not node then
        return nil
    end

    while node and node:type() ~= "class_declaration" do
        node = node:parent()
    end
    if not node then
        return nil
    end

    local is_abstract = false
    for c in node:iter_children() do
        if c:type() == "modifiers" then
            for m in c:iter_children() do
                if vim.treesitter.get_node_text(m, 0) == "abstract" then
                    is_abstract = true
                end
            end
        end
    end

    local cls_name = nested_binary_class_name(node)
    local pkg = get_package_name(node)
    local full_name = pkg and (pkg .. "." .. cls_name) or cls_name

    return { fqn = full_name, is_abstract = is_abstract }
end

-- ---------------------------------------------------------
--  GET PACKAGE NAME (public)
-- ---------------------------------------------------------
--- Get the package name of the current Java file.
function M.get_class_package()
    local tree = vim.treesitter.get_parser(0, "java"):parse()[1]
    if not tree then
        return nil
    end
    return get_package_name(tree:root())
end

-- ---------------------------------------------------------
--  GET ROOT CLASS FQN (first class in file, cursor-independent)
-- ---------------------------------------------------------
--- Get the top-level class name with abstract status.
function M.get_root_class_with_abstract()
    local tree = vim.treesitter.get_parser(0, "java"):parse()[1]
    if not tree then
        return nil
    end
    local root = tree:root()

    local pkg = get_package_name(root)

    for child in root:iter_children() do
        if child:type() == "class_declaration" then
            local cls_name
            local is_abstract = false
            for c in child:iter_children() do
                if c:type() == "identifier" then
                    cls_name = vim.treesitter.get_node_text(c, 0)
                end
                if c:type() == "modifiers" then
                    for m in c:iter_children() do
                        if vim.treesitter.get_node_text(m, 0) == "abstract" then
                            is_abstract = true
                        end
                    end
                end
            end
            if cls_name then
                local full_name = pkg and (pkg .. "." .. cls_name) or cls_name
                return { fqn = full_name, is_abstract = is_abstract }
            end
        end
    end
    return nil
end

-- ---------------------------------------------------------
--  GET METHOD NAME ONLY
-- ---------------------------------------------------------
local function get_method_node()
    local node = node_at_cursor()
    if not node then
        return nil
    end

    while node and node:type() ~= "method_declaration" do
        node = node:parent()
    end
    return node
end

--- Get the method name at the cursor.
function M.get_method_name_only()
    local m = get_method_node()
    if not m then
        return nil
    end

    for c in m:iter_children() do
        if c:type() == "identifier" then
            return vim.treesitter.get_node_text(c, 0)
        end
    end

    return nil
end

-- ---------------------------------------------------------
--  ENCLOSING METHOD (explicit buffer + position)
-- ---------------------------------------------------------
--- Find the `method_declaration` enclosing a position in an arbitrary buffer and
--- return its name-identifier start (for LSP requests targeting the method name)
--- plus the method node's start row (a stable dedup key). Unlike the cursor-based
--- helpers above, this operates on a background buffer (e.g. a generated
--- `*MapperImpl.java` that an LSP reference points into) that is not the current
--- window. The buffer must already be loaded (`vim.fn.bufload`).
---@param bufnr integer
---@param row integer 0-indexed row inside the method
---@param col integer 0-indexed column inside the method
---@return { name_row: integer, name_col: integer, method_row: integer }|nil
function M.get_enclosing_method_name_pos(bufnr, row, col)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return nil
    end

    local tree = parser:parse()[1]
    if not tree then
        return nil
    end

    local node = tree:root():named_descendant_for_range(row, col, row, col)
    while node and node:type() ~= "method_declaration" do
        node = node:parent()
    end
    if not node then
        return nil
    end

    local method_row = node:start()

    -- The method name is the `identifier` child of the method_declaration.
    for c in node:iter_children() do
        if c:type() == "identifier" then
            local name_row, name_col = c:start()
            return { name_row = name_row, name_col = name_col, method_row = method_row }
        end
    end

    return nil
end

-- ---------------------------------------------------------
--  TYPE-SYMBOL DETECTION (gr gating)
-- ---------------------------------------------------------
-- Declaration node kinds whose `name` identifier denotes a *type*, not a member.
local TYPE_DECLARATIONS = {
    class_declaration = true,
    interface_declaration = true,
    enum_declaration = true,
    record_declaration = true,
    annotation_type_declaration = true,
}

--- Whether the symbol at a position denotes a Java *type* (a class / record / interface /
--- enum / annotation name, or a reference to one) rather than a member (field / getter /
--- setter / record accessor / parameter / local). MapStruct `@Mapping` paths address
--- members only, so the MapStruct-aware `gr` skips its augmentation for types.
--- Detects both a type *reference* (`type_identifier` / `scoped_type_identifier` — e.g. a
--- parameter/return/field type or `new Foo()`) and a type *declaration name* (the
--- `identifier` naming a class/record/interface/enum/annotation). Operates on an arbitrary
--- buffer/position (the current model buffer where `gr` was invoked).
---@param bufnr integer
---@param row integer 0-indexed row
---@param col integer 0-indexed column
---@return boolean
function M.is_type_symbol_at(bufnr, row, col)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return false
    end
    local tree = parser:parse()[1]
    if not tree then
        return false
    end
    local node = tree:root():named_descendant_for_range(row, col, row, col)
    if not node then
        return false
    end
    local t = node:type()
    if t == "type_identifier" or t == "scoped_type_identifier" then
        return true
    end
    if t == "identifier" then
        local parent = node:parent()
        if parent and TYPE_DECLARATIONS[parent:type()] then
            local name = parent:field("name")[1]
            return name ~= nil and name:id() == node:id()
        end
    end
    return false
end

-- ---------------------------------------------------------
--  GET METHOD PARAMETER TYPES
-- ---------------------------------------------------------
local function get_method_param_types(method_node)
    local params = {}

    for c in method_node:iter_children() do
        if c:type() == "formal_parameters" then
            for p in c:iter_children() do
                if p:type() == "formal_parameter" then
                    for pc in p:iter_children() do
                        if
                            pc:type() == "type_type"
                            or pc:type() == "unann_type"
                            or pc:type() == "type_identifier"
                            or pc:type() == "generic_type"
                            or pc:type() == "array_type"
                            or pc:type() == "integral_type"
                            or pc:type() == "scoped_type_identifier"
                        then
                            table.insert(params, vim.treesitter.get_node_text(pc, 0))
                            break
                        end
                    end
                end
            end
        end
    end

    return params
end

-- ---------------------------------------------------------
--  GET FULL METHOD SIGNATURE
-- ---------------------------------------------------------
--- Get the method signature at the cursor.
function M.get_method_signature()
    local m = get_method_node()
    if not m then
        return nil
    end

    local name = M.get_method_name_only()

    local params = get_method_param_types(m)

    return name .. "(" .. table.concat(params, ", ") .. ")"
end

-- ---------------------------------------------------------
--  GET FULL METHOD WITH CLASS + PACKAGE
-- ---------------------------------------------------------
--- Get the fully qualified method name at the cursor.
function M.get_full_method(delimiter)
    delimiter = delimiter or "."
    local class = M.get_class_name()
    local sig = M.get_method_name_only()
    if not class or not sig then
        return nil
    end
    return class .. delimiter .. sig
end

-- ---------------------------------------------------------
--  GET FULL METHOD WITH CLASS + PACKAGE + PARAMS
-- ---------------------------------------------------------
--- Get the fully qualified method name with parameter types.
function M.get_full_method_with_params(delimiter)
    delimiter = delimiter or "."
    local class = M.get_class_name()
    local method_name = M.get_method_name_only()
    if not class or not method_name then
        return nil
    end

    local m = get_method_node()
    if not m then
        return nil
    end

    local params = get_method_param_types(m)
    local sig = method_name .. "(" .. table.concat(params, ", ") .. ")"

    return class .. delimiter .. sig
end

-- ---------------------------------------------------------
--  GET FULL METHOD WITH CLASS + PACKAGE + PARAMS + ABSTRACT FLAG
-- ---------------------------------------------------------
--- Get the fully qualified method with params and abstract status.
function M.get_full_method_with_params_and_abstract(delimiter)
    delimiter = delimiter or "."
    local class_info = M.get_class_name_with_abstract()
    if not class_info then
        return nil
    end

    local method_name = M.get_method_name_only()
    if not method_name then
        return nil
    end

    local m = get_method_node()
    if not m then
        return nil
    end

    local params = get_method_param_types(m)
    local sig = method_name .. "(" .. table.concat(params, ", ") .. ")"

    return {
        fsignature = class_info.fqn .. delimiter .. sig,
        is_abstract = class_info.is_abstract,
    }
end

-- ---------------------------------------------------------
--  TEST / MAIN CLASSIFICATION (run vs test guards)
-- ---------------------------------------------------------
-- Resolve the simple (unqualified) name of an annotation node. Handles both
-- `@Test` (marker_annotation) and `@RepeatedTest(3)` (annotation), and strips
-- any package qualifier, e.g. `org.junit.jupiter.api.Test` -> `Test`.
---@param ann_node TSNode
---@return string|nil
local function annotation_simple_name(ann_node)
    local name_node = ann_node:field("name")[1]
    if not name_node then
        return nil
    end
    local text = vim.treesitter.get_node_text(name_node, 0)
    return text:match("([%w_]+)%s*$")
end

-- Collect the set of annotation simple-names declared on a method's `modifiers`.
---@param method_node TSNode
---@return table<string, boolean>
local function method_annotation_names(method_node)
    local names = {}
    for c in method_node:iter_children() do
        if c:type() == "modifiers" then
            for m in c:iter_children() do
                local t = m:type()
                if t == "marker_annotation" or t == "annotation" then
                    local n = annotation_simple_name(m)
                    if n then
                        names[n] = true
                    end
                end
            end
        end
    end
    return names
end

---@param method_node TSNode
---@return boolean
local function method_is_test(method_node)
    for name in pairs(method_annotation_names(method_node)) do
        if TEST_ANNOTATIONS[name] then
            return true
        end
    end
    return false
end

-- Detect the conventional entry point `public static void main(String[] args)`.
-- We only require name == "main" and the `static` modifier; that is enough to
-- distinguish a runnable class from a test without false positives in practice.
---@param method_node TSNode
---@return boolean
local function method_is_main(method_node)
    local name, is_static
    for c in method_node:iter_children() do
        local t = c:type()
        if t == "identifier" then
            name = vim.treesitter.get_node_text(c, 0)
        elseif t == "modifiers" then
            for m in c:iter_children() do
                if vim.treesitter.get_node_text(m, 0) == "static" then
                    is_static = true
                end
            end
        end
    end
    return name == "main" and is_static == true
end

---@param predicate fun(method_node:TSNode):boolean
---@return boolean
local function any_method_in_file(predicate)
    local ok, parser = pcall(vim.treesitter.get_parser, 0, "java")
    if not ok or not parser then
        return false
    end
    local tree = parser:parse()[1]
    if not tree then
        return false
    end

    local found = false
    local function walk(node)
        if found then
            return
        end
        if node:type() == "method_declaration" and predicate(node) then
            found = true
            return
        end
        for c in node:iter_children() do
            walk(c)
        end
    end
    walk(tree:root())
    return found
end

--- Whether the method under the cursor carries a JUnit test annotation.
---@return boolean
function M.is_test_method_at_cursor()
    local m = get_method_node()
    if not m then
        return false
    end
    return method_is_test(m)
end

--- Whether the method under the cursor is a `static main` entry point.
---@return boolean
function M.is_main_method_at_cursor()
    local m = get_method_node()
    if not m then
        return false
    end
    return method_is_main(m)
end

--- Whether the current file declares any JUnit test-annotated method.
---@return boolean
function M.class_has_test_methods()
    return any_method_in_file(method_is_test)
end

--- Whether the current file declares a `static main` entry point.
---@return boolean
function M.has_main_method()
    return any_method_in_file(method_is_main)
end

return M
