-- Shared treesitter helpers for C# test discovery.
-- Used by the cs runner (cursor -> test method/class, classes in file) and by the
-- cs test-report adapter (method positions + container line for signs/marks).
--
-- A "test method" is a method_declaration carrying one of the known test
-- attributes (xunit/nunit/mstest). The fully-qualified class name is built as
-- `<namespace>.<outer>+<inner>` to match the TRX `className` convention.

local M = {}

-- Attribute (base) names that mark a method as a test across the three frameworks.
local TEST_ATTRS = {
    Fact = true, -- xunit
    Theory = true, -- xunit / nunit
    Test = true, -- nunit
    TestCase = true, -- nunit
    TestCaseSource = true, -- nunit
    TestMethod = true, -- mstest
    DataTestMethod = true, -- mstest
}

---@param source integer|string  bufnr or file path
---@return TSNode|nil root, string src
local function get_root_src(source)
    local ok = pcall(vim.treesitter.language.add, "c_sharp")
    if not ok then
        return nil, ""
    end
    if type(source) == "number" then
        local src = table.concat(vim.api.nvim_buf_get_lines(source, 0, -1, false), "\n")
        local parser = vim.treesitter.get_parser(source, "c_sharp")
        if not parser then
            return nil, src
        end
        return parser:parse()[1]:root(), src
    end
    if vim.fn.filereadable(source) ~= 1 then
        return nil, ""
    end
    local src = table.concat(vim.fn.readfile(source), "\n")
    local parser = vim.treesitter.get_string_parser(src, "c_sharp")
    return parser:parse()[1]:root(), src
end

---@param node TSNode
---@param src string
---@return string
local function field_text(node, src)
    local f = node:field("name")[1]
    return f and vim.treesitter.get_node_text(f, src) or ""
end

--- Strip a possible namespace qualifier from an attribute name (Xunit.Fact -> Fact).
---@param name string
---@return string
local function base_attr_name(name)
    return name:match("([%w_]+)$") or name
end

--- Does this method_declaration carry a recognised test attribute?
---@param method TSNode
---@param src string
---@return boolean
local function method_has_test_attr(method, src)
    for child in method:iter_children() do
        if child:type() == "attribute_list" then
            for attr in child:iter_children() do
                if attr:type() == "attribute" then
                    local nm = attr:field("name")[1]
                    if nm then
                        local base = base_attr_name(vim.treesitter.get_node_text(nm, src))
                        if TEST_ATTRS[base] then
                            return true
                        end
                    end
                end
            end
        end
    end
    return false
end

--- File-scoped namespaces (`namespace X;`) are siblings of the type declarations
--- rather than parents, so resolve the file-level namespace up front.
---@param root TSNode
---@param src string
---@return string
local function file_scoped_namespace(root, src)
    for child in root:iter_children() do
        if child:type() == "file_scoped_namespace_declaration" then
            return field_text(child, src)
        end
    end
    return ""
end

---@class cs.TestEntry
---@field method string
---@field line integer       0-based line of the method name
---@field class_fqn string   e.g. "Ns.Outer+Inner"
---@field class_line integer 0-based line of the (innermost) class name

--- Core walk: collect every test method with its owning class FQN + class line.
---@param source integer|string
---@return string namespace, cs.TestEntry[] tests
local function collect_entries(source)
    local root, src = get_root_src(source)
    if not root then
        return "", {}
    end
    local base_ns = file_scoped_namespace(root, src)
    local tests = {}

    ---@param node TSNode
    ---@param namespace string
    ---@param class_stack { name: string, line: integer }[]
    local function walk(node, namespace, class_stack)
        local t = node:type()
        if t == "namespace_declaration" then
            local nm = field_text(node, src)
            if nm ~= "" then
                namespace = namespace == "" and nm or (namespace .. "." .. nm)
            end
        elseif t == "class_declaration" then
            local name_node = node:field("name")[1]
            local line = name_node and ({ name_node:range() })[1] or ({ node:range() })[1]
            class_stack = vim.deepcopy(class_stack)
            table.insert(class_stack, { name = field_text(node, src), line = line })
        elseif t == "method_declaration" then
            if #class_stack > 0 and method_has_test_attr(node, src) then
                local name_node = node:field("name")[1]
                if name_node then
                    local names = {}
                    for _, c in ipairs(class_stack) do
                        table.insert(names, c.name)
                    end
                    local class_fqn = (namespace ~= "" and (namespace .. ".") or "") .. table.concat(names, "+")
                    table.insert(tests, {
                        method = field_text(node, src),
                        line = ({ name_node:range() })[1],
                        class_fqn = class_fqn,
                        class_line = class_stack[#class_stack].line,
                    })
                end
            end
            return -- tests cannot be nested inside a method
        end
        for child in node:iter_children() do
            walk(child, namespace, class_stack)
        end
    end

    walk(root, base_ns, {})
    return base_ns, tests
end

--- All test methods in a file.
---@param source integer|string
---@return { namespace: string, tests: cs.TestEntry[] }
function M.collect(source)
    local namespace, tests = collect_entries(source)
    return { namespace = namespace, tests = tests }
end

--- Distinct class FQNs in a file that contain at least one test method.
---@param source integer|string
---@return { fqn: string, line: integer }[]
function M.test_classes(source)
    local _, tests = collect_entries(source)
    local classes = {}
    local seen = {}
    for _, tst in ipairs(tests) do
        if not seen[tst.class_fqn] then
            seen[tst.class_fqn] = true
            table.insert(classes, { fqn = tst.class_fqn, line = tst.class_line })
        end
    end
    return classes
end

--- Method-name -> 0-based line map plus the first test-class line (container).
--- Consumed by the test-report core for placing signs/marks.
---@param source integer|string
---@return table<string, integer> positions, integer|nil container_line
function M.positions(source)
    local _, tests = collect_entries(source)
    local positions = {}
    local container_line
    for _, tst in ipairs(tests) do
        positions[tst.method] = tst.line
        if container_line == nil then
            container_line = tst.class_line
        end
    end
    return positions, container_line
end

--- Resolve the innermost test method whose range contains the given 0-based row.
---@param bufnr integer
---@param row integer 0-based
---@return cs.TestEntry|nil
function M.test_at_cursor(bufnr, row)
    local root, src = get_root_src(bufnr)
    if not root then
        return nil
    end
    local base_ns = file_scoped_namespace(root, src)
    local best

    local function walk(node, namespace, class_stack)
        local t = node:type()
        if t == "namespace_declaration" then
            local nm = field_text(node, src)
            if nm ~= "" then
                namespace = namespace == "" and nm or (namespace .. "." .. nm)
            end
        elseif t == "class_declaration" then
            local name_node = node:field("name")[1]
            local line = name_node and ({ name_node:range() })[1] or ({ node:range() })[1]
            class_stack = vim.deepcopy(class_stack)
            table.insert(class_stack, { name = field_text(node, src), line = line })
        elseif t == "method_declaration" then
            local sr, _, er = node:range()
            if row >= sr and row <= er and #class_stack > 0 and method_has_test_attr(node, src) then
                local name_node = node:field("name")[1]
                local names = {}
                for _, c in ipairs(class_stack) do
                    table.insert(names, c.name)
                end
                best = {
                    method = field_text(node, src),
                    class_fqn = (namespace ~= "" and (namespace .. ".") or "") .. table.concat(names, "+"),
                    line = name_node and ({ name_node:range() })[1] or sr,
                    class_line = class_stack[#class_stack].line,
                }
            end
            return
        end
        for child in node:iter_children() do
            walk(child, namespace, class_stack)
        end
    end

    walk(root, base_ns, {})
    return best
end

return M
