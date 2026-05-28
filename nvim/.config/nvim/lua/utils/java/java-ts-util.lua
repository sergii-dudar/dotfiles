-- Java treesitter utilities: extract class/method/package info from AST.
--
-- • get_class_name — simple class name at cursor
-- • get_class_name_with_abstract — class name + abstract flag
-- • get_class_package — full package name
-- • get_root_class_with_abstract — top-level class name + abstract
-- • get_method_name_only — method name at cursor
-- • get_method_signature — method name with descriptor
-- • get_full_method — package.Class.method
-- • get_full_method_with_params — full method with parameter types
-- • get_full_method_with_params_and_abstract — full method + abstract class info

local M = {}

-- Modern Neovim Treesitter API
local function node_at_cursor()
    local pos = vim.api.nvim_win_get_cursor(0)
    return vim.treesitter.get_node({ pos = { pos[1] - 1, pos[2] } })
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

    -- Extract class identifier
    local cls_name
    for c in node:iter_children() do
        if c:type() == "identifier" then
            cls_name = vim.treesitter.get_node_text(c, 0)
            break
        end
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

    local cls_name
    local is_abstract = false
    for c in node:iter_children() do
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

return M
