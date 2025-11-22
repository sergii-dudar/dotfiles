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
                        if pc:type() == "type_type" or pc:type() == "unann_type" then
                            table.insert(params, vim.treesitter.get_node_text(pc, 0))
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
function M.get_full_method(delimiter)
    delimiter = delimiter or "."
    local class = M.get_class_name()
    local sig = M.get_method_name_only()
    if not class or not sig then
        return nil
    end
    return class .. delimiter .. sig
end

return M