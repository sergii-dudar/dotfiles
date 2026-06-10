-- Rust treesitter utilities used by the run/test guards.
--
-- Unlike Java there is no `src/test/` path convention — Rust unit tests live in
-- `#[cfg(test)] mod tests { … }` blocks next to production code, and a binary's
-- `main.rs` commonly holds both `fn main` and a test module. So detection here is
-- deliberately cursor-scoped (am I inside a test fn / test module right now?) with
-- a separate file-scoped check for the "run every test in this file" case.
--
-- • is_test_context_at_cursor — cursor sits in a `#[test]` fn or a `#[cfg(test)]` mod
-- • file_has_tests — file declares any test fn or `#[cfg(test)]` module
-- • has_main_function — file declares a top-level `fn main`

local M = {}

local function node_at_cursor()
    local pos = vim.api.nvim_win_get_cursor(0)
    return vim.treesitter.get_node({ pos = { pos[1] - 1, pos[2] } })
end

-- Resolve the attribute path of an `attribute_item`, e.g. `#[tokio::test]` ->
-- "tokio::test", `#[cfg(test)]` -> "cfg".
---@param attr_item TSNode
---@return string|nil
local function attribute_path(attr_item)
    for c in attr_item:iter_children() do
        if c:type() == "attribute" then
            for a in c:iter_children() do
                local t = a:type()
                if t == "identifier" or t == "scoped_identifier" then
                    return vim.treesitter.get_node_text(a, 0)
                end
            end
        end
    end
    return nil
end

-- A test attribute is `#[test]` or any path whose final segment carries the word
-- `test` (`tokio::test`, `async_std::test`, `rstest`, `test_case`, …). `#[cfg(test)]`
-- is intentionally excluded: its path is `cfg`, so it never matches here.
---@param path string|nil
---@return boolean
local function is_test_attr_path(path)
    if not path then
        return false
    end
    local last = path:match("([%w_]+)%s*$") or path
    return last == "test" or last:match("test") ~= nil
end

-- Whether the contiguous attribute_items preceding a function_item mark it as a
-- test. Attributes are sibling nodes that come before the function in Rust's AST.
---@param fn_node TSNode
---@return boolean
local function function_is_test(fn_node)
    local node = fn_node:prev_sibling()
    while node do
        local t = node:type()
        if t == "attribute_item" then
            if is_test_attr_path(attribute_path(node)) then
                return true
            end
        elseif t == "line_comment" or t == "block_comment" then
            -- doc/comment between attribute and fn — keep scanning upward
        else
            break
        end
        node = node:prev_sibling()
    end
    return false
end

-- Whether a `mod_item` is annotated with `#[cfg(test)]`.
---@param mod_node TSNode
---@return boolean
local function mod_is_cfg_test(mod_node)
    local node = mod_node:prev_sibling()
    while node do
        local t = node:type()
        if t == "attribute_item" then
            if attribute_path(node) == "cfg" then
                local text = vim.treesitter.get_node_text(node, 0)
                if text:match("%(%s*test%s*%)") then
                    return true
                end
            end
        elseif t == "line_comment" or t == "block_comment" then
            -- skip
        else
            break
        end
        node = node:prev_sibling()
    end
    return false
end

--- Whether the cursor is inside a test function or a `#[cfg(test)]` module.
---@return boolean
function M.is_test_context_at_cursor()
    local node = node_at_cursor()
    while node do
        local t = node:type()
        if t == "function_item" and function_is_test(node) then
            return true
        end
        if t == "mod_item" and mod_is_cfg_test(node) then
            return true
        end
        node = node:parent()
    end
    return false
end

---@param predicate fun(node:TSNode):boolean
---@return boolean
local function any_node_in_file(predicate)
    local ok, parser = pcall(vim.treesitter.get_parser, 0, "rust")
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
        if predicate(node) then
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

--- Whether the file declares any test function or a `#[cfg(test)]` module.
---@return boolean
function M.file_has_tests()
    return any_node_in_file(function(node)
        local t = node:type()
        if t == "function_item" then
            return function_is_test(node)
        end
        if t == "mod_item" then
            return mod_is_cfg_test(node)
        end
        return false
    end)
end

--- Whether the file declares a top-level `fn main`.
---@return boolean
function M.has_main_function()
    local ok, parser = pcall(vim.treesitter.get_parser, 0, "rust")
    if not ok or not parser then
        return false
    end
    local tree = parser:parse()[1]
    if not tree then
        return false
    end
    for child in tree:root():iter_children() do
        if child:type() == "function_item" then
            for c in child:iter_children() do
                if c:type() == "identifier" and vim.treesitter.get_node_text(c, 0) == "main" then
                    return true
                end
            end
        end
    end
    return false
end

return M
