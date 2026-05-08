local M = {}

-- Function to check if the import already exists
local function import_exists(import_statement)
    for _, line in ipairs(vim.api.nvim_buf_get_lines(0, 0, -1, false)) do
        if line:match(import_statement) then
            return true
        end
    end
    return false
end

-- Find FQCN for a class from existing regular imports in the buffer
local function find_import_fqcn(class_name)
    local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
    local pattern = "^import%s+([%w%.]+%." .. vim.pesc(class_name) .. ")%s*;%s*$"
    for _, line in ipairs(lines) do
        local fqcn = line:match(pattern)
        if fqcn then
            return fqcn
        end
    end
    return nil
end

--- Check if an explicit static import for `member` already exists in the buffer.
--- Matches `import static <fqcn>.<member>;` — wildcard imports are not
--- inspected (we can't know which members they pull in without resolution).
---@param member string
---@param bufnr integer
---@return boolean
function M.static_import_exists(member, bufnr)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local pattern = "^import%s+static%s+.+%." .. vim.pesc(member) .. "%s*;%s*$"
    for _, line in ipairs(lines) do
        if line:match(pattern) then
            return true
        end
    end
    return false
end

--- Check if a regular (non-static) type import for `class_name` already
--- exists. Matches `import <fqcn>.<class_name>;`. Static imports are excluded
--- because the FQCN segment is matched as `[%w%.]+`, which cannot span the
--- whitespace between `static` and the package.
---@param class_name string
---@param bufnr integer
---@return boolean
function M.import_exists(class_name, bufnr)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local pattern = "^import%s+[%w%.]+%." .. vim.pesc(class_name) .. "%s*;%s*$"
    for _, line in ipairs(lines) do
        if line:match(pattern) then
            return true
        end
    end
    return false
end

-- Function to insert the import if it doesn't exist
local function insert_import(import_statement)
    if not import_exists(import_statement) then
        -- Find the correct place to insert the import
        local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
        local insert_line = 0
        for i, line in ipairs(lines) do
            if line:match("^package ") then
                insert_line = i + 1
                break
            elseif line:match("^import ") then
                insert_line = i
            end
        end
        -- Insert the import
        vim.api.nvim_buf_set_lines(0, insert_line, insert_line, false, { import_statement })
    end
end

-- Function to replace the class under cursor
local function replace_full_to_simple_class_name(full_class, simple_class)
    local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
    for i, line in ipairs(lines) do
        lines[i] = string.gsub(line, full_class, simple_class)
    end
    vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
end

-- Import java class name under cursor, and apply simple class name in all buffer
function M.import_class_and_replace()
    local simple_class_name = vim.fn.expand("<cword>")
    local full_name_under_cursor = vim.fn.expand("<cWORD>")

    local remove_all_part = full_name_under_cursor:match("^([%w%.]+)%." .. simple_class_name .. "%(?")

    if not remove_all_part then
        vim.notify("class '" .. simple_class_name .. "' already was imported!", vim.log.levels.INFO)
        return
    end

    -- Determine if this is a "convert to static import" case:
    -- 1. Prefix has no dots and starts with uppercase (e.g., "BooleanUtils.and") → class already imported
    -- 2. Prefix has dots and last segment starts with uppercase (e.g., "org...BooleanUtils.and") → FQCN static
    local is_static_import = false
    local class_name_for_static = nil
    local fqcn_for_static = nil

    if not remove_all_part:find("%.") then
        if remove_all_part:match("^[A-Z]") then
            is_static_import = true
            class_name_for_static = remove_all_part
            fqcn_for_static = find_import_fqcn(class_name_for_static)
        end
    else
        local last_segment = remove_all_part:match("%.([^%.]+)$")
        if last_segment and last_segment:match("^[A-Z]") then
            is_static_import = true
            class_name_for_static = last_segment
            fqcn_for_static = remove_all_part
        end
    end

    if is_static_import then
        if not fqcn_for_static then
            vim.notify(
                "Cannot find import for '" .. (class_name_for_static or simple_class_name) .. "'",
                vim.log.levels.WARN
            )
            return
        end

        -- Collect every `ClassName.member` referenced in the buffer so a single
        -- invocation converts all usages of the class to static imports, not
        -- only the one under the cursor.
        local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
        local members_in_order = {}
        local seen = {}
        local class_pesc = vim.pesc(class_name_for_static)
        for _, line in ipairs(lines) do
            local idx = 1
            while true do
                local s, e, member = line:find(class_pesc .. "%.([%w_]+)", idx)
                if not s then
                    break
                end
                -- Reject when ClassName is part of a longer identifier or FQCN.
                local prev = s > 1 and line:sub(s - 1, s - 1) or ""
                if prev ~= "." and not prev:match("[%w_]") then
                    if not seen[member] then
                        seen[member] = true
                        table.insert(members_in_order, member)
                    end
                end
                idx = e + 1
            end
        end

        if not seen[simple_class_name] then
            table.insert(members_in_order, simple_class_name)
        end

        for _, member in ipairs(members_in_order) do
            -- PascalCase member (starts upper, has lowercase) → nested class → regular import.
            -- Lowercase or ALL_CAPS member → static method / constant → static import.
            local is_nested_class = member:match("^[A-Z]") and member:match("[a-z]") ~= nil
            local stmt = (is_nested_class and "import " or "import static ")
                .. fqcn_for_static
                .. "."
                .. member
                .. ";"
            replace_full_to_simple_class_name(class_name_for_static .. "." .. member, member)
            insert_import(stmt)
        end
    else
        local full_class_name = remove_all_part .. "." .. simple_class_name

        local is_constant = simple_class_name:match("^[A-Z_][A-Z0-9_]*$") ~= nil
        local import_statement = (is_constant and "import static " or "import ") .. full_class_name .. ";"

        replace_full_to_simple_class_name(full_class_name, simple_class_name)
        insert_import(import_statement)
    end
end

return M
