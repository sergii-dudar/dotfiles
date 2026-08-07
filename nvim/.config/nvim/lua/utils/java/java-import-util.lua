-- Java import management: check existence of imports and add new ones.
--
-- • static_import_exists — check if a static import for a member exists
-- • import_exists — check if a class import exists
-- • import_class_and_replace — import class under cursor and replace with simple name

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

--- Replace qualified class references without modifying Java import declarations.
---@param full_class string
---@param simple_class string
local function replace_full_to_simple_class_name(full_class, simple_class)
    local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
    local full_class_pattern = vim.pesc(full_class)
    for i, line in ipairs(lines) do
        if not line:match("^%s*import%s") then
            lines[i] = string.gsub(line, full_class_pattern, simple_class)
        end
    end
    vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
end

-- Import java class name under cursor, and apply simple class name in all buffer
--- Import the class under the cursor and replace usages.
function M.import_class_and_replace()
    local simple_class_name = vim.fn.expand("<cword>")
    local full_name_under_cursor = vim.fn.expand("<cWORD>")

    -- Don't anchor with `^` — the qualifier is often wrapped in punctuation
    -- (e.g., `((Something.generate()))`, `caller(Something.generate(), ...)`),
    -- so an anchored match would miss it and the user would see a bogus
    -- "already imported" notification while nothing got rewritten.
    local remove_all_part = full_name_under_cursor:match("([%w%.]+)%." .. simple_class_name .. "%(?")

    if not remove_all_part then
        vim.notify("class '" .. simple_class_name .. "' already was imported!", vim.log.levels.INFO)
        return
    end

    -- Bare lowercase identifier (e.g., `someVar.method()`) is an instance
    -- access, not a class qualifier — bail out instead of fabricating an
    -- import like `import someVar.method;`.
    if not remove_all_part:find("%.") and not remove_all_part:match("^[A-Z]") then
        vim.notify("'" .. remove_all_part .. "' is not a class qualifier — nothing to import", vim.log.levels.INFO)
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
        -- The qualifier may be a type declared in this very file — the enclosing
        -- type itself or one of its nested types (e.g. `BalanceBookingResolver`
        -- inside `BalanceBookingResolver.java`). Its members are reachable by
        -- simple name inside the class body, so static members need no import.
        local java_ts_util = require("utils.java.java-ts-util")
        local same_file_type = java_ts_util.declared_type_names()[class_name_for_static]

        -- A nested type's simple name is not in scope in its enclosing class
        -- header. Reuse the top-level type's FQCN so a regular self-import can
        -- make the replacement valid there as well as inside the class body.
        if same_file_type and not fqcn_for_static then
            local root_class = java_ts_util.get_root_class_with_abstract()
            local root_class_name = root_class and root_class.fqn:match("([^%.]+)$")
            if root_class_name == class_name_for_static then
                fqcn_for_static = root_class.fqn
            end
        end

        if not fqcn_for_static and not same_file_type then
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
            -- PascalCase member (starts upper, has lowercase) → nested class.
            -- Lowercase or ALL_CAPS member → static method / constant.
            local is_nested_class = member:match("^[A-Z]") and member:match("[a-z]") ~= nil
            local can_replace = not same_file_type or not is_nested_class or fqcn_for_static ~= nil

            if can_replace then
                -- Replace the complete FQCN first; otherwise replacing only the
                -- `ClassName.member` suffix leaves a broken package-qualified member.
                if fqcn_for_static then
                    replace_full_to_simple_class_name(fqcn_for_static .. "." .. member, member)
                end
                replace_full_to_simple_class_name(class_name_for_static .. "." .. member, member)
            end

            -- Same-file static members need no import. A same-file nested type
            -- does need one when its qualifier is removed from the class header.
            if not same_file_type or (is_nested_class and fqcn_for_static ~= nil) then
                local stmt = (is_nested_class and "import " or "import static ")
                    .. fqcn_for_static
                    .. "."
                    .. member
                    .. ";"
                insert_import(stmt)
            end
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
