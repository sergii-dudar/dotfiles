local util = require("utils.common-util")
local mapstruct = require("utils.java.mapstruct")
local jdtls_util = require("utils.java.jdtls-util")

local M = {}

-- Cache project root (git root or cwd) - this won't change during session
-- local git_root = vim.fn.systemlist("git rev-parse --show-toplevel")[1]
-- local project_root = (git_root and git_root ~= "") and git_root or vim.fn.getcwd()

-- Try to find a Java file directly in the project by FQN
-- This is much faster than using jdtls for project files
-- @param class_fqn Fully qualified class name (e.g., "com.example.Address")
-- @return file_path or nil if not found
local function find_project_file(class_fqn)
    -- Convert FQN to relative path: com.example.Address -> com/example/Address.java
    local relative_path = class_fqn:gsub("%.", "/") .. ".java"

    -- Search patterns with ** to match both single and multi-module projects
    -- **/ matches zero or more directories, so it finds:
    -- - project/src/main/java/Foo.java (single-module)
    -- - project/module-a/src/main/java/Foo.java (multi-module)
    -- - project/services/user-service/src/main/java/Foo.java (nested multi-module)
    local search_patterns = {
        "**/src/main/java/" .. relative_path,
        "**/src/test/java/" .. relative_path,
        "**/src/" .. relative_path,
    }

    for _, pattern in ipairs(search_patterns) do
        local matches = vim.fn.glob(project_root .. "/" .. pattern, false, true)
        if matches and #matches > 0 then
            -- Return first match (most likely the correct one)
            return matches[1]
        end
    end

    -- Last resort: Generic glob search for non-standard directories
    local glob_pattern = "**/" .. relative_path
    local matches = vim.fn.glob(project_root .. "/" .. glob_pattern, false, true)

    if matches and #matches > 0 then
        return matches[1]
    end

    return nil
end

-- Find the line number of a field or method (getter/setter/builder) in a Java class
-- @param bufnr buffer number (0 for current buffer)
-- @param class_name the name of the class (can be inner class)
-- @param field_name the field name to search for
-- @return line number (1-indexed), column (0-indexed) or nil, nil if not found
local function find_field_position(bufnr, class_name, field_name)
    bufnr = bufnr or 0
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

    local in_target_class = false
    local class_depth = 0
    local brace_depth = 0

    -- Pre-compile patterns for better performance
    local field_pattern = field_name .. "%s*[;=]"
    local getter_capitalized = field_name:sub(1, 1):upper() .. field_name:sub(2)
    local getter_pattern = "get" .. getter_capitalized .. "%s*%("
    local setter_pattern = "set" .. getter_capitalized .. "%s*%("
    local method_pattern = field_name .. "%s*%(%)"
    local builder_pattern = "Builder%s+" .. field_name .. "%s*%("
    local fluent_pattern = field_name .. "%s*%(.*%)%s*{"

    -- Pre-compile class detection patterns
    local class_patterns = {
        "class%s+" .. class_name .. "%s",
        "record%s+" .. class_name .. "%s",
        "class%s+" .. class_name .. "%(",
        "record%s+" .. class_name .. "%(",
    }

    for line_num, line in ipairs(lines) do
        -- Check if we found the target class/record (optimized)
        if not in_target_class then
            for _, pattern in ipairs(class_patterns) do
                if line:match(pattern) then
                    in_target_class = true
                    class_depth = brace_depth
                    break
                end
            end
        end

        -- Track brace depth (count all at once)
        local open_count = select(2, line:gsub("{", ""))
        local close_count = select(2, line:gsub("}", ""))
        brace_depth = brace_depth + open_count - close_count

        -- If we're in the target class, search for the field/method
        if in_target_class then
            -- Try patterns in order of likelihood (fields first, then getters, then setters)
            local col = line:find(field_pattern)
                or line:find(getter_pattern)
                or line:find(method_pattern)
                or line:find(setter_pattern)

            if col then
                return line_num, col - 1
            end

            -- Builder pattern needs special handling for column
            col = line:find(builder_pattern)
            if col then
                local name_start = line:find(field_name, col)
                return line_num, name_start - 1
            end

            -- Fluent pattern
            col = line:find(fluent_pattern)
            if col then
                return line_num, col - 1
            end

            -- Exit the target class when we close its braces
            if brace_depth <= class_depth then
                in_target_class = false
            end
        end
    end

    return nil, nil
end

-- Search for field and navigate to it
-- Extracted to avoid code duplication
local function navigate_to_field(simple_name, member_name)
    vim.schedule(function()
        local line_num, col = find_field_position(0, simple_name, member_name)
        if line_num then
            vim.api.nvim_win_set_cursor(0, { line_num, col or 0 })
            vim.cmd("normal! zz") -- Center the screen
        else
            vim.notify(
                string.format("[MapStruct] Field '%s' not found in class '%s'", member_name, simple_name),
                vim.log.levels.WARN
            )
        end
    end)
end

local function get_mapping_path_under_cursor()
    local full = util.get_token_under_cursor('"')
    local member = util.get_token_under_cursor('[%."]')
    local member_with_right = util.get_token_under_cursor_sides('[%."]', '"')

    -- Early returns for invalid cases
    if not full or full == "" or not member or member == "" or (full and full:find("=")) then
        return {}
    end

    -- Simple case: member without path
    if full == member then
        return { member = member }
    end

    -- Extract path
    local path = full:sub(1, -#member_with_right - 2)
    if path == "" then
        return { member = member }
    end

    return { path = path, member = member }
end

function M.goto_path_item_definitions()
    local current_path = get_mapping_path_under_cursor()

    if not current_path.member then
        vim.notify("[MapStruct] No path member found under cursor", vim.log.levels.WARN)
        return
    end

    -- Get the MapStruct context to determine which class to search
    local ctx = mapstruct.get_context({})

    if not ctx then
        vim.notify("[MapStruct] Not in a valid @Mapping annotation", vim.log.levels.WARN)
        return
    end

    -- Get completions to find the class FQN that contains our member
    mapstruct.get_completions({}, function(result, err)
        if err then
            vim.notify("[MapStruct] Failed to get completions: " .. err, vim.log.levels.ERROR)
            return
        end

        if not result or not result.className then
            vim.notify("[MapStruct] No class information found", vim.log.levels.WARN)
            return
        end

        -- Extract the class FQN (remove inner class notation if present)
        local class_fqn = result.className:gsub("%$.*$", "")

        -- Get simple class name for field search (might be inner class)
        local simple_name = result.simpleName or result.className:match("[^%.]+$")

        --[[ 
        -- Try to find the file directly in the project first (fast path)
        local project_file = find_project_file(class_fqn)
        if project_file then
            -- Fast path: Open project file directly
            vim.cmd.edit(project_file)
            navigate_to_field(simple_name, current_path.member)
            -- vim.notify("local search")
        else ]]
        -- Slow path: Use jdtls to load the class (JAR dependencies, external libs)
        jdtls_util.jdt_load_unique_class(class_fqn, function(class_result)
            if not class_result or not class_result.location then
                vim.notify("[MapStruct] Could not load class: " .. class_fqn, vim.log.levels.ERROR)
                return
            end

            -- Open the file
            vim.lsp.util.show_document(class_result.location, "utf-8", { focus = true })
            navigate_to_field(simple_name, current_path.member)
            -- vim.notify("jstls search")
        end)
        -- end
    end)
end

return M
