local util = require("utils.common-util")
local mapstruct = require("utils.java.mapstruct")
local jdtls_util = require("utils.java.jdtls-util")

local M = {}

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

    for line_num, line in ipairs(lines) do
        -- Check if we found the target class/record
        if
            line:match("class%s+" .. class_name .. "%s")
            or line:match("record%s+" .. class_name .. "%s")
            or line:match("class%s+" .. class_name .. "%(")
            or line:match("record%s+" .. class_name .. "%(")
        then
            in_target_class = true
            class_depth = brace_depth
        end

        -- Track brace depth
        for _ in line:gmatch("{") do
            brace_depth = brace_depth + 1
        end

        -- If we're in the target class, search for the field/method
        if in_target_class then
            -- Direct field declaration: private String fieldName;
            local col = line:find(field_name .. "%s*[;=]")
            if col then
                return line_num, col - 1
            end

            -- Getter: public String getFieldName() or public String fieldName()
            local getter_pattern = "get" .. field_name:sub(1, 1):upper() .. field_name:sub(2) .. "%s*%("
            col = line:find(getter_pattern)
            if col then
                return line_num, col - 1
            end

            col = line:find(field_name .. "%s*%(%)")
            if col then
                return line_num, col - 1
            end

            -- Setter: public void setFieldName( or public Builder fieldName(
            local setter_pattern = "set" .. field_name:sub(1, 1):upper() .. field_name:sub(2) .. "%s*%("
            col = line:find(setter_pattern)
            if col then
                return line_num, col - 1
            end

            col = line:find("Builder%s+" .. field_name .. "%s*%(")
            if col then
                local name_start = line:find(field_name, col)
                return line_num, name_start - 1
            end

            col = line:find(field_name .. "%s*%(.*%)%s*{")
            if col then
                return line_num, col - 1
            end
        end

        -- Track closing braces
        for _ in line:gmatch("}") do
            brace_depth = brace_depth - 1
            -- Exit the target class when we close its braces
            if in_target_class and brace_depth <= class_depth then
                in_target_class = false
            end
        end
    end

    return nil, nil
end

local function get_mapping_path_under_cursor()
    local full = util.get_token_under_cursor('"')
    local member = util.get_token_under_cursor('[%."]')
    local member_with_right = util.get_token_under_cursor_sides('[%."]', '"')

    if full and full:find("=") then
        return {}
    end

    if full == "" or member == "" then
        return {}
    end

    if (not full or full == "") and member and #member > 0 then
        return { member = member }
    end

    if full == member then
        return { member = member }
    end

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

    -- Build the path expression to send to server
    -- If we have a path like "person.address", send it
    -- If we only have "person" (no path), send empty string
    -- local path_expression = current_path.path or ""

    -- Prepare request params based on context type
    -- local request_params
    -- if ctx.attribute_type == "target" then
    --     request_params = {
    --         sources = { { name = "$target", type = ctx.class_name } },
    --         pathExpression = path_expression,
    --         isEnum = ctx.is_enum or false,
    --     }
    -- else
    --     request_params = {
    --         sources = ctx.sources,
    --         pathExpression = path_expression,
    --         isEnum = ctx.is_enum or false,
    --     }
    -- end

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

        -- Extract the class FQN
        -- Remove inner class notation if present (e.g., "com.Foo$Inner" -> "com.Foo")
        local class_fqn = result.className:gsub("%$.*$", "")

        -- Get simple class name for field search (might be inner class)
        local simple_name = result.simpleName or result.className:match("[^%.]+$")

        -- Load the class file using jdtls
        jdtls_util.jdt_load_unique_class(class_fqn, function(class_result)
            if not class_result or not class_result.location then
                vim.notify("[MapStruct] Could not load class: " .. class_fqn, vim.log.levels.ERROR)
                return
            end

            -- Open the file
            vim.lsp.util.show_document(class_result.location, "utf-8", { focus = true })

            -- Wait briefly for the file to open, then search for the field
            -- Using schedule instead of defer_fn for faster execution in the event loop
            vim.schedule(function()
                local line_num, col = find_field_position(0, simple_name, current_path.member)
                if line_num then
                    vim.api.nvim_win_set_cursor(0, { line_num, col or 0 })
                    vim.cmd("normal! zz") -- Center the screen
                else
                    vim.notify(
                        string.format(
                            "[MapStruct] Field '%s' not found in class '%s'",
                            current_path.member,
                            simple_name
                        ),
                        vim.log.levels.WARN
                    )
                end
            end)
            -- - vim.lsp.util.show_document() is asynchronous
            -- - vim.schedule() waits for the current event loop to complete
            -- - This ensures the document is loaded before searching
            -- - But executes immediately in the next cycle (< 1ms)
        end)
    end)
end

return M
