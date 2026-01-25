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

    -- TODO: fix this call:
    -- mapstruct.get_completions(params, callback)
    -- 1. we should send in request current_path.path
    -- or empty if not present to get fqn of java class that contains our member (filed, getter, setter, builder setter)
    -- 2. after we will get class full qualifier name, in case fqn have inner class in path, remove it (just remove all after first $ inclusive, we need just root class fqn)
    -- 3. call put to jdtls_util.jdt_load_unique_class our result class_fqn
    -- 4. make nvim command to testing

    jdtls_util.jdt_load_unique_class(class_fqn, function(result)
        -- Open the file and jump to the position
        vim.lsp.util.show_document(result.location, "utf-8", { focus = true })

        -- Open the file (JDTLS handles the jdt:// URI automatically)

        -- vim.cmd.edit("src/test/java/com/dsm/mapstruct/integration/dto/ProductDTO.java")
        local line_num, col = find_field_position(0, "ProductDTOInnerInner", "name")
        if line_num then
            vim.defer_fn(function()
                vim.api.nvim_win_set_cursor(0, { line_num, col or 0 })
                vim.cmd("normal! zz") -- Center the screen
            end, 10)
        end
    end)
end

return M
