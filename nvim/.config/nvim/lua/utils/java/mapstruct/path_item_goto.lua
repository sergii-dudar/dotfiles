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

-- Get the MapStruct mapping path and member under cursor
-- Extracts the path (everything before the last dot) and member name (after the last dot)
-- from a MapStruct mapping expression like "orders.first.items.first.product.name"
-- @param bufnr buffer number (0 for current buffer)
-- @param line_num line number (1-indexed)
-- @param col column position (0-indexed)
-- @return table with {path = "...", member = "..."} or nil if not found
local function get_mapping_path_under_cursor(bufnr, line_num, col)
    bufnr = bufnr or 0
    local line = vim.api.nvim_buf_get_lines(bufnr, line_num - 1, line_num, false)[1]

    if not line then
        return nil
    end

    -- Find all quoted strings in the line (both single and double quotes)
    local quotes = { '"', "'" }

    for _, quote in ipairs(quotes) do
        local start_pos = 1
        while true do
            local quote_start = line:find(quote, start_pos, true)
            if not quote_start then
                break
            end

            local quote_end = line:find(quote, quote_start + 1, true)
            if not quote_end then
                break
            end

            -- Check if cursor is within this quoted string (convert col to 1-indexed)
            if col + 1 >= quote_start and col + 1 <= quote_end then
                -- Extract the full path from the quoted string
                local full_path = line:sub(quote_start + 1, quote_end - 1)

                -- Find the last dot to separate path and member
                local last_dot = full_path:match("^.*()%.")

                if last_dot then
                    -- There's at least one dot
                    local path = full_path:sub(1, last_dot - 1)
                    local member = full_path:sub(last_dot + 1)
                    return { path = path, member = member }
                else
                    -- No dots, it's just a member name
                    return { path = "", member = full_path }
                end
            end

            start_pos = quote_end + 1
        end
    end

    return nil
end

-- Test the function
local result = get_mapping_path_under_cursor()
if result then
    print(string.format('path = "%s", member = "%s"', result.path, result.member))
else
    print("No mapping path found under cursor")
end

vim.cmd.edit("src/test/java/com/dsm/mapstruct/integration/dto/ProductDTO.java")
local line_num, col = find_field_position(0, "ProductDTOInnerInner", "name")
if line_num then
    vim.api.nvim_win_set_cursor(0, { line_num, col or 0 })
end