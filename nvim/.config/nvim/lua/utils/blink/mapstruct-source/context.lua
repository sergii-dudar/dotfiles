-- Context Parser for MapStruct Completion using Treesitter
-- Extracts class name and path expression from the current buffer

local M = {}

-- Get the Treesitter node at cursor position
local function get_node_at_cursor(bufnr, row, col)
    local parser = vim.treesitter.get_parser(bufnr, "java")
    if not parser then
        return nil
    end

    local tree = parser:parse()[1]
    if not tree then
        return nil
    end

    local root = tree:root()
    return root:descendant_for_range(row, col, row, col)
end

-- Find parent node of specific type
local function find_parent_node(node, node_type)
    if not node then
        return nil
    end

    local current = node
    while current do
        if current:type() == node_type then
            return current
        end
        current = current:parent()
    end

    return nil
end

-- Get text from a node
local function get_node_text(node, bufnr)
    if not node then
        return nil
    end
    return vim.treesitter.get_node_text(node, bufnr)
end

-- Extract the path expression from a string literal node
local function extract_path_from_string(node, bufnr, cursor_col)
    if not node or node:type() ~= "string_literal" then
        return nil
    end

    local text = get_node_text(node, bufnr)
    if not text then
        return nil
    end

    -- Remove quotes
    text = text:gsub('^"', ""):gsub('"$', "")

    -- Get cursor position relative to the string
    local start_row, start_col = node:start()
    local relative_col = cursor_col - start_col - 1 -- -1 for opening quote

    -- Extract the path up to cursor position
    local path_before_cursor = text:sub(1, relative_col)

    -- If cursor is after a dot, return the full path including the dot
    if path_before_cursor:match("%.$") then
        return path_before_cursor
    end

    -- Otherwise, remove the partial field being typed after the last dot
    local last_dot_pos = path_before_cursor:match("^(.*)%.")
    if last_dot_pos then
        return last_dot_pos .. "."
    end

    -- If no dot, we're at the beginning
    return ""
end

-- Check if node is a descendant of parent
local function is_descendant_of(node, parent)
    if not node or not parent then
        return false
    end

    local current = node
    while current do
        if current:id() == parent:id() then
            return true
        end
        current = current:parent()
    end

    return false
end

-- Find the annotation element for source or target
-- Returns the value node only if the given node is actually inside that value
local function find_annotation_element(node, element_name)
    -- Navigate up to annotation node
    local annotation = find_parent_node(node, "annotation")
    if not annotation then
        return nil, nil
    end

    -- Check if it's a @Mapping or @ValueMapping annotation
    local name_node = annotation:field("name")[1]
    if not name_node then
        return nil, nil
    end

    local annotation_name = get_node_text(name_node, vim.api.nvim_get_current_buf())
    if not annotation_name or not annotation_name:match("Mapping") then
        return nil, nil
    end

    -- Determine annotation type (Mapping or ValueMapping)
    local is_value_mapping = annotation_name:match("ValueMapping") ~= nil

    -- Find the arguments node
    local arguments = annotation:field("arguments")[1]
    if not arguments then
        return nil, nil
    end

    -- Iterate through annotation elements to find source/target
    for child in arguments:iter_children() do
        if child:type() == "element_value_pair" then
            local key_node = child:field("key")[1]
            if key_node then
                local key = get_node_text(key_node, vim.api.nvim_get_current_buf())

                -- Check if this is the element we're looking for AND the node is inside it
                if key == element_name then
                    local value_node = child:field("value")[1]
                    -- Only return if the node is actually a descendant of this value
                    if value_node and is_descendant_of(node, value_node) then
                        return value_node, annotation, is_value_mapping
                    end
                end
            end
        end
    end

    return nil, annotation
end

-- Find the method declaration containing the annotation
local function find_method_declaration(annotation_node)
    if not annotation_node then
        return nil
    end

    -- Navigate up to find method_declaration
    local method = find_parent_node(annotation_node, "method_declaration")
    if not method then
        return nil
    end

    return method
end

-- Get the mapper class name from the file
local function get_mapper_class_info(bufnr)
    local parser = vim.treesitter.get_parser(bufnr, "java")
    if not parser then
        return nil, nil
    end

    local tree = parser:parse()[1]
    if not tree then
        return nil, nil
    end

    local root = tree:root()

    -- Find package declaration
    local package_name = nil
    local query_str = [[
        (package_declaration
            (scoped_identifier) @package)
    ]]
    local ok, query = pcall(vim.treesitter.query.parse, "java", query_str)
    if ok then
        for id, node in query:iter_captures(root, bufnr, 0, -1) do
            if query.captures[id] == "package" then
                package_name = get_node_text(node, bufnr)
                break
            end
        end
    end

    -- Find interface/class declaration
    local class_name = nil
    query_str = [[
        (interface_declaration
            name: (identifier) @name)
        (class_declaration
            name: (identifier) @name)
    ]]
    ok, query = pcall(vim.treesitter.query.parse, "java", query_str)
    if ok then
        for id, node in query:iter_captures(root, bufnr, 0, -1) do
            if query.captures[id] == "name" then
                class_name = get_node_text(node, bufnr)
                break
            end
        end
    end

    return package_name, class_name
end

-- Find project root directory
local function find_project_root(bufnr)
    local file_path = vim.api.nvim_buf_get_name(bufnr)
    local dir = vim.fn.fnamemodify(file_path, ":p:h")

    -- Look for pom.xml or build.gradle
    while dir ~= "/" and dir ~= "." do
        if vim.fn.filereadable(dir .. "/pom.xml") == 1 or
           vim.fn.filereadable(dir .. "/build.gradle") == 1 or
           vim.fn.filereadable(dir .. "/build.gradle.kts") == 1 then
            return dir
        end
        dir = vim.fn.fnamemodify(dir, ":h")
    end

    return nil
end

-- Get classpath from project structure (fallback)
local function get_classpath_from_project(bufnr)
    local project_root = find_project_root(bufnr)
    if not project_root then
        vim.notify("[MapStruct Context] Could not find project root", vim.log.levels.DEBUG)
        return nil
    end

    vim.notify("[MapStruct Context] Project root: " .. project_root, vim.log.levels.DEBUG)

    -- Build classpath from standard Maven/Gradle locations
    local classpaths = {}

    -- Maven structure
    local maven_paths = {
        project_root .. "/target/classes",
        project_root .. "/target/test-classes",
    }

    -- Gradle structure
    local gradle_paths = {
        project_root .. "/build/classes/java/main",
        project_root .. "/build/classes/java/test",
    }

    -- Check which build tool
    local use_maven = vim.fn.filereadable(project_root .. "/pom.xml") == 1
    local paths_to_check = use_maven and maven_paths or gradle_paths

    for _, path in ipairs(paths_to_check) do
        if vim.fn.isdirectory(path) == 1 then
            table.insert(classpaths, path)
            vim.notify("[MapStruct Context] Found classpath: " .. path, vim.log.levels.DEBUG)
        end
    end

    if #classpaths > 0 then
        return table.concat(classpaths, ":")
    end

    return nil
end

-- Get classpath from jdtls with fallback
local function get_jdtls_classpath(bufnr)
    -- Try jdtls first
    local clients = vim.lsp.get_clients({ name = "jdtls" })
    if clients and #clients > 0 then
        local client = clients[1]
        local uri = vim.uri_from_bufnr(bufnr)

        vim.notify("[MapStruct Context] Querying jdtls for classpath...", vim.log.levels.DEBUG)

        -- Try to get both runtime and test classpaths
        local all_classpaths = {}

        -- Query with 'test' scope to get test dependencies
        local result_test, err = client.request_sync(
            "workspace/executeCommand",
            {
                command = "java.project.getClasspaths",
                arguments = { uri, { scope = "test" } }
            },
            5000,
            bufnr
        )

        if result_test and result_test.result then
            local classpaths = result_test.result.classpaths or result_test.result
            if type(classpaths) == "table" and #classpaths > 0 then
                vim.notify("[MapStruct Context] Got " .. #classpaths .. " test classpath entries from jdtls", vim.log.levels.INFO)
                for _, cp in ipairs(classpaths) do
                    table.insert(all_classpaths, cp)
                end
            end
        else
            -- Try runtime scope as fallback
            local result_runtime, err = client.request_sync(
                "workspace/executeCommand",
                {
                    command = "java.project.getClasspaths",
                    arguments = { uri, { scope = "runtime" } }
                },
                5000,
                bufnr
            )

            if result_runtime and result_runtime.result then
                local classpaths = result_runtime.result.classpaths or result_runtime.result
                if type(classpaths) == "table" and #classpaths > 0 then
                    vim.notify("[MapStruct Context] Got " .. #classpaths .. " runtime classpath entries from jdtls", vim.log.levels.INFO)
                    for _, cp in ipairs(classpaths) do
                        table.insert(all_classpaths, cp)
                    end
                end
            end
        end

        if #all_classpaths > 0 then
            vim.notify("[MapStruct Context] Total jdtls classpath entries: " .. #all_classpaths, vim.log.levels.INFO)
            return table.concat(all_classpaths, ":")
        end

        if err then
            vim.notify("[MapStruct Context] jdtls request error: " .. vim.inspect(err), vim.log.levels.DEBUG)
        end
    else
        vim.notify("[MapStruct Context] No jdtls client found", vim.log.levels.DEBUG)
    end

    -- Fallback to project structure
    vim.notify("[MapStruct Context] Falling back to project structure classpath", vim.log.levels.INFO)
    return get_classpath_from_project(bufnr)
end

-- Use javap to get method signature with fully qualified class names
local function resolve_class_from_javap(bufnr, method_name, param_name)
    -- Get mapper class info
    local package_name, class_name = get_mapper_class_info(bufnr)
    if not package_name or not class_name then
        vim.notify("[MapStruct Context] Could not determine mapper class", vim.log.levels.DEBUG)
        return nil
    end

    local fqcn = package_name .. "." .. class_name
    vim.notify("[MapStruct Context] Mapper FQCN: " .. fqcn, vim.log.levels.DEBUG)

    -- Get classpath from jdtls (with fallback)
    local classpath = get_jdtls_classpath(bufnr)
    if not classpath then
        vim.notify("[MapStruct Context] Could not get classpath", vim.log.levels.ERROR)
        return nil
    end

    -- Run javap
    local cmd = string.format("javap -cp '%s' '%s'", classpath, fqcn)
    vim.notify("[MapStruct Context] Running: javap -cp <classpath> " .. fqcn, vim.log.levels.DEBUG)

    local handle = io.popen(cmd)
    if not handle then
        vim.notify("[MapStruct Context] Failed to run javap", vim.log.levels.ERROR)
        return nil
    end

    local output = handle:read("*a")
    handle:close()

    if not output or output == "" then
        vim.notify("[MapStruct Context] javap returned empty output", vim.log.levels.WARN)
        return nil
    end

    vim.notify("[MapStruct Context] javap output received", vim.log.levels.DEBUG)

    -- Parse javap output to find the method signature
    -- javap format: public abstract ReturnType methodName(FullyQualifiedClassName);
    -- Example: public abstract ComplexNestedDTO mapComplexNested(com.dsm.mapstruct.testdata.TestClasses$Person);
    for line in output:gmatch("[^\r\n]+") do
        if line:match("%s+" .. method_name .. "%s*%(") then
            vim.notify("[MapStruct Context] Found method line: " .. line, vim.log.levels.DEBUG)

            -- Extract parameter type from: methodName(Type);
            -- Pattern: captures fully qualified class name inside parentheses
            local param_type = line:match(method_name .. "%s*%(([%w%.%$<>,]+)%s*%)")

            if param_type then
                -- Remove parameter name if present (e.g., "Type paramName" -> "Type")
                -- javap might include or exclude parameter names depending on debug info
                param_type = param_type:match("^([%w%.%$<>,]+)") or param_type

                vim.notify("[MapStruct Context] ✓ Extracted parameter type: " .. param_type, vim.log.levels.INFO)
                return param_type
            end
        end
    end

    vim.notify("[MapStruct Context] Could not find method signature in javap output", vim.log.levels.WARN)
    return nil
end

-- Extract parameter type from method declaration using javap
-- For MapStruct: TargetDTO map(SourceDTO source)
-- We want to extract the fully qualified SourceDTO
local function get_source_class_from_method(method_node, bufnr)
    if not method_node then
        return nil
    end

    -- Get method name
    local method_name_node = method_node:field("name")[1]
    if not method_name_node then
        return nil
    end
    local method_name = get_node_text(method_name_node, bufnr)

    -- Get formal parameters to extract parameter name
    local params = method_node:field("parameters")[1]
    if not params then
        return nil
    end

    -- Get the first parameter name
    local param_name = nil
    for child in params:iter_children() do
        if child:type() == "formal_parameter" then
            local name_node = child:field("name")[1]
            if name_node then
                param_name = get_node_text(name_node, bufnr)
                break
            end
        end
    end

    if not param_name then
        vim.notify("[MapStruct Context] Could not extract parameter name", vim.log.levels.DEBUG)
        return nil
    end

    vim.notify("[MapStruct Context] Method: " .. method_name .. ", Param: " .. param_name, vim.log.levels.DEBUG)

    -- Use javap to resolve the fully qualified class name
    return resolve_class_from_javap(bufnr, method_name, param_name)
end

-- Extract return type from method declaration using javap
-- For MapStruct: TargetDTO map(SourceDTO source)
-- We want to extract the fully qualified TargetDTO (return type)
local function get_target_class_from_method(method_node, bufnr)
    if not method_node then
        return nil
    end

    -- Get method name
    local method_name_node = method_node:field("name")[1]
    if not method_name_node then
        return nil
    end
    local method_name = get_node_text(method_name_node, bufnr)

    vim.notify("[MapStruct Context] Getting return type for method: " .. method_name, vim.log.levels.DEBUG)

    -- Get mapper class info
    local package_name, class_name = get_mapper_class_info(bufnr)
    if not package_name or not class_name then
        vim.notify("[MapStruct Context] Could not determine mapper class", vim.log.levels.DEBUG)
        return nil
    end

    local fqcn = package_name .. "." .. class_name
    vim.notify("[MapStruct Context] Mapper FQCN: " .. fqcn, vim.log.levels.DEBUG)

    -- Get classpath from jdtls (with fallback)
    local classpath = get_jdtls_classpath(bufnr)
    if not classpath then
        vim.notify("[MapStruct Context] Could not get classpath", vim.log.levels.ERROR)
        return nil
    end

    -- Run javap
    local cmd = string.format("javap -cp '%s' '%s'", classpath, fqcn)
    vim.notify("[MapStruct Context] Running: javap -cp <classpath> " .. fqcn, vim.log.levels.DEBUG)

    local handle = io.popen(cmd)
    if not handle then
        vim.notify("[MapStruct Context] Failed to run javap", vim.log.levels.ERROR)
        return nil
    end

    local output = handle:read("*a")
    handle:close()

    if not output or output == "" then
        vim.notify("[MapStruct Context] javap returned empty output", vim.log.levels.WARN)
        return nil
    end

    vim.notify("[MapStruct Context] javap output received", vim.log.levels.DEBUG)

    -- Parse javap output to find the method and extract return type
    -- javap format: public abstract ReturnType methodName(ParamType);
    -- Example: public abstract com.example.OrderComplexDTO mapOrderComplex(com.example.Order);
    for line in output:gmatch("[^\r\n]+") do
        if line:match("%s+" .. method_name .. "%s*%(") then
            vim.notify("[MapStruct Context] Found method line: " .. line, vim.log.levels.DEBUG)

            -- Extract return type: capture the type name immediately before the method name
            -- Split by spaces and find the type before method name
            local parts = {}
            for part in line:gmatch("%S+") do
                table.insert(parts, part)
            end

            -- Find method name position
            local method_pos = nil
            for i, part in ipairs(parts) do
                if part:match("^" .. method_name .. "%(") or part == method_name then
                    method_pos = i
                    break
                end
            end

            -- Return type is the part before the method name
            if method_pos and method_pos > 1 then
                local return_type = parts[method_pos - 1]
                -- Remove any trailing semicolons or parentheses
                return_type = return_type:gsub("[;%(].*$", "")

                vim.notify("[MapStruct Context] ✓ Extracted return type: " .. return_type, vim.log.levels.INFO)
                return return_type
            end
        end
    end

    vim.notify("[MapStruct Context] Could not find return type in javap output", vim.log.levels.WARN)
    return nil
end

-- Extract completion context from the current cursor position using Treesitter
function M.get_completion_context(bufnr, row, col)
    bufnr = bufnr or vim.api.nvim_get_current_buf()

    -- Check if we're in a Java file
    local filetype = vim.bo[bufnr].filetype
    if filetype ~= "java" then
        vim.notify("[MapStruct Context] Not a Java file: " .. filetype, vim.log.levels.DEBUG)
        return nil
    end

    -- Get the node at cursor
    local node = get_node_at_cursor(bufnr, row, col)
    if not node then
        vim.notify("[MapStruct Context] No node at cursor", vim.log.levels.DEBUG)
        return nil
    end

    vim.notify("[MapStruct Context] Node type at cursor: " .. node:type(), vim.log.levels.DEBUG)

    -- Check if we're in a string literal (source or target value)
    local string_node = node
    if node:type() ~= "string_literal" then
        string_node = find_parent_node(node, "string_literal")
    end

    if not string_node then
        vim.notify("[MapStruct Context] Not in string literal", vim.log.levels.DEBUG)
        return nil
    end

    -- Find if this string is part of source or target attribute
    local value_node, annotation_node, is_value_mapping = find_annotation_element(string_node, "source")
    local attribute_type = "source"

    if not value_node then
        value_node, annotation_node, is_value_mapping = find_annotation_element(string_node, "target")
        attribute_type = "target"
    end

    if not value_node or not annotation_node then
        vim.notify("[MapStruct Context] Not in @Mapping/@ValueMapping source/target", vim.log.levels.DEBUG)
        return nil
    end

    local mapping_type = is_value_mapping and "ValueMapping" or "Mapping"
    vim.notify("[MapStruct Context] Detected annotation: @" .. mapping_type .. ", attribute: " .. attribute_type, vim.log.levels.INFO)

    -- Extract the path expression being typed
    local path_expr = extract_path_from_string(string_node, bufnr, col)
    if path_expr == nil then
        vim.notify("[MapStruct Context] Could not extract path", vim.log.levels.DEBUG)
        return nil
    end

    -- For ValueMapping, only empty path is valid (enum constants, no nested paths)
    if is_value_mapping and path_expr ~= "" then
        vim.notify("[MapStruct Context] ValueMapping does not support nested paths", vim.log.levels.DEBUG)
        return nil
    end

    vim.notify("[MapStruct Context] Path expression: '" .. path_expr .. "'", vim.log.levels.DEBUG)

    -- Find the method declaration
    local method_node = find_method_declaration(annotation_node)
    if not method_node then
        vim.notify("[MapStruct Context] No method declaration found", vim.log.levels.DEBUG)
        return nil
    end

    -- Get the appropriate class based on attribute type
    local resolved_class = nil
    if attribute_type == "source" then
        -- For source attribute, extract parameter type
        resolved_class = get_source_class_from_method(method_node, bufnr)
        if not resolved_class then
            vim.notify("[MapStruct Context] Could not resolve source class", vim.log.levels.DEBUG)
            return nil
        end
    elseif attribute_type == "target" then
        -- For target attribute, extract return type
        resolved_class = get_target_class_from_method(method_node, bufnr)
        if not resolved_class then
            vim.notify("[MapStruct Context] Could not resolve target class", vim.log.levels.DEBUG)
            return nil
        end
    else
        vim.notify("[MapStruct Context] Unknown attribute type: " .. attribute_type, vim.log.levels.ERROR)
        return nil
    end

    vim.notify("[MapStruct Context] Resolved " .. attribute_type .. " class: " .. resolved_class, vim.log.levels.INFO)

    return {
        class_name = resolved_class,
        path_expression = path_expr,
        attribute_type = attribute_type, -- "source" or "target"
        is_enum = is_value_mapping, -- true for @ValueMapping (enum constants)
        line = row,
        col = col,
    }
end

-- Get all @Mapping annotations in the current method
function M.get_method_mappings(bufnr, row)
    bufnr = bufnr or vim.api.nvim_get_current_buf()

    local node = get_node_at_cursor(bufnr, row, 0)
    if not node then
        return {}
    end

    local method_node = find_parent_node(node, "method_declaration")
    if not method_node then
        return {}
    end

    local mappings = {}

    -- Find all annotations on this method
    local modifiers = method_node:field("modifiers")
    if not modifiers then
        return {}
    end

    for _, modifier_node in ipairs(modifiers) do
        if modifier_node:type() == "modifiers" then
            for child in modifier_node:iter_children() do
                if child:type() == "annotation" or child:type() == "marker_annotation" then
                    local name_node = child:field("name")[1]
                    if name_node then
                        local annotation_name = get_node_text(name_node, bufnr)
                        if annotation_name and annotation_name:match("Mapping") then
                            -- Extract source and target from this annotation
                            local mapping = { annotation_name = annotation_name }

                            local arguments = child:field("arguments")[1]
                            if arguments then
                                for arg_child in arguments:iter_children() do
                                    if arg_child:type() == "element_value_pair" then
                                        local key_node = arg_child:field("key")[1]
                                        local value_node = arg_child:field("value")[1]

                                        if key_node and value_node then
                                            local key = get_node_text(key_node, bufnr)
                                            local value = get_node_text(value_node, bufnr)

                                            if key == "source" then
                                                mapping.source = value:gsub('^"', ""):gsub('"$', "")
                                            elseif key == "target" then
                                                mapping.target = value:gsub('^"', ""):gsub('"$', "")
                                            end
                                        end
                                    end
                                end
                            end

                            table.insert(mappings, mapping)
                        end
                    end
                end
            end
        end
    end

    return mappings
end

return M
