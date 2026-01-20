-- Context Parser for MapStruct Completion using Treesitter
-- Extracts class name and path expression from the current buffer

local classpath_util = require("utils.blink.mapstruct-source.classpath-util")
local log = require("utils.logging-util").new({ name = "MapStruct.Context", filename = "mapstruct-source.log" })

local M = {}

-- Set log level for this module
function M.set_log_level(level)
    log.set_level(level)
end

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


-- Extract parameter names from source code using Treesitter
-- Returns: array of parameter names in order
local function get_parameter_names_from_source(method_node, bufnr)
    if not method_node then
        return {}
    end

    local params = method_node:field("parameters")[1]
    if not params then
        return {}
    end

    local param_names = {}
    for child in params:iter_children() do
        if child:type() == "formal_parameter" then
            local name_node = child:field("name")[1]
            if name_node then
                local param_name = get_node_text(name_node, bufnr)
                table.insert(param_names, param_name)
            end
        end
    end

    return param_names
end

-- Get all method parameters with types and detect @MappingTarget annotations
-- Strategy:
--   1. Parameter types: from javap (fully qualified class names)
--   2. Parameter names: from Treesitter (source code) - javap doesn't reliably return them
--   3. @MappingTarget: from javap -v (RuntimeVisibleParameterAnnotations or RuntimeInvisibleParameterAnnotations)
-- Returns: array of {name=string, type=string, is_mapping_target=boolean}
local function get_all_method_parameters(bufnr, method_name, method_node)
    -- Get mapper class info
    local package_name, class_name = get_mapper_class_info(bufnr)
    if not package_name or not class_name then
        log.debug("Could not determine mapper class")
        return nil
    end

    local fqcn = package_name .. "." .. class_name
    log.debug("Mapper FQCN:", fqcn)

    -- Get classpath from jdtls
    local classpath = classpath_util.get_classpath({ bufnr = bufnr })
    if not classpath then
        log.error("Could not get classpath")
        vim.notify("[MapStruct Context] Could not get classpath", vim.log.levels.ERROR)
        return nil
    end

    -- Run javap with verbose flag to get parameter types and annotations
    local cmd = string.format("javap -v -cp '%s' '%s'", classpath, fqcn)
    log.debug("Running: javap -v -cp <classpath>", fqcn)

    local handle = io.popen(cmd)
    if not handle then
        log.error("Failed to run javap")
        vim.notify("[MapStruct Context] Failed to run javap", vim.log.levels.ERROR)
        return nil
    end

    local output = handle:read("*a")
    handle:close()

    if not output or output == "" then
        log.warn("javap returned empty output")
        vim.notify("[MapStruct Context] javap returned empty output", vim.log.levels.WARN)
        return nil
    end

    log.debug("javap verbose output received")

    -- First, get expected parameter count from source code to match correct overload
    local source_names = get_parameter_names_from_source(method_node, bufnr)
    local expected_param_count = #source_names
    log.info("Expected parameter count from source: " .. expected_param_count)

    -- Parse javap output to find method signature and parameter annotations
    -- Single-pass state machine for efficient parsing
    local method_found = false
    local param_types = {}
    local param_names = {}
    local param_annotations = {} -- param_index -> {has_mapping_target=bool}

    -- State machine states
    local state = "seeking_method"
    local method_indent_len = nil
    local lines_since_method = 0
    local in_annotations = false
    local current_param_index = nil

    -- Single pass through output
    for line in output:gmatch("[^\r\n]+") do
        if state == "seeking_method" then
            if line:match("%s+" .. method_name .. "%s*%(") then
                log.info("Found method signature candidate:", line)

                -- Extract parameters from signature: methodName(Type1,Type2,Type3);
                local params_str = line:match(method_name .. "%s*%(([^%)]*)")
                log.info("Extracted params string:", params_str or "nil")

                if params_str and params_str ~= "" then
                    -- Split by comma to get individual parameter types
                    -- Handle generic types with commas inside <>
                    local candidate_param_types = {}
                    local depth = 0
                    local current_param = ""

                    for i = 1, #params_str do
                        local char = params_str:sub(i, i)
                        if char == "<" then
                            depth = depth + 1
                            current_param = current_param .. char
                        elseif char == ">" then
                            depth = depth - 1
                            current_param = current_param .. char
                        elseif char == "," and depth == 0 then
                            -- End of parameter
                            local param_type = current_param:match("^%s*(.-)%s*$") -- trim
                            local type_only = param_type:match("^([%w%.%$<>,]+)")
                            if type_only then
                                table.insert(candidate_param_types, type_only)
                            end
                            current_param = ""
                        else
                            current_param = current_param .. char
                        end
                    end

                    -- Don't forget the last parameter
                    if current_param ~= "" then
                        local param_type = current_param:match("^%s*(.-)%s*$")
                        local type_only = param_type:match("^([%w%.%$<>,]+)")
                        if type_only then
                            table.insert(candidate_param_types, type_only)
                        end
                    end

                    -- Check if parameter count matches expected count
                    log.info("Candidate has " .. #candidate_param_types .. " parameters")
                    if #candidate_param_types == expected_param_count then
                        -- This is the right method!
                        param_types = candidate_param_types
                        method_found = true
                        log.info("Parameter count matches! Using this method.")

                        -- Get method indentation for annotation search
                        local method_indent = line:match("^(%s*)")
                        method_indent_len = #method_indent
                        log.info("Method indentation level: " .. method_indent_len .. " spaces")
                        log.info("Searching for @MappingTarget annotations for this method...")

                        -- Transition to seeking annotations
                        state = "seeking_annotations"
                    else
                        log.info("Parameter count mismatch, continuing search...")
                    end
                elseif params_str == "" and expected_param_count == 0 then
                    -- No parameters expected and none found
                    method_found = true
                    break
                end
            end

        elseif state == "seeking_annotations" then
            lines_since_method = lines_since_method + 1

            -- Stop after 300 lines
            if lines_since_method > 300 then
                if not in_annotations then
                    log.warn("No RuntimeParameterAnnotations found within 300 lines (might not have parameter annotations)")
                end
                break
            end

            -- Get indentation of current line
            local line_indent = line:match("^(%s*)")
            local line_indent_len = #line_indent

            -- Stop if we hit another method signature at the SAME indentation level
            if line_indent_len == method_indent_len then
                local trimmed = line:match("^%s*(.*)$")
                if trimmed and (
                    trimmed:match("^public%s+.*%s+%w+%s*%(") or
                    trimmed:match("^private%s+.*%s+%w+%s*%(") or
                    trimmed:match("^protected%s+.*%s+%w+%s*%(")
                ) then
                    log.info("Reached next method signature at same indentation, stopping annotation search")
                    break
                end
            end

            -- Check for RuntimeParameterAnnotations
            if line:match("Runtime.*ParameterAnnotations:") then
                in_annotations = true
                log.info("Found RuntimeParameterAnnotations for this method")
            elseif in_annotations then
                local param_idx = line:match("^%s+parameter%s+(%d+):")
                if param_idx then
                    current_param_index = tonumber(param_idx)
                    param_annotations[current_param_index] = { has_mapping_target = false }
                    log.info("Found parameter " .. param_idx .. " in annotations")
                elseif current_param_index and line:match("MappingTarget") then
                    param_annotations[current_param_index].has_mapping_target = true
                    log.info(">>> Found @MappingTarget on parameter " .. current_param_index)
                end
            end
        end
    end

    log.info("Parsed " .. #param_types .. " parameter types from javap")
    for i, ptype in ipairs(param_types) do
        log.info(string.format("  Type %d: %s", i, ptype))
    end

    if not method_found or #param_types == 0 then
        log.warn("Could not find matching method overload")
        return {}
    end

    -- Step 2: Use parameter names from source code (already extracted)
    -- We should have matching counts since we matched the method by count
    log.info("Using " .. #source_names .. " parameter names from source")
    for i, pname in ipairs(source_names) do
        log.info(string.format("  Name %d: %s", i, pname))
    end

    if #source_names == #param_types then
        param_names = source_names
        log.info("Parameter names matched!")
    else
        -- This shouldn't happen since we matched by count, but just in case...
        log.error(
            string.format(
                "UNEXPECTED: Parameter count mismatch after matching: types=%d, names=%d",
                #param_types,
                #source_names
            )
        )
        for i = 1, #param_types do
            table.insert(param_names, "param" .. (i - 1))
        end
    end

    -- Step 3: Build result array (annotations already parsed in step 1)
    local parameters = {}
    log.info("Building final parameter list...")
    for i = 1, #param_types do
        local param_index = i - 1 -- Java uses 0-based indexing
        local is_mapping_target = param_annotations[param_index] and param_annotations[param_index].has_mapping_target or false

        table.insert(parameters, {
            name = param_names[i],
            type = param_types[i],
            is_mapping_target = is_mapping_target,
        })

        log.info(
            string.format(
                "Parameter %d: name=%s, type=%s, @MappingTarget=%s",
                i,
                param_names[i],
                param_types[i],
                tostring(is_mapping_target)
            )
        )
    end

    log.info("Total parameters extracted: " .. #parameters)
    return parameters
end

-- Use javap to get method signature with fully qualified class names
-- DEPRECATED: Use get_all_method_parameters() instead
local function resolve_class_from_javap(bufnr, method_name, param_name)
    -- Get mapper class info
    local package_name, class_name = get_mapper_class_info(bufnr)
    if not package_name or not class_name then
        log.debug("Could not determine mapper class")
        return nil
    end

    local fqcn = package_name .. "." .. class_name
    log.debug("Mapper FQCN:", fqcn)

    -- Get classpath from jdtls (with fallback)
    local classpath = classpath_util.get_classpath({ bufnr = bufnr })
    if not classpath then
        log.error("Could not get classpath")
        vim.notify("[MapStruct Context] Could not get classpath", vim.log.levels.ERROR)
        return nil
    end

    -- Run javap
    local cmd = string.format("javap -cp '%s' '%s'", classpath, fqcn)
    log.debug("Running: javap -cp <classpath>", fqcn)

    local handle = io.popen(cmd)
    if not handle then
        log.error("Failed to run javap")
        vim.notify("[MapStruct Context] Failed to run javap", vim.log.levels.ERROR)
        return nil
    end

    local output = handle:read("*a")
    handle:close()

    if not output or output == "" then
        log.warn("javap returned empty output")
        vim.notify("[MapStruct Context] javap returned empty output", vim.log.levels.WARN)
        return nil
    end

    log.debug("javap output received")

    -- Parse javap output to find the method signature
    -- javap format: public abstract ReturnType methodName(FullyQualifiedClassName);
    -- Example: public abstract ComplexNestedDTO mapComplexNested(com.dsm.mapstruct.testdata.TestClasses$Person);
    for line in output:gmatch("[^\r\n]+") do
        if line:match("%s+" .. method_name .. "%s*%(") then
            log.debug("Found method line:", line)

            -- Extract parameter type from: methodName(Type);
            -- Pattern: captures fully qualified class name inside parentheses
            local param_type = line:match(method_name .. "%s*%(([%w%.%$<>,]+)%s*%)")

            if param_type then
                -- Remove parameter name if present (e.g., "Type paramName" -> "Type")
                -- javap might include or exclude parameter names depending on debug info
                param_type = param_type:match("^([%w%.%$<>,]+)") or param_type

                log.info("Extracted parameter type:", param_type)
                return param_type
            end
        end
    end

    log.warn("Could not find method signature in javap output")
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
        log.debug("Could not extract parameter name")
        return nil
    end

    log.debug("Method:", method_name, ", Param:", param_name)

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

    log.debug("Getting return type for method:", method_name)

    -- Get mapper class info
    local package_name, class_name = get_mapper_class_info(bufnr)
    if not package_name or not class_name then
        log.debug("Could not determine mapper class")
        return nil
    end

    local fqcn = package_name .. "." .. class_name
    log.debug("Mapper FQCN:", fqcn)

    -- Get classpath from jdtls (with fallback)
    local classpath = classpath_util.get_classpath({ bufnr = bufnr })
    if not classpath then
        log.error("Could not get classpath")
        vim.notify("[MapStruct Context] Could not get classpath", vim.log.levels.ERROR)
        return nil
    end

    -- Run javap
    local cmd = string.format("javap -cp '%s' '%s'", classpath, fqcn)
    log.debug("Running: javap -cp <classpath>", fqcn)

    local handle = io.popen(cmd)
    if not handle then
        log.error("Failed to run javap")
        vim.notify("[MapStruct Context] Failed to run javap", vim.log.levels.ERROR)
        return nil
    end

    local output = handle:read("*a")
    handle:close()

    if not output or output == "" then
        log.warn("javap returned empty output")
        vim.notify("[MapStruct Context] javap returned empty output", vim.log.levels.WARN)
        return nil
    end

    log.debug("javap output received")

    -- Parse javap output to find the method and extract return type
    -- javap format: public abstract ReturnType methodName(ParamType);
    -- Example: public abstract com.example.OrderComplexDTO mapOrderComplex(com.example.Order);
    for line in output:gmatch("[^\r\n]+") do
        if line:match("%s+" .. method_name .. "%s*%(") then
            log.debug("Found method line:", line)

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

                log.info("Extracted return type:", return_type)
                return return_type
            end
        end
    end

    log.warn("Could not find return type in javap output")
    vim.notify("[MapStruct Context] Could not find return type in javap output", vim.log.levels.WARN)
    return nil
end

-- Extract completion context from the current cursor position using Treesitter
-- Optimized with early exit checks to avoid expensive operations for invalid contexts
function M.get_completion_context(bufnr, row, col)
    bufnr = bufnr or vim.api.nvim_get_current_buf()

    -- EARLY EXIT 1: Check filetype (fastest check)
    local filetype = vim.bo[bufnr].filetype
    if filetype ~= "java" then
        log.debug("Not a Java file:", filetype)
        return nil
    end

    -- EARLY EXIT 2: Get treesitter node at cursor
    local node = get_node_at_cursor(bufnr, row, col)
    if not node then
        log.debug("No node at cursor")
        return nil
    end

    log.debug("Node type at cursor:", node:type())

    -- EARLY EXIT 3: Check if we're in a string literal (fast treesitter check)
    local string_node = node
    if node:type() ~= "string_literal" then
        string_node = find_parent_node(node, "string_literal")
    end

    if not string_node then
        log.debug("Not in string literal")
        return nil
    end

    -- EARLY EXIT 4: Check if string is inside @Mapping annotation (medium cost)
    -- Try source attribute first (most common)
    local value_node, annotation_node, is_value_mapping = find_annotation_element(string_node, "source")
    local attribute_type = "source"

    if not value_node then
        -- Try target attribute
        value_node, annotation_node, is_value_mapping = find_annotation_element(string_node, "target")
        attribute_type = "target"
    end

    if not value_node or not annotation_node then
        log.debug("Not in @Mapping/@ValueMapping source/target")
        return nil
    end

    -- At this point, we've confirmed we're in a valid MapStruct annotation context
    -- Now we can proceed with expensive operations (javap, etc.)

    local mapping_type = is_value_mapping and "ValueMapping" or "Mapping"
    log.info("Detected annotation: @" .. mapping_type .. ", attribute:", attribute_type)

    -- EARLY EXIT 5: Extract and validate path expression (before javap)
    local path_expr = extract_path_from_string(string_node, bufnr, col)
    if path_expr == nil then
        log.debug("Could not extract path")
        return nil
    end

    -- For ValueMapping, only empty path is valid (enum constants, no nested paths)
    if is_value_mapping and path_expr ~= "" then
        log.debug("ValueMapping does not support nested paths")
        return nil
    end

    log.debug("Path expression: '" .. path_expr .. "'")

    -- Find the method declaration
    local method_node = find_method_declaration(annotation_node)
    if not method_node then
        log.debug("No method declaration found")
        return nil
    end

    -- Get method name for parameter extraction
    local method_name_node = method_node:field("name")[1]
    if not method_name_node then
        log.debug("Could not get method name")
        return nil
    end
    local method_name = get_node_text(method_name_node, bufnr)

    if attribute_type == "source" then
        -- For source attribute, get ALL method parameters (excluding @MappingTarget)
        local all_params = get_all_method_parameters(bufnr, method_name, method_node)
        if not all_params or #all_params == 0 then
            log.debug("Could not extract method parameters")
            return nil
        end

        -- Filter out @MappingTarget parameters (they are targets, not sources)
        local sources = {}
        for _, param in ipairs(all_params) do
            log.debug(
                string.format(
                    "Checking parameter: name=%s, type=%s, is_mapping_target=%s",
                    param.name,
                    param.type,
                    tostring(param.is_mapping_target)
                )
            )
            if not param.is_mapping_target then
                table.insert(sources, {
                    name = param.name,
                    type = param.type,
                })
                log.info("Added source parameter:", param.name, param.type)
            else
                log.info("Skipped @MappingTarget parameter:", param.name)
            end
        end

        if #sources == 0 then
            log.warn("No source parameters found (all are @MappingTarget?)")
            return nil
        end

        log.info(string.format("Built sources array with %d parameter(s)", #sources))

        return {
            sources = sources, -- Array of {name, type}
            path_expression = path_expr,
            attribute_type = attribute_type, -- "source"
            is_enum = is_value_mapping, -- true for @ValueMapping (enum constants)
            line = row,
            col = col,
        }

    elseif attribute_type == "target" then
        -- For target attribute, we need to determine the target type:
        -- 1. If method has @MappingTarget parameter, use that parameter's type
        -- 2. Otherwise, use the return type

        -- Get all parameters to check for @MappingTarget
        local all_params = get_all_method_parameters(bufnr, method_name, method_node)
        local target_type = nil

        if all_params and #all_params > 0 then
            -- Look for @MappingTarget parameter
            for _, param in ipairs(all_params) do
                if param.is_mapping_target then
                    target_type = param.type
                    log.info("Found @MappingTarget parameter, using type:", target_type)
                    break
                end
            end
        end

        -- If no @MappingTarget found, fall back to return type
        if not target_type then
            target_type = get_target_class_from_method(method_node, bufnr)
            if not target_type then
                log.debug("Could not resolve target class")
                return nil
            end
            log.info("Using return type as target:", target_type)
        end

        -- Check if target type is void (invalid)
        if target_type == "void" then
            log.error("Target type is void, cannot navigate fields")
            vim.notify("[MapStruct Context] Method has void return type and no @MappingTarget parameter", vim.log.levels.ERROR)
            return nil
        end

        return {
            class_name = target_type, -- Direct class name for backward compatibility
            path_expression = path_expr,
            attribute_type = attribute_type, -- "target"
            is_enum = is_value_mapping, -- true for @ValueMapping (enum constants)
            line = row,
            col = col,
        }

    else
        log.error("Unknown attribute type:", attribute_type)
        vim.notify("[MapStruct Context] Unknown attribute type: " .. attribute_type, vim.log.levels.ERROR)
        return nil
    end
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