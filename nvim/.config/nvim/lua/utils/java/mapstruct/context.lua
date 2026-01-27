-- Context Parser for MapStruct Completion using Treesitter
-- Extracts class name and path expression from the current buffer

local log = require("utils.logging-util").new({ name = "MapStruct.Context", filename = "mapstruct-source.log" })

local M = {}

-- Cache for type source paths: typeName -> sourcePath
local type_source_cache = {}

-- Set log level for this module
function M.set_log_level(level)
    log.set_level(level)
end

-- Clear the type source cache (useful for testing or when classpath changes)
function M.clear_type_source_cache()
    type_source_cache = {}
    log.info("Type source cache cleared")
end

-- Get class source path using explore_type_source IPC
-- Returns the JAR or directory path where the class is located
local function get_class_source_path(type_name)
    -- Handle nil or empty type names
    if not type_name or type_name == "" then
        return nil
    end

    -- Handle primitives - they don't have source files
    if
        type_name:match("^void$")
        or type_name:match("^int$")
        or type_name:match("^boolean$")
        or type_name:match("^byte$")
        or type_name:match("^short$")
        or type_name:match("^long$")
        or type_name:match("^float$")
        or type_name:match("^double$")
        or type_name:match("^char$")
    then
        log.debug("Skipping primitive type:", type_name)
        return nil
    end

    -- Strip array brackets (e.g., String[] -> String)
    local base_type = type_name:gsub("%[%]$", "")

    -- Strip generics (e.g., List<String> -> List)
    base_type = base_type:gsub("<.*>", "")

    -- Check cache first
    if type_source_cache[base_type] then
        log.debug("Cache hit for type:", base_type, "->", type_source_cache[base_type])
        return type_source_cache[base_type]
    end

    log.debug("Cache miss for type:", base_type, "- fetching from server...")

    -- Get the mapstruct module to make IPC request
    -- Use pcall to avoid circular dependency issues
    local ok, mapstruct = pcall(require, "utils.java.mapstruct")
    if not ok then
        log.error("Failed to load mapstruct module:", mapstruct)
        return nil
    end

    -- Make synchronous request (using vim.wait)
    local result = nil
    local completed = false

    mapstruct.explore_type_source({ typeName = base_type }, function(res, err)
        if err then
            log.warn("Failed to get source path for type:", base_type, "error:", err)
        elseif res and res.sourcePath then
            log.info("Got source path for type:", base_type, "->", res.sourcePath)
            type_source_cache[base_type] = res.sourcePath
            result = res.sourcePath
        else
            log.warn("No source path returned for type:", base_type)
        end
        completed = true
    end)

    -- Wait for completion (max 5 seconds)
    local wait_result = vim.wait(5000, function()
        return completed
    end, 50)

    if not wait_result then
        log.error("Timeout waiting for source path for type:", base_type)
        return nil
    end

    return result
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
-- Also extracts return type from the same javap call for efficiency
-- Strategy:
--   1. Parameter types: from javap (fully qualified class names)
--   2. Parameter names: from Treesitter (source code) - javap doesn't reliably return them
--   3. @MappingTarget: from javap -v (RuntimeVisibleParameterAnnotations or RuntimeInvisibleParameterAnnotations)
--   4. Return type: from javap method signature (first match)
-- Returns: {parameters=array of {name=string, type=string, is_mapping_target=boolean}, return_type=string}
local function get_all_method_parameters(bufnr, method_name, method_node)
    -- Get mapper class info
    local package_name, class_name = get_mapper_class_info(bufnr)
    if not package_name or not class_name then
        log.debug("Could not determine mapper class")
        return nil
    end

    local fqcn = package_name .. "." .. class_name
    log.debug("Mapper FQCN:", fqcn)

    -- Get class source path using optimized IPC call
    local startSource = vim.fn.reltime()
    local classpath = get_class_source_path(fqcn)
    local elapsedSource = vim.fn.reltimefloat(vim.fn.reltime(startSource))
    log.debug(string.format("GetSourcePaht context Took %.6f s" .. classpath, elapsedSource))

    if not classpath then
        log.error("Could not get class source path for:", fqcn)
        vim.notify("[MapStruct Context] Could not get class source path", vim.log.levels.ERROR)
        return nil
    end

    log.debug("Using optimized classpath:", classpath)

    -- Run javap with verbose flag to get parameter types and annotations
    local cmd = string.format("javap -v -cp '%s' '%s'", classpath, fqcn)
    log.debug("Running: javap -v -cp <classpath>", fqcn)

    local startJavap = vim.fn.reltime()
    local handle = io.popen(cmd)
    if not handle then
        log.error("Failed to run javap")
        vim.notify("[MapStruct Context] Failed to run javap", vim.log.levels.ERROR)
        return nil
    end

    local output = handle:read("*a")
    handle:close()
    local elapsedJavap = vim.fn.reltimefloat(vim.fn.reltime(startJavap))
    vim.notify(string.format("Javap execution Took %.6f s", elapsedJavap), vim.log.levels.WARN)
    log.debug(string.format("Javap execution Took %.6f s", elapsedJavap))

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
    local startParsing = vim.fn.reltime()
    local method_found = false
    local param_types = {}
    local param_names = {}
    local param_annotations = {} -- param_index -> {has_mapping_target=bool}
    local return_type = nil -- Extract return type from the same pass

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

                -- Extract return type from signature: public abstract ReturnType methodName(...)
                -- Match pattern: <modifiers> <return_type> <method_name>(
                local ret_type_match = line:match("^%s*%w+%s+%w*%s*([%w%.%$<>,]+)%s+" .. method_name .. "%s*%(")
                if ret_type_match then
                    return_type = ret_type_match
                    log.info("Extracted return type:", return_type)
                end

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
                    log.warn(
                        "No RuntimeParameterAnnotations found within 300 lines (might not have parameter annotations)"
                    )
                end
                break
            end

            -- Get indentation of current line
            local line_indent = line:match("^(%s*)")
            local line_indent_len = #line_indent

            -- Stop if we hit another method signature at the SAME indentation level
            if line_indent_len == method_indent_len then
                local trimmed = line:match("^%s*(.*)$")
                if
                    trimmed
                    and (
                        trimmed:match("^public%s+.*%s+%w+%s*%(")
                        or trimmed:match("^private%s+.*%s+%w+%s*%(")
                        or trimmed:match("^protected%s+.*%s+%w+%s*%(")
                    )
                then
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

    local elapsedParsing = vim.fn.reltimefloat(vim.fn.reltime(startParsing))
    log.debug(string.format("Javap output parsing Took %.6f s", elapsedParsing))

    log.info("Parsed " .. #param_types .. " parameter types from javap")
    local startLogging = vim.fn.reltime()
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
        local is_mapping_target = param_annotations[param_index] and param_annotations[param_index].has_mapping_target
            or false

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

    local elapsedLogging = vim.fn.reltimefloat(vim.fn.reltime(startLogging))
    log.debug(string.format("Parameter logging Took %.6f s", elapsedLogging))

    log.info("Total parameters extracted: " .. #parameters)
    return {
        parameters = parameters,
        return_type = return_type,
    }
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
        local start = vim.fn.reltime()
        local result = get_all_method_parameters(bufnr, method_name, method_node)
        local elapsed = vim.fn.reltimefloat(vim.fn.reltime(start))
        log.debug(string.format("Source context Took %.6f s", elapsed))

        if not result or not result.parameters or #result.parameters == 0 then
            log.debug("Could not extract method parameters")
            return nil
        end

        -- Filter out @MappingTarget parameters (they are targets, not sources)
        local sources = {}
        for _, param in ipairs(result.parameters) do
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
        -- 2. Otherwise, use the return type (extracted from the same javap call)

        -- Get all parameters to check for @MappingTarget and also get return type

        local start = vim.fn.reltime()
        local result = get_all_method_parameters(bufnr, method_name, method_node)
        local elapsed = vim.fn.reltimefloat(vim.fn.reltime(start))
        log.debug(string.format("Target context Took %.6f s", elapsed))

        local target_type = nil

        if result and result.parameters and #result.parameters > 0 then
            -- Look for @MappingTarget parameter
            for _, param in ipairs(result.parameters) do
                if param.is_mapping_target then
                    target_type = param.type
                    log.info("Found @MappingTarget parameter, using type:", target_type)
                    break
                end
            end
        end

        -- If no @MappingTarget found, use return type from the same javap call
        if not target_type then
            if result and result.return_type then
                target_type = result.return_type
                log.info("Using return type from javap:", target_type)
            else
                log.debug("Could not resolve target class from javap")
                return nil
            end
        end

        -- Check if target type is void (invalid)
        if target_type == "void" then
            log.error("Target type is void, cannot navigate fields")
            vim.notify(
                "[MapStruct Context] Method has void return type and no @MappingTarget parameter",
                vim.log.levels.ERROR
            )
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
