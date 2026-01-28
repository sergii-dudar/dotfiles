-- Context Parser for MapStruct Completion using Treesitter
-- Extracts class name and path expression from the current buffer

local log = require("utils.logging-util").new({ name = "MapStruct.Context", filename = "mapstruct-source.log" })

local M = {}

-- Cache TTL (time to live) in seconds - 5 minutes
local CACHE_TTL = 300

-- Cleanup interval in milliseconds - run cleanup every 60 seconds
local CLEANUP_INTERVAL_MS = 60000

-- Cache for type source paths: typeName -> {value, last_used}
local type_source_cache = {}

-- Cache for method parameters: signature_key -> {value, last_used}
-- Key format: "methodName|ReturnType|param1Type:param1Name,param2Type:param2Name"
local method_params_cache = {}

-- Cache for wildcard import resolution: typeName -> FQN
local wildcard_resolution_cache = {}

-- Timer handle for cleanup scheduler
local cleanup_timer = nil

-- Clean expired cache entries (entries not used for more than CACHE_TTL seconds)
local function cleanup_cache(cache, cache_name)
    local current_time = os.time()
    local removed_count = 0

    for key, entry in pairs(cache) do
        if entry.last_used and (current_time - entry.last_used) > CACHE_TTL then
            cache[key] = nil
            removed_count = removed_count + 1
        end
    end

    if removed_count > 0 then
        log.info(string.format("Cleaned %d expired entries from %s cache", removed_count, cache_name))
    end
end

-- Periodic cleanup for all caches
local function cleanup_all_caches()
    cleanup_cache(type_source_cache, "type_source")
    cleanup_cache(method_params_cache, "method_params")
    cleanup_cache(wildcard_resolution_cache, "wildcard_resolution")
end

-- Start automatic cache cleanup timer
local function start_cleanup_timer()
    if cleanup_timer then
        return -- Timer already running
    end

    cleanup_timer = vim.fn.timer_start(CLEANUP_INTERVAL_MS, function()
        cleanup_all_caches()
        log.debug("Automatic cache cleanup completed")
    end, { ["repeat"] = -1 }) -- -1 means repeat indefinitely

    log.info(string.format("Cache cleanup timer started (interval: %dms)", CLEANUP_INTERVAL_MS))
end

-- Stop automatic cache cleanup timer
local function stop_cleanup_timer()
    if cleanup_timer then
        vim.fn.timer_stop(cleanup_timer)
        cleanup_timer = nil
        log.info("Cache cleanup timer stopped")
    end
end

-- Set log level for this module
function M.set_log_level(level)
    log.set_level(level)
end

-- Clear the type source cache (useful for testing or when classpath changes)
function M.clear_type_source_cache()
    type_source_cache = {}
    log.info("Type source cache cleared")
end

-- Clear the method parameters cache
function M.clear_method_params_cache()
    method_params_cache = {}
    log.info("Method parameters cache cleared")
end

-- Clear the wildcard resolution cache
function M.clear_wildcard_cache()
    wildcard_resolution_cache = {}
    log.info("Wildcard resolution cache cleared")
end

-- Clear all caches
function M.clear_all_caches()
    type_source_cache = {}
    method_params_cache = {}
    wildcard_resolution_cache = {}
    log.info("All caches cleared")
end

-- Stop the automatic cleanup timer (for testing or shutdown)
function M.stop_cleanup_timer()
    stop_cleanup_timer()
end

-- Start/restart the automatic cleanup timer
function M.start_cleanup_timer()
    start_cleanup_timer()
end

-- Manually trigger cache cleanup (useful for testing)
function M.cleanup_now()
    cleanup_all_caches()
    log.info("Manual cache cleanup triggered")
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

    -- Check cache first and update last_used timestamp
    local cached_entry = type_source_cache[base_type]
    if cached_entry then
        log.debug("Cache hit for type:", base_type, "->", cached_entry.value)
        cached_entry.last_used = os.time()
        return cached_entry.value
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
            type_source_cache[base_type] = {
                value = res.sourcePath,
                last_used = os.time(),
            }
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

-- Get @MappingTarget annotations from source using Treesitter (fast!)
-- Returns a map: param_name -> true for parameters with @MappingTarget
local function get_mapping_target_params(method_node, bufnr)
    local mapping_target_params = {}

    if not method_node then
        log.warn("get_mapping_target_params: method_node is nil")
        return mapping_target_params
    end

    local params = method_node:field("parameters")[1]
    if not params then
        log.debug("get_mapping_target_params: no parameters field")
        return mapping_target_params
    end

    log.debug("get_mapping_target_params: checking parameters...")

    -- Check each parameter for @MappingTarget annotation
    for child in params:iter_children() do
        if child:type() == "formal_parameter" then
            -- Get parameter name
            local name_node = child:field("name")[1]
            if name_node then
                local param_name = get_node_text(name_node, bufnr)
                log.debug(string.format("  Checking param: %s", param_name))

                local has_mapping_target = false

                -- Check all children - the annotation is in a child of type "modifiers"
                for param_child in child:iter_children() do
                    if param_child:type() == "modifiers" then
                        local modifiers_text = get_node_text(param_child, bufnr)
                        log.debug(string.format("    Found modifiers: '%s'", modifiers_text or "nil"))
                        if modifiers_text and modifiers_text:match("@MappingTarget") then
                            has_mapping_target = true
                            break
                        end
                    end
                end

                if has_mapping_target then
                    mapping_target_params[param_name] = true
                    log.info("✓ Found @MappingTarget on parameter:", param_name)
                else
                    log.debug("    No @MappingTarget found")
                end
            end
        end
    end

    log.info(
        string.format("get_mapping_target_params: found %d @MappingTarget params", vim.tbl_count(mapping_target_params))
    )

    return mapping_target_params
end

-- Extract method signature from treesitter for cache key generation
-- Returns: return_type, params_array where params_array = {{type, name}, ...}
local function extract_method_signature_from_treesitter(method_node, bufnr)
    if not method_node then
        return nil, {}
    end

    -- Extract return type
    local return_type = "void"
    local type_node = method_node:field("type")[1]
    if type_node then
        return_type = get_node_text(type_node, bufnr)
    end

    -- Extract parameters with types and names
    local params_array = {}
    local params = method_node:field("parameters")[1]
    if params then
        for child in params:iter_children() do
            if child:type() == "formal_parameter" then
                local param_type_node = child:field("type")[1]
                local param_name_node = child:field("name")[1]

                if param_type_node and param_name_node then
                    local param_type = get_node_text(param_type_node, bufnr)
                    local param_name = get_node_text(param_name_node, bufnr)
                    table.insert(params_array, { type = param_type, name = param_name })
                end
            end
        end
    end

    return return_type, params_array
end

-- Generate cache key from method signature
-- Key format: "methodName|ReturnType|param1Type:param1Name,param2Type:param2Name"
local function generate_method_cache_key(method_name, return_type, params_array)
    local param_parts = {}
    for _, p in ipairs(params_array) do
        table.insert(param_parts, p.type .. ":" .. p.name)
    end
    local params_str = table.concat(param_parts, ",")
    return method_name .. "|" .. (return_type or "void") .. "|" .. params_str
end

-- Get all imports from the source file
local function get_imports_from_source(bufnr)
    local parser = vim.treesitter.get_parser(bufnr, "java")
    if not parser then
        return {}, {}
    end

    local tree = parser:parse()[1]
    if not tree then
        return {}, {}
    end

    local root = tree:root()
    local direct_imports = {}
    local wildcard_imports = {}

    -- Get buffer lines to check for wildcards
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

    -- Query for import statements
    local query_str = [[
        (import_declaration) @import_decl
    ]]

    local ok, query = pcall(vim.treesitter.query.parse, "java", query_str)
    if not ok then
        return {}, {}
    end

    for id, node in query:iter_captures(root, bufnr, 0, -1) do
        if query.captures[id] == "import_decl" then
            local start_row = node:start()
            local import_line = lines[start_row + 1]

            if import_line then
                -- Extract import statement using pattern matching
                local import_text = import_line:match("import%s+([^;]+);")

                if import_text then
                    if import_text:match("%*$") then
                        -- Wildcard import: remove .*
                        local package = import_text:gsub("%.%*$", "")
                        table.insert(wildcard_imports, package)
                        log.debug("Found wildcard import:", package)
                    else
                        -- Direct import: extract simple name and FQN
                        local simple_name = import_text:match("([^%.]+)$")
                        direct_imports[simple_name] = import_text
                        log.debug("Found direct import:", simple_name, "->", import_text)
                    end
                end
            end
        end
    end

    return direct_imports, wildcard_imports
end

-- Common java.lang types mapping
local JAVA_LANG_TYPES = {
    String = "java.lang.String",
    Object = "java.lang.Object",
    Integer = "java.lang.Integer",
    Long = "java.lang.Long",
    Double = "java.lang.Double",
    Float = "java.lang.Float",
    Boolean = "java.lang.Boolean",
    Character = "java.lang.Character",
    Byte = "java.lang.Byte",
    Short = "java.lang.Short",
    StringBuilder = "java.lang.StringBuilder",
    StringBuffer = "java.lang.StringBuffer",
    Class = "java.lang.Class",
    System = "java.lang.System",
    Math = "java.lang.Math",
    Exception = "java.lang.Exception",
    RuntimeException = "java.lang.RuntimeException",
    Thread = "java.lang.Thread",
    Runnable = "java.lang.Runnable",
    Throwable = "java.lang.Throwable",
    Error = "java.lang.Error",
    Enum = "java.lang.Enum",
    Number = "java.lang.Number",
    Comparable = "java.lang.Comparable",
    CharSequence = "java.lang.CharSequence",
    Iterable = "java.lang.Iterable",
    Override = "java.lang.Override",
    Deprecated = "java.lang.Deprecated",
    SuppressWarnings = "java.lang.SuppressWarnings",
}

-- Resolve type FQN using imports and jdtls
-- Returns: FQN or nil if not found
local function resolve_type_fqn(type_name, direct_imports, wildcard_imports, bufnr)
    -- Handle primitives
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
        return type_name
    end

    -- Extract and preserve generics and array brackets
    local generics = type_name:match("<.*>") or ""
    local array_brackets = type_name:match("%[%]$") or ""
    local base_type = type_name:gsub("<.*>", ""):gsub("%[%]$", "")

    -- Check for Lombok Builder pattern BEFORE checking if it's already an FQN
    -- Pattern: ClassName.ClassNameBuilder (need to resolve ClassName first)
    local parts = {}
    for part in base_type:gmatch("[^%.]+") do
        table.insert(parts, part)
    end

    if #parts == 2 then
        local first = parts[1]
        local second = parts[2]
        -- Check if it's ClassName.ClassNameBuilder pattern
        if second == first .. "Builder" then
            log.debug("Detected Lombok builder in resolve_type_fqn:", base_type)
            -- Recursively resolve the base class
            local base_class_fqn = resolve_type_fqn(first, direct_imports, wildcard_imports, bufnr)
            if base_class_fqn then
                local builder_fqn = base_class_fqn .. "." .. second
                log.debug("Resolved Lombok builder:", base_type, "->", builder_fqn)
                return builder_fqn .. generics .. array_brackets
            end
        end
    end

    -- Check if already FQN (contains dots)
    if base_type:match("%.") then
        return type_name
    end

    -- Check cache first
    if wildcard_resolution_cache[base_type] then
        log.debug("Wildcard resolution cache hit for:", base_type)
        return wildcard_resolution_cache[base_type] .. generics .. array_brackets
    end

    -- Check direct imports
    if direct_imports[base_type] then
        log.debug("Found in direct imports:", base_type, "->", direct_imports[base_type])
        wildcard_resolution_cache[base_type] = direct_imports[base_type]
        return direct_imports[base_type] .. generics .. array_brackets
    end

    -- Check java.lang types (implicit import)
    if JAVA_LANG_TYPES[base_type] then
        log.debug("Found in java.lang mapping:", base_type, "->", JAVA_LANG_TYPES[base_type])
        wildcard_resolution_cache[base_type] = JAVA_LANG_TYPES[base_type]
        return JAVA_LANG_TYPES[base_type] .. generics .. array_brackets
    end

    -- Try wildcard imports via jdtls
    local jdtls_client = require("utils.lsp-util").get_client_by_name("jdtls")

    if not jdtls_client then
        log.warn("jdtls client not available for type resolution - cannot resolve wildcard imports safely")
        -- Do NOT fall back to same-package assumption when JDTLS is not ready
        -- This prevents incorrect FQN resolution (e.g., com.example.mapper.DTO instead of com.example.dto.DTO)
        -- The caller will handle this failure and fall back to javap approach
        return nil
    end

    for _, package in ipairs(wildcard_imports) do
        local candidate_fqn = package .. "." .. base_type
        log.debug("Trying wildcard import:", candidate_fqn)

        local result = jdtls_client:request_sync("workspace/symbol", { query = candidate_fqn }, 5000, bufnr)
        if result and result.result and #result.result > 0 then
            -- jdtls workspace/symbol can return multiple matches (e.g., Person, PersonAnother)
            -- We need to find the exact match for our type
            for _, symbol in ipairs(result.result) do
                -- Check if this symbol exactly matches what we're looking for
                -- symbol.name should be the simple class name (e.g., "Person")
                -- symbol.containerName should be the package (e.g., "com.example" or "Outer" for inner classes)
                local symbol_fqn = nil

                if symbol.containerName and symbol.containerName ~= "" then
                    symbol_fqn = symbol.containerName .. "." .. symbol.name
                else
                    symbol_fqn = symbol.name
                end

                -- Check for exact match
                if symbol_fqn == candidate_fqn or symbol.name == base_type and symbol.containerName == package then
                    log.debug("Exact match found via wildcard import:", candidate_fqn)
                    wildcard_resolution_cache[base_type] = candidate_fqn
                    return candidate_fqn .. generics .. array_brackets
                end
            end
            log.debug(
                "No exact match found for:",
                candidate_fqn,
                "- got",
                #result.result,
                "results but none matched exactly"
            )
        end
    end

    -- Check in same package as current file (only if JDTLS is available but didn't find the type in wildcard imports)
    local package_name, _ = get_mapper_class_info(bufnr)
    if package_name then
        local same_package_fqn = package_name .. "." .. base_type
        log.debug("Assuming same package:", same_package_fqn)
        wildcard_resolution_cache[base_type] = same_package_fqn
        return same_package_fqn .. generics .. array_brackets
    end

    log.warn("Could not resolve type:", base_type)
    return nil
end

-- Get all method parameters using TreeSitter + jdtls (main approach)
-- Returns: {parameters=array of {name=string, type=string, is_mapping_target=boolean}, return_type=string} or nil on failure
local function get_all_method_parameters_main(bufnr, method_name, method_node, param_has_mapping_target)
    param_has_mapping_target = param_has_mapping_target or {}

    if not method_node then
        log.debug("get_all_method_parameters_main: method_node is nil")
        return nil
    end

    -- Extract signature from treesitter for cache key
    local return_type_from_ts, params_from_ts = extract_method_signature_from_treesitter(method_node, bufnr)
    local cache_key = generate_method_cache_key(method_name, return_type_from_ts, params_from_ts)

    -- Check cache first
    local cached_entry = method_params_cache[cache_key]
    if cached_entry then
        log.info("Cache hit for method signature (main):", cache_key)
        cached_entry.last_used = os.time()

        -- Update @MappingTarget flags
        for _, param in ipairs(cached_entry.value.parameters) do
            param.is_mapping_target = param_has_mapping_target[param.name] or false
        end

        return cached_entry.value
    end

    log.info("Cache miss for method signature (main):", cache_key)

    -- Get imports from source
    local direct_imports, wildcard_imports = get_imports_from_source(bufnr)

    -- Resolve return type
    local return_type_fqn = resolve_type_fqn(return_type_from_ts, direct_imports, wildcard_imports, bufnr)
    if not return_type_fqn then
        log.warn("Failed to resolve return type:", return_type_from_ts)
        return nil
    end

    -- Build parameters array with resolved FQNs
    local parameters = {}
    for i, param_info in ipairs(params_from_ts) do
        local param_type_fqn = resolve_type_fqn(param_info.type, direct_imports, wildcard_imports, bufnr)
        if not param_type_fqn then
            log.warn("Failed to resolve parameter type:", param_info.type)
            return nil
        end

        table.insert(parameters, {
            name = param_info.name,
            type = param_type_fqn,
            is_mapping_target = param_has_mapping_target[param_info.name] or false,
        })

        log.info(
            string.format(
                "Parameter %d: name=%s, type=%s, @MappingTarget=%s",
                i,
                param_info.name,
                param_type_fqn,
                tostring(param_has_mapping_target[param_info.name] or false)
            )
        )
    end

    local result = {
        parameters = parameters,
        return_type = return_type_fqn,
    }

    -- Store in cache
    method_params_cache[cache_key] = {
        value = result,
        last_used = os.time(),
    }
    log.info("Cached result for method signature (main):", cache_key)

    return result
end

-- Get all method parameters using javap (fallback approach)
-- Also extracts return type from the same javap call for efficiency
-- Strategy:
--   1. Check cache using method signature from treesitter
--   2. If cache hit, return cached result immediately
--   3. If cache miss, run javap:
--      - Parameter types: from javap (fully qualified class names) - NO -v flag needed!
--      - Parameter names: from Treesitter (source code)
--      - @MappingTarget: from Treesitter (source code) - passed via param_has_mapping_target map
--      - Return type: from javap method signature (first match)
-- @param param_has_mapping_target table - map of param_name -> boolean (@MappingTarget detection from Treesitter)
-- Returns: {parameters=array of {name=string, type=string, is_mapping_target=boolean}, return_type=string}
local function get_all_method_parameters_javap(bufnr, method_name, method_node, param_has_mapping_target)
    param_has_mapping_target = param_has_mapping_target or {}

    -- Extract signature from treesitter for cache key
    local return_type_from_ts, params_from_ts = extract_method_signature_from_treesitter(method_node, bufnr)
    local cache_key = generate_method_cache_key(method_name, return_type_from_ts, params_from_ts)

    -- vim.notify("key: " .. cache_key)

    -- Check cache first and update last_used timestamp
    local cached_entry = method_params_cache[cache_key]
    if cached_entry then
        log.info("Cache hit for method signature:", cache_key)
        cached_entry.last_used = os.time()

        -- Update @MappingTarget flags from current param_has_mapping_target map
        -- (these might have changed without changing the signature)
        for _, param in ipairs(cached_entry.value.parameters) do
            param.is_mapping_target = param_has_mapping_target[param.name] or false
        end

        return cached_entry.value
    end

    log.info("Cache miss for method signature:", cache_key)

    -- Get mapper class info
    local package_name, class_name = get_mapper_class_info(bufnr)
    if not package_name or not class_name then
        log.debug("Could not determine mapper class")
        return nil
    end

    local fqcn = package_name .. "." .. class_name
    log.debug("Mapper FQCN:", fqcn)

    -- Get class source path using optimized IPC call
    local classpath = get_class_source_path(fqcn)
    if not classpath then
        log.error("Could not get class source path for:", fqcn)
        vim.notify("[MapStruct Context] Could not get class source path", vim.log.levels.ERROR)
        return nil
    end

    log.debug("Using optimized classpath:", classpath)

    -- vim.notify("Calling Javap")

    -- Run javap WITHOUT verbose flag (10x faster!)
    -- We don't need -v because @MappingTarget is detected via Treesitter
    local cmd = string.format("javap -cp '%s' '%s'", classpath, fqcn)
    log.debug(string.format("Running: javap -cp <classpath> %s", fqcn))

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

    -- Parse javap output to find method signature
    -- Much simpler now - no annotation parsing needed!
    local method_found = false
    local param_types = {}
    local param_names = {}
    local return_type = nil

    -- Single pass through output
    for line in output:gmatch("[^\r\n]+") do
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
                    -- Check if parameter types match the expected types from Treesitter
                    local types_match = true
                    for i, javap_type in ipairs(candidate_param_types) do
                        local expected_type = params_from_ts[i] and params_from_ts[i].type
                        if expected_type then
                            -- Extract simple class names (last part after . or $)
                            local javap_simple = javap_type:match("([^%.$]+)$") or javap_type
                            local expected_simple = expected_type:match("([^%.$]+)$") or expected_type
                            if javap_simple ~= expected_simple then
                                types_match = false
                                log.info(
                                    string.format(
                                        "Type mismatch at param %d: expected '%s', got '%s'",
                                        i,
                                        expected_simple,
                                        javap_simple
                                    )
                                )
                                break
                            end
                        end
                    end

                    if types_match then
                        -- This is the right method!
                        param_types = candidate_param_types
                        method_found = true
                        log.info("Parameter count and types match! Using this method.")
                        break -- Found it, no need to continue
                    else
                        log.info("Parameter types mismatch, continuing search for correct overload...")
                    end
                else
                    log.info("Parameter count mismatch, continuing search...")
                end
            elseif params_str == "" and expected_param_count == 0 then
                -- No parameters expected and none found
                method_found = true
                break
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

    -- Step 3: Build result array using @MappingTarget info from Treesitter
    local parameters = {}
    log.info("Building final parameter list...")
    for i = 1, #param_types do
        local param_name = param_names[i]
        local is_mapping_target = param_has_mapping_target[param_name] or false

        table.insert(parameters, {
            name = param_name,
            type = param_types[i],
            is_mapping_target = is_mapping_target,
        })

        log.info(
            string.format(
                "Parameter %d: name=%s, type=%s, @MappingTarget=%s",
                i,
                param_name,
                param_types[i],
                tostring(is_mapping_target)
            )
        )
    end

    log.info("Total parameters extracted: " .. #parameters)

    local result = {
        parameters = parameters,
        return_type = return_type,
    }

    -- Store in cache for future calls with timestamp
    method_params_cache[cache_key] = {
        value = result,
        last_used = os.time(),
    }
    log.info("Cached result for method signature:", cache_key)

    return result
end

-- Get all method parameters with automatic fallback
-- Tries TreeSitter+jdtls first, falls back to javap if that fails
-- @param param_has_mapping_target table - map of param_name -> boolean (@MappingTarget detection from Treesitter)
-- Returns: {parameters=array of {name=string, type=string, is_mapping_target=boolean}, return_type=string}
local function get_all_method_parameters(bufnr, method_name, method_node, param_has_mapping_target)
    -- Try main approach first (TreeSitter + jdtls)
    log.info("Attempting TreeSitter+jdtls approach for method:", method_name)
    local result = get_all_method_parameters_main(bufnr, method_name, method_node, param_has_mapping_target)

    if result then
        log.info("Successfully resolved method parameters using TreeSitter+jdtls")
        return result
    end

    -- Fallback to javap
    log.warn("TreeSitter+jdtls approach failed, falling back to javap for method:", method_name)
    result = get_all_method_parameters_javap(bufnr, method_name, method_node, param_has_mapping_target)

    if result then
        log.info("Successfully resolved method parameters using javap fallback")
    else
        log.error("Both TreeSitter+jdtls and javap approaches failed for method:", method_name)
    end

    return result
end

-- Convert FQN to Java's inner class notation (using $ instead of . for inner classes)
-- For example: com.example.TestClasses.Person -> com.example.TestClasses$Person
-- This is needed because Java's Class.forName() requires $ for inner classes
--
-- Strategy: If the FQN has a segment that starts with uppercase followed by another segment
-- that also starts with uppercase, it's likely OuterClass.InnerClass pattern
local function convert_to_java_inner_class_notation(fqn)
    if not fqn or fqn == "" then
        return fqn
    end

    local parts = {}
    for part in fqn:gmatch("[^%.]+") do
        table.insert(parts, part)
    end

    -- Find the first part that starts with uppercase (likely the outer class)
    local first_class_idx = nil
    for i, part in ipairs(parts) do
        if part:match("^[A-Z]") then
            first_class_idx = i
            break
        end
    end

    -- If we found a class and there are more parts after it (inner classes)
    if first_class_idx and first_class_idx < #parts then
        -- Check for Lombok Builder pattern: ClassName.ClassNameBuilder
        -- If the last part is the first part + "Builder", it's a Lombok builder
        local last_part = parts[#parts]
        local first_class_part = parts[first_class_idx]

        if last_part == first_class_part .. "Builder" then
            -- This is a Lombok builder - it's a generated inner class, convert to $
            log.debug("Detected Lombok builder pattern:", fqn, "- converting to $ notation")
            local package_part = table.concat(parts, ".", 1, first_class_idx - 1)
            local builder_notation = first_class_part .. "$" .. last_part

            if package_part ~= "" then
                return package_part .. "." .. builder_notation
            else
                return builder_notation
            end
        end

        -- Join package parts with dots, then outer class, then inner classes with $
        local package_part = table.concat(parts, ".", 1, first_class_idx - 1)
        local class_parts = {}
        for i = first_class_idx, #parts do
            table.insert(class_parts, parts[i])
        end
        local class_part = table.concat(class_parts, "$")

        if package_part ~= "" then
            return package_part .. "." .. class_part
        else
            return class_part
        end
    end

    -- No inner class detected, return as-is
    return fqn
end

-- Extract completion context from the current cursor position using Treesitter
-- Optimized with early exit checks to avoid expensive operations for invalid contexts
-- Returns: { ok = true, value = context } on success
--          { ok = false, reason = "error_code", message = "user message" } on failure
-- Error codes:
--   - "not_java_file": Not a Java file
--   - "not_in_string": Cursor not in string literal
--   - "not_in_mapping": Not in @Mapping/@ValueMapping annotation
--   - "jdtls_not_ready": JDTLS is still initializing
--   - "invalid_path": Could not extract path expression
--   - "invalid_context": Other context validation failed
--- @return table
function M.get_completion_context(bufnr, row, col)
    -- Lazy initialization: Start cleanup timer on first use
    start_cleanup_timer()

    bufnr = bufnr or vim.api.nvim_get_current_buf()

    -- EARLY EXIT 1: Check filetype (fastest check)
    local filetype = vim.bo[bufnr].filetype
    if filetype ~= "java" then
        log.debug("Not a Java file:", filetype)
        return { ok = false, reason = "not_java_file", message = "Not a Java file" }
    end

    -- EARLY EXIT 2: Get treesitter node at cursor
    local node = get_node_at_cursor(bufnr, row, col)
    if not node then
        log.debug("No node at cursor")
        return { ok = false, reason = "invalid_context", message = "No treesitter node at cursor" }
    end

    log.debug("Node type at cursor:", node:type())

    -- EARLY EXIT 3: Check if we're in a string literal (fast treesitter check)
    local string_node = node
    if node:type() ~= "string_literal" then
        string_node = find_parent_node(node, "string_literal")
    end

    if not string_node then
        log.debug("Not in string literal")
        return { ok = false, reason = "not_in_string", message = "Not in a string literal" }
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
        return { ok = false, reason = "not_in_mapping", message = "Not in a MapStruct @Mapping annotation" }
    end

    -- At this point, we've confirmed we're in a valid MapStruct annotation context
    -- Now we can proceed with expensive operations (javap, etc.)

    local mapping_type = is_value_mapping and "ValueMapping" or "Mapping"
    log.info("Detected annotation: @" .. mapping_type .. ", attribute:", attribute_type)

    -- EARLY EXIT 6: Check if JDTLS is ready before attempting any type resolution
    -- This prevents incorrect FQN resolution when JDTLS is still initializing
    local jdtls_client = require("utils.lsp-util").get_client_by_name("jdtls")
    if not jdtls_client then
        log.warn("JDTLS is not ready yet - cannot resolve types safely")
        return {
            ok = false,
            reason = "jdtls_not_ready",
            message = "JDTLS is still initializing. Please wait and try again.",
        }
    end

    -- EARLY EXIT 7: Extract and validate path expression (before javap)
    local path_expr = extract_path_from_string(string_node, bufnr, col)
    if path_expr == nil then
        log.debug("Could not extract path")
        return { ok = false, reason = "invalid_path", message = "Could not extract path expression" }
    end

    -- For ValueMapping, only empty path is valid (enum constants, no nested paths)
    if is_value_mapping and path_expr ~= "" then
        log.debug("ValueMapping does not support nested paths")
        return { ok = false, reason = "invalid_path", message = "ValueMapping does not support nested paths" }
    end

    log.debug("Path expression: '" .. path_expr .. "'")

    -- Find the method declaration
    local method_node = find_method_declaration(annotation_node)
    if not method_node then
        log.debug("No method declaration found")
        return { ok = false, reason = "invalid_context", message = "No method declaration found" }
    end

    -- Get method name for parameter extraction
    local method_name_node = method_node:field("name")[1]
    if not method_name_node then
        log.debug("Could not get method name")
        return { ok = false, reason = "invalid_context", message = "Could not get method name" }
    end
    local method_name = get_node_text(method_name_node, bufnr)

    if attribute_type == "source" then
        -- For source attribute, get ALL method parameters (excluding @MappingTarget)
        -- Use Treesitter to detect @MappingTarget (no verbose javap needed!)
        local mapping_target_params = get_mapping_target_params(method_node, bufnr)

        local result = get_all_method_parameters(bufnr, method_name, method_node, mapping_target_params)

        if not result or not result.parameters or #result.parameters == 0 then
            log.debug("Could not extract method parameters")
            return { ok = false, reason = "invalid_context", message = "Could not extract method parameters" }
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
                    type = convert_to_java_inner_class_notation(param.type),
                })
                log.info("Added source parameter:", param.name, param.type)
            else
                log.info("Skipped @MappingTarget parameter:", param.name)
            end
        end

        if #sources == 0 then
            log.warn("No source parameters found (all are @MappingTarget?)")
            return {
                ok = false,
                reason = "invalid_context",
                message = "No source parameters found (all are @MappingTarget?)",
            }
        end

        log.info(string.format("Built sources array with %d parameter(s)", #sources))

        return {
            ok = true,
            value = {
                sources = sources, -- Array of {name, type}
                path_expression = path_expr,
                attribute_type = attribute_type, -- "source"
                is_enum = is_value_mapping, -- true for @ValueMapping (enum constants)
                line = row,
                col = col,
            },
        }
    elseif attribute_type == "target" then
        -- For target attribute, we need to determine the target type:
        -- 1. If method has @MappingTarget parameter, use that parameter's type
        -- 2. Otherwise, use the return type (extracted from the same javap call)

        -- Use Treesitter to detect @MappingTarget (no verbose javap needed!)
        local mapping_target_params = get_mapping_target_params(method_node, bufnr)
        local result = get_all_method_parameters(bufnr, method_name, method_node, mapping_target_params)
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
                return { ok = false, reason = "invalid_context", message = "Could not resolve target class" }
            end
        end

        -- Check if target type is void (invalid)
        if target_type == "void" then
            log.error("Target type is void, cannot navigate fields")
            return {
                ok = false,
                reason = "invalid_context",
                message = "Method has void return type and no @MappingTarget parameter",
            }
        end

        return {
            ok = true,
            value = {
                class_name = convert_to_java_inner_class_notation(target_type), -- Convert to $ notation for Java
                path_expression = path_expr,
                attribute_type = attribute_type, -- "target"
                is_enum = is_value_mapping, -- true for @ValueMapping (enum constants)
                line = row,
                col = col,
            },
        }
    else
        log.error("Unknown attribute type:", attribute_type)
        return { ok = false, reason = "invalid_context", message = "Unknown attribute type: " .. attribute_type }
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
