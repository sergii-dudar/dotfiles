local util = require("utils.common-util")
local mapstruct = require("modules.java.mapstruct")
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

-- Member matchers (each returns the 0-indexed column of the member on `line`, or nil).
-- Kept separate so `find_field_position` can order them by the mapping side we came from
-- (a `source` reads via a getter, a `target` writes via a setter), which is what makes
-- the landing consistent — generated types (e.g. Avro) name fields in snake_case
-- (`segment_id`) but accessors in camelCase (`getSegmentId`), so matching the camelCase
-- member against a field declaration is unreliable while the accessor is exact.

--- A field / record-component DECLARATION: a type token (word, `>` or `]` for generics
--- / arrays) then the name then a terminator (`;` `=` `,` `)`). Requiring a preceding
--- type avoids matching a bare mention such as `return name;` before the real field.
---@param line string
---@param esc string vim.pesc'd member name
---@return integer|nil col 0-indexed
local function find_field_decl(line, esc)
    local c = line:find('[^"][%w_>%]]%s+' .. esc .. "%f[^%w_]%s*[;=,)]")
    if c then
        local name_start = line:find(esc .. "%f[^%w_]", c)
        return name_start and name_start - 1 or c - 1
    end
    return nil
end

--- JavaBeans getter `getName(` (name capitalized). Exact regardless of field casing.
---@param line string
---@param ecap string vim.pesc'd capitalized name
---@return integer|nil col 0-indexed
local function find_getter(line, ecap)
    local c = line:find("get" .. ecap .. "%s*%(")
    return c and c - 1 or nil
end

--- JavaBeans setter `setName(` (name capitalized).
---@param line string
---@param ecap string vim.pesc'd capitalized name
---@return integer|nil col 0-indexed
local function find_setter(line, ecap)
    local c = line:find("set" .. ecap .. "%s*%(")
    return c and c - 1 or nil
end

--- Record accessor / fluent getter: `name()` with no arguments.
---@param line string
---@param esc string vim.pesc'd member name
---@return integer|nil col 0-indexed
local function find_accessor(line, esc)
    local c = line:find("%f[%w_]" .. esc .. "%f[^%w_]%s*%(%s*%)")
    return c and c - 1 or nil
end

--- Fluent builder setter: `name(<arg>…)` (arguments present) — distinct from the
--- no-arg accessor above.
---@param line string
---@param esc string vim.pesc'd member name
---@return integer|nil col 0-indexed
local function find_fluent(line, esc)
    local c = line:find("%f[%w_]" .. esc .. "%f[^%w_]%s*%(%s*[%w_@]")
    return c and c - 1 or nil
end

--- Enum constant: bare name followed by `,` `;` `(` `)` — matches `NEW,` / `NEW;` /
--- `NEW(args)`. Only used when the attribute is an enum, so this never over-matches a
--- word buried inside a qualified name in ordinary types.
---@param line string
---@param esc string vim.pesc'd member name
---@return integer|nil col 0-indexed
local function find_enum_const(line, esc)
    local c = line:find('[^"]%f[%w_]' .. esc .. "%f[^%w_]%s*[,;()]")
    if c then
        local name_start = line:find(esc, c)
        return name_start and name_start - 1 or c - 1
    end
    return nil
end

-- Find the line number of a field or method (getter/setter/builder) in a Java class.
-- Matchers are tried by preference (see `pref`): the FIRST hit of each matcher is recorded
-- while scanning the class body, then the highest-priority matcher that hit wins — so
-- physical order in the file never lets a lower-priority form (e.g. the field) beat a
-- higher-priority one (e.g. the getter) that appears later.
-- @param bufnr buffer number (0 for current buffer)
-- @param class_name the name of the class (can be inner class)
-- @param field_name the field name to search for
-- @param pref { attribute_type?: "source"|"target", is_enum?: boolean } mapping side hint
-- @return line number (1-indexed), column (0-indexed) or nil, nil if not found
local function find_field_position(bufnr, class_name, field_name, pref)
    bufnr = bufnr or 0
    pref = pref or {}
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

    local escaped_field_name = vim.pesc(field_name)
    local capitalized = field_name:sub(1, 1):upper() .. field_name:sub(2)
    local escaped_capitalized = vim.pesc(capitalized)

    -- Named matchers, bound to this member's escaped forms.
    local field = {
        name = "field",
        fn = function(l)
            return find_field_decl(l, escaped_field_name)
        end,
    }
    local getter = {
        name = "getter",
        fn = function(l)
            return find_getter(l, escaped_capitalized)
        end,
    }
    local setter = {
        name = "setter",
        fn = function(l)
            return find_setter(l, escaped_capitalized)
        end,
    }
    local accessor = {
        name = "accessor",
        fn = function(l)
            return find_accessor(l, escaped_field_name)
        end,
    }
    local fluent = {
        name = "fluent",
        fn = function(l)
            return find_fluent(l, escaped_field_name)
        end,
    }
    local enum_const = {
        name = "enum",
        fn = function(l)
            return find_enum_const(l, escaped_field_name)
        end,
    }

    -- Preference order. Source reads → getter/accessor first; target writes →
    -- setter/fluent first; enum attributes → the constant. Field is a fallback.
    local order
    if pref.is_enum then
        order = { enum_const, getter, accessor, field }
    elseif pref.attribute_type == "source" then
        order = { getter, accessor, field, fluent, setter }
    elseif pref.attribute_type == "target" then
        order = { setter, fluent, field, accessor, getter }
    else
        order = { field, getter, accessor, setter, fluent }
    end

    -- Pre-compile class detection patterns
    -- Use word boundary to handle cases where class name is at end of line
    local escaped_class_name = vim.pesc(class_name)
    local class_patterns = {
        "class%s+" .. escaped_class_name .. "%f[^%w_]",
        "interface%s+" .. escaped_class_name .. "%f[^%w_]",
        "record%s+" .. escaped_class_name .. "%f[^%w_]",
        "enum%s+" .. escaped_class_name .. "%f[^%w_]",
    }

    local in_target_class = false
    local class_depth = 0
    local brace_depth = 0
    local hits = {} -- matcher name -> { line_num, col }

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
        local open_count = select(2, line:gsub("[{(]", ""))
        local close_count = select(2, line:gsub("[})]", ""))
        brace_depth = brace_depth + open_count - close_count

        -- While inside the target class, record the first hit of each matcher.
        if in_target_class then
            for _, matcher in ipairs(order) do
                if not hits[matcher.name] then
                    local col = matcher.fn(line)
                    if col then
                        hits[matcher.name] = { line_num, col }
                    end
                end
            end

            -- Exit the target class when we close its braces
            if brace_depth < class_depth then
                in_target_class = false
            end
        end
    end

    -- Highest-priority matcher that hit wins.
    for _, matcher in ipairs(order) do
        local hit = hits[matcher.name]
        if hit then
            return hit[1], hit[2]
        end
    end

    return nil, nil
end

--- Add the current location to the jump list before a manual cursor move.
---@return boolean
local function push_current_position_to_jumplist()
    local ok, pushed = pcall(function()
        local bufnr = vim.api.nvim_get_current_buf()
        local line_count = vim.api.nvim_buf_line_count(bufnr)
        if line_count < 2 then
            return false
        end

        local view = vim.fn.winsaveview()
        local cursor = vim.api.nvim_win_get_cursor(0)
        if cursor[1] < line_count then
            vim.cmd("normal! G")
        else
            vim.cmd("normal! gg")
        end
        vim.fn.winrestview(view)

        return true
    end)

    return ok and pushed == true
end

-- Search for field and navigate to it
-- Extracted to avoid code duplication
---@param pref? { attribute_type?: "source"|"target", is_enum?: boolean } mapping side hint
local function navigate_to_field(simple_name, member_name, opts, pref)
    opts = opts or {}
    vim.schedule(function()
        local attempts = 0
        while attempts < 2 do
            local line_num, col = find_field_position(0, simple_name, member_name, pref)

            if line_num then
                if opts.push_jump ~= false then
                    push_current_position_to_jumplist()
                end
                vim.api.nvim_win_set_cursor(0, { line_num, col or 0 })
                vim.cmd("normal! zz") -- Center the screen
                return
            end

            -- support goto for lombok builder
            if vim.endswith(simple_name, "Builder") then
                simple_name = simple_name:gsub("Builder$", "")
            else
                break
            end

            attempts = attempts + 1
        end

        vim.notify(
            string.format("[MapStruct] Field '%s' not found in class '%s'", member_name, simple_name),
            vim.log.levels.WARN
        )
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

    -- Extract the parent path, including the trailing dot expected by the path explorer.
    local path = full:sub(1, #full - #member_with_right)
    if path == "" then
        return { member = member }
    end

    return { path = path, member = member }
end

--- Return current-buffer cursor params in MapStruct's expected shape.
---@return { bufnr: integer, row: integer, col: integer }
local function get_current_cursor_params()
    local row, col = unpack(vim.api.nvim_win_get_cursor(0))
    return {
        bufnr = vim.api.nvim_get_current_buf(),
        row = row - 1,
        col = col,
    }
end

--- Copy cursor params and optionally force the parent path used for lookup.
---@param params table
---@param current_path table
---@return table
local function get_lookup_params(params, current_path)
    local lookup_params = vim.deepcopy(params)
    if current_path.path then
        lookup_params.path_expression = current_path.path
    end

    return lookup_params
end

--- Remove Java generic arguments from a type name.
---@param type_name string|nil
---@return string|nil
local function erase_type_arguments(type_name)
    if type(type_name) ~= "string" then
        return type_name
    end

    local result = {}
    local generic_depth = 0

    for idx = 1, #type_name do
        local char = type_name:sub(idx, idx)
        if char == "<" then
            generic_depth = generic_depth + 1
        elseif char == ">" then
            generic_depth = math.max(generic_depth - 1, 0)
        elseif generic_depth == 0 then
            table.insert(result, char)
        end
    end

    return table.concat(result):gsub("%s+", "")
end

--- Return source root when path is exactly a direct method-parameter object path.
---@param path string|nil
---@return string|nil
local function get_direct_source_root(path)
    if type(path) ~= "string" then
        return nil
    end

    return path:match("^([%w_]+)%.$")
end

--- Find a source parameter in a completion context by name.
---@param completion_ctx table
---@param source_name string
---@return table|nil
local function find_source_by_name(completion_ctx, source_name)
    for _, source in ipairs(completion_ctx.sources or {}) do
        if source.name == source_name then
            return source
        end
    end

    return nil
end

--- Return load FQN and simple class name for a Java source type.
---@param type_name string|nil
---@return string|nil class_fqn
---@return string|nil simple_name
local function get_class_parts(type_name)
    local class_fqn = erase_type_arguments(type_name)
    if not class_fqn or class_fqn == "" then
        return nil, nil
    end

    class_fqn = class_fqn:gsub("%[%]$", "")
    local simple_name = class_fqn:match("%$([^$]+)$") or class_fqn:match("[^%.]+$")
    local load_fqn = class_fqn:gsub("%$.*$", "")

    return load_fqn, simple_name
end

--- Open a Java class with JDTLS and navigate to a member in that class.
---@param class_fqn string
---@param simple_name string
---@param member_name string
---@param opts table
---@param pref? { attribute_type?: "source"|"target", is_enum?: boolean } mapping side hint
local function open_class_member(class_fqn, simple_name, member_name, opts, pref)
    jdtls_util.jdt_load_unique_class(class_fqn, function(class_result)
        if not class_result or not class_result.location then
            vim.notify("[MapStruct] Could not load class: " .. class_fqn, vim.log.levels.ERROR)
            return
        end

        if opts.is_open_as_floating_win then
            local uri = class_result.location.uri or class_result.location.targetUri
            local bufnr = vim.uri_to_bufnr(uri)
            vim.fn.bufload(bufnr)
            require("goto-preview.lib").open_floating_win(bufnr, { 1, 0 })
        else
            push_current_position_to_jumplist()
            vim.lsp.util.show_document(class_result.location, "utf-8", { focus = true })
        end

        navigate_to_field(simple_name, member_name, { push_jump = false }, pref)
    end)
end

--- Jump directly to a field of a source method parameter when the owner is known.
---@param current_path table
---@param completion_ctx table
---@param opts table
---@return boolean
local function navigate_to_direct_source_field(current_path, completion_ctx, opts)
    if not completion_ctx or completion_ctx.attribute_type ~= "source" then
        return false
    end

    local source_root = get_direct_source_root(current_path.path)
    if not source_root then
        return false
    end

    local source = find_source_by_name(completion_ctx, source_root)
    if not source then
        return false
    end

    local class_fqn, simple_name = get_class_parts(source.type)
    if not class_fqn or not simple_name then
        return false
    end

    -- Direct source field: a `source` path, so prefer the getter.
    open_class_member(class_fqn, simple_name, current_path.member, opts, {
        attribute_type = "source",
        is_enum = completion_ctx.is_enum,
    })
    return true
end

--- Find a method parameter declaration near the mapping annotation.
---@param bufnr integer
---@param param_name string
---@param start_row integer
---@return integer|nil line_num
---@return integer|nil col
local function find_method_parameter_position(bufnr, param_name, start_row)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local max_line = math.min(#lines, start_row + 80)
    local in_parameter_list = false
    local escaped_name = vim.pesc(param_name)

    for line_num = start_row + 1, max_line do
        local line = lines[line_num] or ""
        local search_start = 1

        if not in_parameter_list and not line:match("^%s*@") then
            local open_paren = line:find("%(")
            if open_paren then
                in_parameter_list = true
                search_start = open_paren + 1
            end
        end

        if in_parameter_list then
            local col = line:find("%f[%w_]" .. escaped_name .. "%f[^%w_]", search_start)
            if col then
                return line_num, col - 1
            end

            if line:find("%)") then
                break
            end
        end
    end

    return nil, nil
end

--- Jump to the declaration of a source method parameter.
---@param params { bufnr?: integer, row?: integer }
---@param param_name string
---@return boolean
local function navigate_to_source_parameter(params, param_name)
    local bufnr = params.bufnr or vim.api.nvim_get_current_buf()
    local start_row = params.row or (vim.api.nvim_win_get_cursor(0)[1] - 1)
    local line_num, col = find_method_parameter_position(bufnr, param_name, start_row)

    if not line_num then
        return false
    end

    push_current_position_to_jumplist()

    if vim.api.nvim_get_current_buf() ~= bufnr then
        vim.api.nvim_set_current_buf(bufnr)
    end

    vim.api.nvim_win_set_cursor(0, { line_num, col or 0 })
    vim.cmd("normal! zz")
    return true
end

--- Check whether a member is one of the current source method parameters.
---@param completion_ctx table
---@param member_name string
---@return boolean
local function is_source_parameter(completion_ctx, member_name)
    if not completion_ctx or completion_ctx.attribute_type ~= "source" then
        return false
    end

    for _, source in ipairs(completion_ctx.sources or {}) do
        if source.name == member_name then
            return true
        end
    end

    return false
end

---@param line string
---@param quote_idx integer
---@return boolean
local function is_escaped_quote(line, quote_idx)
    local slash_count = 0
    local idx = quote_idx - 1
    while idx >= 1 and line:sub(idx, idx) == "\\" do
        slash_count = slash_count + 1
        idx = idx - 1
    end
    return slash_count % 2 == 1
end

---@param line string
---@param start_idx integer
---@return integer|nil
local function find_left_quote(line, start_idx)
    for idx = math.min(start_idx, #line), 1, -1 do
        if line:sub(idx, idx) == '"' and not is_escaped_quote(line, idx) then
            return idx
        end
    end
    return nil
end

---@param line string
---@param start_idx integer
---@return integer|nil
local function find_right_quote(line, start_idx)
    for idx = math.max(start_idx, 1), #line do
        if line:sub(idx, idx) == '"' and not is_escaped_quote(line, idx) then
            return idx
        end
    end
    return nil
end

--- Check whether the cursor is inside a quoted string without touching Tree-sitter.
---@return boolean
local function is_cursor_inside_quoted_string()
    local _, col = unpack(vim.api.nvim_win_get_cursor(0))
    local line = vim.api.nvim_get_current_line()
    local cursor_idx = col + 1
    return find_left_quote(line, cursor_idx) ~= nil and find_right_quote(line, cursor_idx + 1) ~= nil
end

--- Check whether the current cursor is on a MapStruct path item that can own go-to-definition.
---@param params? { bufnr?: integer, row?: integer, col?: integer }
---@return boolean
function M.can_goto_path_item_definition(params)
    if not mapstruct.is_mapper_file(params and params.bufnr) then
        return false
    end

    if not is_cursor_inside_quoted_string() then
        return false
    end

    local current_path = get_mapping_path_under_cursor()
    if not current_path.member then
        return false
    end

    return mapstruct.is_in_mapping_context(params or {})
end

---@class GoToMapStructOptions
---@field is_open_as_floating_win (boolean|nil) widget to apply icon, default - false
---@param opts GoToMapStructOptions|nil of options
function M.goto_path_item_definitions(opts)
    opts = opts or {}
    local current_path = get_mapping_path_under_cursor()
    local params = opts.params or get_current_cursor_params()
    local lookup_params = get_lookup_params(params, current_path)

    if not current_path.member then
        vim.notify("[MapStruct] No path member found under cursor", vim.log.levels.WARN)
        return
    end

    local context_result = mapstruct.get_context(params)

    -- Mapping side (source→getter / target→setter) so the landing prefers the right
    -- accessor over the (snake_case, unreliable) field declaration in generated types.
    local pref = context_result.ok
            and context_result.value
            and {
                attribute_type = context_result.value.attribute_type,
                is_enum = context_result.value.is_enum,
            }
        or nil

    if not current_path.path then
        if
            context_result.ok
            and is_source_parameter(context_result.value, current_path.member)
            and navigate_to_source_parameter(params, current_path.member)
        then
            return
        end
    end

    if context_result.ok and navigate_to_direct_source_field(current_path, context_result.value, opts) then
        return
    end

    -- Get completions to find the class FQN that contains our member
    -- Note: get_completions internally calls get_context, so we don't need to call it separately
    mapstruct.get_completions(lookup_params, function(result, err)
        if err then
            vim.notify("[MapStruct] Failed to get completions: " .. err, vim.log.levels.ERROR)
            return
        end

        -- If no result, it means we're not in a valid @Mapping annotation context
        if not result then
            vim.notify("[MapStruct] Not in a valid @Mapping annotation", vim.log.levels.WARN)
            return
        end

        if not result.className then
            vim.notify("[MapStruct] No class information found", vim.log.levels.WARN)
            return
        end

        -- Extract the class FQN (remove inner class notation if present)
        local class_fqn = result.className:gsub("%$.*$", "")

        -- Get simple class name for field search (might be inner class)
        local simple_name = result.simpleName or result.className:match("[^%.]+$")

        -- Try to find the file directly in the project first (fast path)
        --[[ local project_file = find_project_file(class_fqn)
        if project_file then
            -- Fast path: Open project file directly
            vim.cmd.edit(project_file)
            -- require("goto-preview.lib").open_floating_win("file:///" .. project_file, { 1, 0 })
            navigate_to_field(simple_name, current_path.member)
            -- vim.notify("local search")
        else ]]
        -- Slow path: Use jdtls to load the class (JAR dependencies, external libs)
        open_class_member(class_fqn, simple_name, current_path.member, opts, pref)
        -- end
    end)
end

return M
