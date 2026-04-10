local M = {}

local NS = vim.api.nvim_create_namespace("java_format_check")

local LOG_METHODS = { debug = true, info = true, warn = true, error = true, trace = true }

-- Treesitter query: find method_invocation nodes with relevant method names.
-- @_name is predicate-only (underscore prefix = not yielded by iter_captures).
local QUERY_STR = [[
    (method_invocation
        name: (identifier) @_name
        (#match? @_name "^(format|formatted|debug|info|warn|error|trace)$")
    ) @call
]]

local STRING_NODE_TYPES = { string_literal = true, text_block = true }

-- ---------------------------------------------------------------------------
--  Placeholder counting
-- ---------------------------------------------------------------------------

--- Count printf-style placeholders (%s, %d, %02d, %1$s, etc.) in a raw string.
--- Handles %% (escaped percent) correctly.
local function count_printf_placeholders(s)
    -- Remove surrounding quotes
    s = s:gsub('^"', ""):gsub('"$', "")
    -- Remove triple-quote text block delimiters
    s = s:gsub('^"""', ""):gsub('"""$', "")
    -- Neutralise escaped percent signs so they don't look like format specs
    s = s:gsub("%%%%", "\0\0")
    local count = 0
    -- Match: % [argument_index$] [flags] [width] [.precision] conversion
    -- Simplified pattern that catches real-world format specifiers
    for _ in s:gmatch("%%[%-+ #0%(]*[%d%.]*%$?[%-+ #0%(]*[%d%.]*[a-zA-Z]") do
        count = count + 1
    end
    return count
end

--- Count SLF4J-style {} placeholders in a raw string.
--- Handles \{} (escaped braces) correctly.
local function count_slf4j_placeholders(s)
    -- Remove surrounding quotes
    s = s:gsub('^"', ""):gsub('"$', "")
    s = s:gsub('^"""', ""):gsub('"""$', "")
    -- Neutralise escaped braces
    s = s:gsub("\\{}", "\0\0\0")
    local count = 0
    for _ in s:gmatch("{}") do
        count = count + 1
    end
    return count
end

-- ---------------------------------------------------------------------------
--  Treesitter helpers
-- ---------------------------------------------------------------------------

--- Collect argument nodes from an argument_list, skipping punctuation.
local function collect_arg_nodes(arg_list)
    local args = {}
    for child in arg_list:iter_children() do
        local t = child:type()
        if t ~= "(" and t ~= ")" and t ~= "," then
            args[#args + 1] = child
        end
    end
    return args
end

--- Return the first string_literal or text_block child of an argument_list.
local function first_string_literal(arg_list, bufnr)
    for child in arg_list:iter_children() do
        if STRING_NODE_TYPES[child:type()] then
            return child
        end
    end
    return nil
end

--- Walk children of a method_invocation to extract object, method name, and argument_list.
--- Returns obj_node, method_name_str, arg_list_node (any may be nil).
local function decompose_invocation(node, bufnr)
    local obj_node, method_name, arg_list
    local prev_child = nil
    for child in node:iter_children() do
        local t = child:type()
        if t == "identifier" then
            -- The last identifier before argument_list is the method name.
            -- Any preceding expression/identifier is the object.
            if method_name then
                -- Previous identifier was actually the object
                obj_node = prev_child
            end
            method_name = vim.treesitter.get_node_text(child, bufnr)
            prev_child = child
        elseif t == "argument_list" then
            arg_list = child
        elseif t ~= "." and t ~= "(" and t ~= ")" and t ~= "," and t ~= "type_arguments" then
            -- Non-punctuation, non-identifier child before the method name = the object
            obj_node = child
        end
    end
    return obj_node, method_name, arg_list
end

-- ---------------------------------------------------------------------------
--  Call classification
-- ---------------------------------------------------------------------------

--- Classify a method_invocation node. Returns a result table or nil.
--- Result: { kind = "printf"|"slf4j", fmt_node, placeholder_count, arg_count, wrong_style = false|string }
local function classify_invocation(node, bufnr)
    local obj_node, method_name, arg_list = decompose_invocation(node, bufnr)
    if not method_name or not arg_list then
        return nil
    end

    local fmt_node = nil
    local is_printf = false
    local is_slf4j = false
    local fmt_is_first_arg = false -- true when format string is in argument_list (not receiver)

    -- Case A: "...".formatted(args...)
    if method_name == "formatted" and obj_node and STRING_NODE_TYPES[obj_node:type()] then
        is_printf = true
        fmt_node = obj_node
        fmt_is_first_arg = false

    -- Case B: String.format(fmt, args...)
    elseif method_name == "format" and obj_node then
        local obj_text = vim.treesitter.get_node_text(obj_node, bufnr)
        if obj_text == "String" then
            fmt_node = first_string_literal(arg_list, bufnr)
            is_printf = true
            fmt_is_first_arg = true
        end

    -- Case C: log.debug/info/warn/error/trace(fmt, args...)
    elseif LOG_METHODS[method_name] then
        fmt_node = first_string_literal(arg_list, bufnr)
        is_slf4j = true
        fmt_is_first_arg = true
    end

    if not fmt_node then
        return nil
    end

    local raw_text = vim.treesitter.get_node_text(fmt_node, bufnr)
    local printf_count = count_printf_placeholders(raw_text)
    local slf4j_count = count_slf4j_placeholders(raw_text)

    local arg_nodes = collect_arg_nodes(arg_list)
    local arg_count = #arg_nodes
    if fmt_is_first_arg then
        arg_count = arg_count - 1 -- exclude the format string itself
    end

    local placeholder_count, wrong_style

    if is_printf then
        placeholder_count = printf_count
        -- Detect wrong style: {} in a printf context
        if slf4j_count > 0 and printf_count == 0 then
            wrong_style = "slf4j_in_printf"
        else
            wrong_style = false
        end
    elseif is_slf4j then
        placeholder_count = slf4j_count
        -- Detect wrong style: %s in a SLF4J context
        if printf_count > 0 and slf4j_count == 0 then
            wrong_style = "printf_in_slf4j"
        else
            wrong_style = false
        end
    else
        return nil
    end

    return {
        fmt_node = fmt_node,
        kind = is_printf and "printf" or "slf4j",
        placeholder_count = placeholder_count,
        arg_count = arg_count,
        wrong_style = wrong_style,
    }
end

-- ---------------------------------------------------------------------------
--  Diagnostic message construction
-- ---------------------------------------------------------------------------

local function build_message(result)
    -- Wrong style takes priority (more fundamental error)
    if result.wrong_style == "slf4j_in_printf" then
        return "String.format uses %s/%d placeholders, not {}"
    elseif result.wrong_style == "printf_in_slf4j" then
        return "SLF4J log uses {} placeholders, not %s/%d"
    end

    local expected = result.placeholder_count
    local got = result.arg_count

    if expected == got then
        return nil -- correct
    end

    -- SLF4J throwable exception: last arg may be a Throwable (not consuming a {})
    if result.kind == "slf4j" and got == expected + 1 then
        return nil
    end

    return string.format("format string expects %d argument(s) but %d provided", expected, got)
end

-- ---------------------------------------------------------------------------
--  Buffer scan
-- ---------------------------------------------------------------------------

local function scan_buffer(bufnr)
    if not vim.api.nvim_buf_is_valid(bufnr) or not vim.api.nvim_buf_is_loaded(bufnr) then
        return {}
    end

    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return {}
    end
    local trees = parser:parse()
    if not trees or not trees[1] then
        return {}
    end
    local root = trees[1]:root()

    local ok2, query = pcall(vim.treesitter.query.parse, "java", QUERY_STR)
    if not ok2 or not query then
        return {}
    end

    -- Find the capture index for @call
    local call_id
    for i, name in ipairs(query.captures) do
        if name == "call" then
            call_id = i
            break
        end
    end
    if not call_id then
        return {}
    end

    local diagnostics = {}
    for id, node, _ in query:iter_captures(root, bufnr, 0, -1) do
        if id == call_id then
            local result = classify_invocation(node, bufnr)
            if result then
                local msg = build_message(result)
                if msg then
                    local sr, sc, er, ec = result.fmt_node:range()
                    diagnostics[#diagnostics + 1] = {
                        lnum = sr,
                        col = sc,
                        end_lnum = er,
                        end_col = ec,
                        severity = vim.diagnostic.severity.WARN,
                        message = msg,
                        source = "java-format",
                    }
                end
            end
        end
    end
    return diagnostics
end

-- ---------------------------------------------------------------------------
--  Public API
-- ---------------------------------------------------------------------------

function M.apply(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()
    if not vim.api.nvim_buf_is_valid(bufnr) or not vim.api.nvim_buf_is_loaded(bufnr) then
        return
    end
    vim.diagnostic.set(NS, bufnr, scan_buffer(bufnr))
end

function M.setup()
    vim.api.nvim_create_autocmd("BufWritePost", {
        group = vim.api.nvim_create_augroup("java_format_check", { clear = true }),
        pattern = "*.java",
        callback = function(ev)
            M.apply(ev.buf)
        end,
    })
end

return M
