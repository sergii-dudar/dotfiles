local M = {}

-- Namespace for diagnostic messages (WARN severity)
local NS = vim.api.nvim_create_namespace("java_format_check")
-- Namespace for per-placeholder color extmarks (green/red)
local NS_HL = vim.api.nvim_create_namespace("java_format_check_hl")

local LOG_METHODS = { debug = true, info = true, warn = true, error = true, trace = true }

-- Highlight groups: users can override via nvim_set_hl before this module loads.
vim.api.nvim_set_hl(0, "JavaFormatOk", { default = true, link = "DiagnosticOk" })
vim.api.nvim_set_hl(0, "JavaFormatBad", { default = true, link = "DiagnosticError" })

-- Treesitter query: find method_invocation nodes with relevant method names.
local QUERY_STR = [[
    (method_invocation
        name: (identifier) @_name
        (#match? @_name "^(format|formatted|debug|info|warn|error|trace)$")
    ) @call
]]

local STRING_NODE_TYPES = { string_literal = true, text_block = true }

-- ---------------------------------------------------------------------------
--  Placeholder position finding
-- ---------------------------------------------------------------------------

--- Find byte positions of printf-style placeholders in unquoted string content.
--- Returns list of { offset = <0-based>, length = N }.
--- Handles %% (escaped percent) correctly.
local function find_printf_positions(s)
    local positions = {}
    local i = 1
    while i <= #s do
        if s:byte(i) == 0x25 then -- '%'
            if i + 1 <= #s and s:byte(i + 1) == 0x25 then
                i = i + 2 -- skip %%
            else
                local spec = s:match("^(%%[%-+ #0%(]*[%d%.]*%$?[%-+ #0%(]*[%d%.]*[a-zA-Z])", i)
                if spec then
                    positions[#positions + 1] = { offset = i - 1, length = #spec }
                    i = i + #spec
                else
                    i = i + 1
                end
            end
        else
            i = i + 1
        end
    end
    return positions
end

--- Find byte positions of SLF4J {} placeholders in unquoted string content.
--- Returns list of { offset = <0-based>, length = 2 }.
--- Handles \{} (escaped braces) correctly.
local function find_slf4j_positions(s)
    local positions = {}
    local i = 1
    while i <= #s do
        if s:byte(i) == 0x5C and i + 2 <= #s and s:sub(i + 1, i + 2) == "{}" then
            i = i + 3 -- skip \{}
        elseif s:sub(i, i + 1) == "{}" then
            positions[#positions + 1] = { offset = i - 1, length = 2 }
            i = i + 2
        else
            i = i + 1
        end
    end
    return positions
end

-- ---------------------------------------------------------------------------
--  Treesitter helpers
-- ---------------------------------------------------------------------------

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

local function first_string_literal(arg_list)
    for child in arg_list:iter_children() do
        if STRING_NODE_TYPES[child:type()] then
            return child
        end
    end
    return nil
end

--- Decompose a method_invocation node into object, method name, argument_list.
local function decompose_invocation(node, bufnr)
    local obj_node, method_name, arg_list
    local prev_child = nil
    for child in node:iter_children() do
        local t = child:type()
        if t == "identifier" then
            if method_name then
                obj_node = prev_child
            end
            method_name = vim.treesitter.get_node_text(child, bufnr)
            prev_child = child
        elseif t == "argument_list" then
            arg_list = child
        elseif t ~= "." and t ~= "(" and t ~= ")" and t ~= "," and t ~= "type_arguments" then
            obj_node = child
        end
    end
    return obj_node, method_name, arg_list
end

-- ---------------------------------------------------------------------------
--  Call classification
-- ---------------------------------------------------------------------------

--- Classify a method_invocation node.
--- Returns { kind, fmt_node, placeholder_count, arg_count, arg_nodes, wrong_style } or nil.
local function classify_invocation(node, bufnr)
    local obj_node, method_name, arg_list = decompose_invocation(node, bufnr)
    if not method_name or not arg_list then
        return nil
    end

    local fmt_node = nil
    local is_printf = false
    local is_slf4j = false
    local fmt_is_first_arg = false

    if method_name == "formatted" and obj_node and STRING_NODE_TYPES[obj_node:type()] then
        is_printf = true
        fmt_node = obj_node
    elseif method_name == "format" and obj_node then
        local obj_text = vim.treesitter.get_node_text(obj_node, bufnr)
        if obj_text == "String" then
            fmt_node = first_string_literal(arg_list)
            is_printf = true
            fmt_is_first_arg = true
        end
    elseif LOG_METHODS[method_name] then
        fmt_node = first_string_literal(arg_list)
        is_slf4j = true
        fmt_is_first_arg = true
    end

    if not fmt_node then
        return nil
    end

    -- Build format-arg node list (excluding the format string itself)
    local all_arg_nodes = collect_arg_nodes(arg_list)
    local format_arg_nodes = {}
    if fmt_is_first_arg then
        for i = 2, #all_arg_nodes do
            format_arg_nodes[#format_arg_nodes + 1] = all_arg_nodes[i]
        end
    else
        format_arg_nodes = all_arg_nodes
    end

    local raw_text = vim.treesitter.get_node_text(fmt_node, bufnr)
    -- Strip quotes to get content
    local content
    if raw_text:sub(1, 3) == '"""' then
        content = raw_text:sub(4):gsub('"""%s*$', "")
        content = content:gsub("^\n", "")
    else
        content = raw_text:sub(2, -2)
    end

    local printf_positions = find_printf_positions(content)
    local slf4j_positions = find_slf4j_positions(content)

    local placeholder_count, wrong_style

    if is_printf then
        placeholder_count = #printf_positions
        if #slf4j_positions > 0 and #printf_positions == 0 then
            wrong_style = "slf4j_in_printf"
        else
            wrong_style = false
        end
    elseif is_slf4j then
        placeholder_count = #slf4j_positions
        if #printf_positions > 0 and #slf4j_positions == 0 then
            wrong_style = "printf_in_slf4j"
        else
            wrong_style = false
        end
    else
        return nil
    end

    return {
        kind = is_printf and "printf" or "slf4j",
        fmt_node = fmt_node,
        content = content,
        printf_positions = printf_positions,
        slf4j_positions = slf4j_positions,
        placeholder_count = placeholder_count,
        arg_count = #format_arg_nodes,
        arg_nodes = format_arg_nodes,
        wrong_style = wrong_style,
    }
end

-- ---------------------------------------------------------------------------
--  Per-placeholder highlighting + diagnostics
-- ---------------------------------------------------------------------------

--- Map a content offset to buffer (row, col) for a format string node.
--- For string_literal (single-line): simple column offset.
--- For text_block (multi-line): walks content tracking newlines.
local function content_offset_to_bufpos(fmt_node, content, content_offset)
    local sr, sc = fmt_node:range()
    local node_type = fmt_node:type()

    if node_type == "string_literal" then
        -- Single-line: col = node_start_col + 1 (opening quote) + offset
        return sr, sc + 1 + content_offset
    end

    -- text_block: opening """ is on start row, content starts after first newline
    local raw_text = vim.treesitter.get_node_text(fmt_node, 0)
    local first_nl = raw_text:find("\n", 4)
    if not first_nl then
        return sr, sc + 3 + content_offset
    end

    -- Count newlines in content up to the offset to find the row
    local row = sr + 1 -- content starts one line after """
    local col = 0
    for i = 1, content_offset do
        if content:byte(i) == 10 then
            row = row + 1
            col = 0
        else
            col = col + 1
        end
    end
    return row, col
end

--- Process a single format call: set extmarks and collect diagnostics.
local function process_result(bufnr, result, diagnostics)
    local has_wrong_style = result.wrong_style and result.wrong_style ~= false

    -- Determine which positions to highlight and how
    local correct_positions, wrong_positions
    if result.kind == "printf" then
        correct_positions = result.printf_positions
        if has_wrong_style then
            wrong_positions = result.slf4j_positions
        end
    else
        correct_positions = result.slf4j_positions
        if has_wrong_style then
            wrong_positions = result.printf_positions
        end
    end

    -- Build diagnostic message (one per call, placed on first bad placeholder)
    local msg
    if has_wrong_style then
        if result.wrong_style == "slf4j_in_printf" then
            msg = "String.format uses %s/%d placeholders, not {}"
        else
            msg = "SLF4J log uses {} placeholders, not %s/%d"
        end
    elseif result.arg_count ~= result.placeholder_count then
        -- SLF4J throwable exception
        if result.kind == "slf4j" and result.arg_count == result.placeholder_count + 1 then
            msg = nil
        else
            msg = string.format(
                "format string expects %d argument(s) but %d provided",
                result.placeholder_count,
                result.arg_count
            )
        end
    end

    local diag_emitted = false

    -- Highlight wrong-style placeholders (all red)
    if wrong_positions then
        for _, pos in ipairs(wrong_positions) do
            local row, col = content_offset_to_bufpos(result.fmt_node, result.content, pos.offset)
            local end_col = col + pos.length
            vim.api.nvim_buf_set_extmark(bufnr, NS_HL, row, col, {
                end_row = row,
                end_col = end_col,
                hl_group = "JavaFormatBad",
            })
            if msg and not diag_emitted then
                diagnostics[#diagnostics + 1] = {
                    lnum = row,
                    col = col,
                    end_lnum = row,
                    end_col = end_col,
                    severity = vim.diagnostic.severity.WARN,
                    message = msg,
                    source = "java-format",
                }
                diag_emitted = true
            end
        end
    end

    -- Highlight correct-style placeholders: green if matched, red if excess
    for i, pos in ipairs(correct_positions) do
        local is_matched = (not has_wrong_style) and (i <= result.arg_count)
        local hl_group = is_matched and "JavaFormatOk" or "JavaFormatBad"
        local row, col = content_offset_to_bufpos(result.fmt_node, result.content, pos.offset)
        local end_col = col + pos.length
        vim.api.nvim_buf_set_extmark(bufnr, NS_HL, row, col, {
            end_row = row,
            end_col = end_col,
            hl_group = hl_group,
        })
        if hl_group == "JavaFormatBad" and msg and not diag_emitted then
            diagnostics[#diagnostics + 1] = {
                lnum = row,
                col = col,
                end_lnum = row,
                end_col = end_col,
                severity = vim.diagnostic.severity.WARN,
                message = msg,
                source = "java-format",
            }
            diag_emitted = true
        end
    end

    -- Too many args: placeholders are all green, highlight extra arg nodes red
    if not has_wrong_style and result.arg_count > result.placeholder_count then
        for i = result.placeholder_count + 1, #result.arg_nodes do
            local arg_node = result.arg_nodes[i]
            local sr, sc, er, ec = arg_node:range()
            vim.api.nvim_buf_set_extmark(bufnr, NS_HL, sr, sc, {
                end_row = er,
                end_col = ec,
                hl_group = "JavaFormatBad",
            })
            if msg and not diag_emitted then
                diagnostics[#diagnostics + 1] = {
                    lnum = sr,
                    col = sc,
                    end_lnum = er,
                    end_col = ec,
                    severity = vim.diagnostic.severity.WARN,
                    message = msg,
                    source = "java-format",
                }
                diag_emitted = true
            end
        end
    end
end

-- ---------------------------------------------------------------------------
--  Buffer scan
-- ---------------------------------------------------------------------------

local function scan_and_highlight(bufnr)
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
                process_result(bufnr, result, diagnostics)
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
    -- Clear previous highlights and diagnostics
    vim.api.nvim_buf_clear_namespace(bufnr, NS_HL, 0, -1)
    vim.diagnostic.set(NS, bufnr, scan_and_highlight(bufnr))
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
