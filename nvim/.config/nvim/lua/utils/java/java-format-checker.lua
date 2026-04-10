local M = {}

-- Namespaces: diagnostics (messages) and highlight extmarks (green/red coloring)
local NS = vim.api.nvim_create_namespace("java_format_check")
local NS_HL = vim.api.nvim_create_namespace("java_format_check_hl")

-- Highlight groups (default = true → user can override)
vim.api.nvim_set_hl(0, "JavaFormatOk", { default = true, link = "DiagnosticOk" })
vim.api.nvim_set_hl(0, "JavaFormatBad", { default = true, link = "DiagnosticError" })

-- ---------------------------------------------------------------------------
--  Constants & cached state
-- ---------------------------------------------------------------------------

local LOG_METHODS = { debug = true, info = true, warn = true, error = true, trace = true }

-- Punctuation node types to skip when collecting argument nodes
local PUNCT = { ["("] = true, [")"] = true, [","] = true }

local STRING_NODES = { string_literal = true, text_block = true }

-- Treesitter query — parsed once and cached
local QUERY_STR = [[
    (method_invocation
        name: (identifier) @_name
        (#match? @_name "^(format|formatted|debug|info|warn|error|trace)$")
    ) @call
]]
local query_cache, call_capture_id

-- Byte constants
local BYTE_PERCENT = 0x25 -- '%'
local BYTE_BACKSLASH = 0x5C -- '\'
local BYTE_LBRACE = 0x7B -- '{'
local BYTE_RBRACE = 0x7D -- '}'
local BYTE_NL = 0x0A -- '\n'

-- Reusable API refs (avoid repeated global lookups in hot paths)
local api = vim.api
local ts = vim.treesitter
local get_node_text = ts.get_node_text
local buf_set_extmark = api.nvim_buf_set_extmark
local buf_is_valid = api.nvim_buf_is_valid
local buf_is_loaded = api.nvim_buf_is_loaded
local diag_set = vim.diagnostic.set
local WARN = vim.diagnostic.severity.WARN

-- ---------------------------------------------------------------------------
--  Query cache
-- ---------------------------------------------------------------------------

local function get_query()
    if query_cache then
        return query_cache, call_capture_id
    end
    local ok, q = pcall(ts.query.parse, "java", QUERY_STR)
    if not ok or not q then
        return nil, nil
    end
    for i, name in ipairs(q.captures) do
        if name == "call" then
            call_capture_id = i
            break
        end
    end
    if not call_capture_id then
        return nil, nil
    end
    query_cache = q
    return query_cache, call_capture_id
end

-- ---------------------------------------------------------------------------
--  Placeholder position finding
-- ---------------------------------------------------------------------------

--- Find byte positions of printf-style placeholders (%s, %d, %02d, %1$s …).
--- Returns list of { offset, length } (0-based offsets into content). Skips %%.
local function find_printf_positions(s)
    local positions = {}
    local n = #s
    local i = 1
    while i <= n do
        if s:byte(i) == BYTE_PERCENT then
            if i < n and s:byte(i + 1) == BYTE_PERCENT then
                i = i + 2 -- skip %%
            else
                -- Match: % [flags] [width] [.precision] [arg_index$] [flags] [width] [.prec] conversion
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

--- Find byte positions of SLF4J {} placeholders. Skips \{}.
local function find_slf4j_positions(s)
    local positions = {}
    local n = #s
    local i = 1
    while i <= n do
        local b = s:byte(i)
        if b == BYTE_BACKSLASH and i + 2 <= n and s:byte(i + 1) == BYTE_LBRACE and s:byte(i + 2) == BYTE_RBRACE then
            i = i + 3 -- skip \{}
        elseif b == BYTE_LBRACE and i < n and s:byte(i + 1) == BYTE_RBRACE then
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

--- Collect non-punctuation children of an argument_list node.
local function collect_arg_nodes(arg_list)
    local args = {}
    for child in arg_list:iter_children() do
        if not PUNCT[child:type()] then
            args[#args + 1] = child
        end
    end
    return args
end

--- Return the first string_literal / text_block child of an argument_list.
local function first_string_arg(arg_list)
    for child in arg_list:iter_children() do
        if STRING_NODES[child:type()] then
            return child
        end
    end
    return nil
end

--- Decompose method_invocation → (object_node, method_name_str, argument_list_node).
--- Uses child iteration (no node:field) for maximum grammar-version compatibility.
local function decompose_invocation(node, bufnr)
    local obj_node, method_name, arg_list
    local prev_child = nil
    for child in node:iter_children() do
        local t = child:type()
        if t == "identifier" then
            -- Last identifier before argument_list = method name; earlier one = object
            if method_name then
                obj_node = prev_child
            end
            method_name = get_node_text(child, bufnr)
            prev_child = child
        elseif t == "argument_list" then
            arg_list = child
        elseif t ~= "." and t ~= "type_arguments" and not PUNCT[t] then
            obj_node = child
        end
    end
    return obj_node, method_name, arg_list
end

-- ---------------------------------------------------------------------------
--  String content extraction
-- ---------------------------------------------------------------------------

--- Strip surrounding quotes from a string node's raw text.
--- Returns (content, quote_prefix_len).
---   string_literal  "…"   → (inner, 1)
---   text_block      """…""" → (inner after first newline, offset to content start)
local function strip_quotes(raw_text)
    if raw_text:byte(1) == 0x22 and raw_text:byte(2) == 0x22 and raw_text:byte(3) == 0x22 then
        -- Text block: strip opening """ + first newline, and trailing """
        local after_open = raw_text:sub(4)
        local nl = after_open:find("\n", 1, true)
        if nl then
            local content = after_open:sub(nl + 1)
            content = content:gsub('"""%s*$', "")
            return content, 3 + nl -- bytes before content start in raw text
        end
        -- Degenerate text block without newline
        local content = after_open:gsub('"""%s*$', "")
        return content, 3
    end
    -- Regular string literal
    return raw_text:sub(2, -2), 1
end

-- ---------------------------------------------------------------------------
--  Call classification
-- ---------------------------------------------------------------------------

--- Classify a method_invocation node.
--- Returns result table or nil (not a format/log call, or no string literal found).
local function classify_invocation(node, bufnr)
    local obj_node, method_name, arg_list = decompose_invocation(node, bufnr)
    if not method_name or not arg_list then
        return nil
    end

    local fmt_node, is_printf, is_slf4j, fmt_is_first_arg

    -- Case A: "…".formatted(args…)
    if method_name == "formatted" then
        if obj_node and STRING_NODES[obj_node:type()] then
            fmt_node = obj_node
            is_printf = true
        end
    -- Case B: String.format(fmt, args…)
    elseif method_name == "format" then
        if obj_node and get_node_text(obj_node, bufnr) == "String" then
            fmt_node = first_string_arg(arg_list)
            is_printf = true
            fmt_is_first_arg = true
        end
    -- Case C: log.debug/info/warn/error/trace(fmt, args…)
    elseif LOG_METHODS[method_name] then
        fmt_node = first_string_arg(arg_list)
        is_slf4j = true
        fmt_is_first_arg = true
    end

    if not fmt_node then
        return nil
    end

    -- Extract content and placeholder positions
    local raw_text = get_node_text(fmt_node, bufnr)
    if not raw_text or #raw_text < 2 then
        return nil
    end

    local content, quote_len = strip_quotes(raw_text)
    local printf_pos = find_printf_positions(content)
    local slf4j_pos = find_slf4j_positions(content)

    -- Build format-argument node list (excluding format string itself)
    local all_args = collect_arg_nodes(arg_list)
    local arg_start = fmt_is_first_arg and 2 or 1
    local arg_count = #all_args - arg_start + 1

    local placeholder_count, wrong_style
    if is_printf then
        placeholder_count = #printf_pos
        wrong_style = #slf4j_pos > 0 and "slf4j_in_printf" or false
    elseif is_slf4j then
        placeholder_count = #slf4j_pos
        wrong_style = #printf_pos > 0 and "printf_in_slf4j" or false
    else
        return nil
    end

    return {
        kind = is_printf and "printf" or "slf4j",
        fmt_node = fmt_node,
        content = content,
        quote_len = quote_len,
        printf_pos = printf_pos,
        slf4j_pos = slf4j_pos,
        placeholder_count = placeholder_count,
        arg_count = arg_count,
        arg_start = arg_start,
        all_args = all_args,
        wrong_style = wrong_style,
    }
end

-- ---------------------------------------------------------------------------
--  Buffer-position mapping
-- ---------------------------------------------------------------------------

--- Map a content byte-offset to buffer (row, col) for a format string node.
--- For string_literal: single-line, pure arithmetic.
--- For text_block: walks content bytes up to the offset (rare path).
local function content_to_bufpos(sr, sc, node_type, content, quote_len, offset)
    if node_type ~= "text_block" then
        return sr, sc + quote_len + offset
    end
    -- text_block multi-line: count newlines before the offset
    local row = sr + 1 -- content starts one line after opening """
    local col = 0
    for i = 1, offset do
        if content:byte(i) == BYTE_NL then
            row = row + 1
            col = 0
        else
            col = col + 1
        end
    end
    return row, col
end

-- ---------------------------------------------------------------------------
--  Per-placeholder highlighting + diagnostic emission
-- ---------------------------------------------------------------------------

--- Emit a single diagnostic entry at (row, col)→(row, end_col).
local function emit_diag(diagnostics, row, col, end_col, msg)
    diagnostics[#diagnostics + 1] = {
        lnum = row,
        col = col,
        end_lnum = row,
        end_col = end_col,
        severity = WARN,
        message = msg,
        source = "java-format",
    }
end

--- Set a highlight extmark at the given range.
local function set_hl(bufnr, row, col, end_col, hl_group)
    buf_set_extmark(bufnr, NS_HL, row, col, {
        end_row = row,
        end_col = end_col,
        hl_group = hl_group,
    })
end

--- Process one classified format call: set extmarks and collect diagnostics.
local function process_result(bufnr, result, diagnostics)
    local has_wrong = result.wrong_style and result.wrong_style ~= false
    local sr, sc = result.fmt_node:range()
    local node_type = result.fmt_node:type()
    local content = result.content
    local ql = result.quote_len

    -- Choose correct-kind and wrong-kind position lists
    local correct_pos, wrong_pos
    if result.kind == "printf" then
        correct_pos = result.printf_pos
        wrong_pos = #result.slf4j_pos > 0 and result.slf4j_pos or nil
    else
        correct_pos = result.slf4j_pos
        wrong_pos = #result.printf_pos > 0 and result.printf_pos or nil
    end
    local all_wrong = has_wrong and #correct_pos == 0

    -- Build diagnostic message (at most one per call)
    local msg
    if has_wrong then
        msg = result.wrong_style == "slf4j_in_printf" and "String.format uses %s/%d placeholders, not {}"
            or "SLF4J log uses {} placeholders, not %s/%d"
    elseif result.arg_count ~= result.placeholder_count then
        -- SLF4J throwable tolerance: last arg may be a Throwable
        if not (result.kind == "slf4j" and result.arg_count == result.placeholder_count + 1) then
            msg = string.format(
                "format string expects %d argument(s) but %d provided",
                result.placeholder_count,
                result.arg_count
            )
        end
    end

    local diag_done = false

    -- 1. Wrong-kind placeholders → always red
    if wrong_pos then
        for _, pos in ipairs(wrong_pos) do
            local row, col = content_to_bufpos(sr, sc, node_type, content, ql, pos.offset)
            local ec = col + pos.length
            set_hl(bufnr, row, col, ec, "JavaFormatBad")
            if msg and not diag_done then
                emit_diag(diagnostics, row, col, ec, msg)
                diag_done = true
            end
        end
    end

    -- 2. Correct-kind placeholders → green if matched, red if excess
    for i, pos in ipairs(correct_pos) do
        local matched = i <= result.arg_count
        local hl = matched and "JavaFormatOk" or "JavaFormatBad"
        local row, col = content_to_bufpos(sr, sc, node_type, content, ql, pos.offset)
        local ec = col + pos.length
        set_hl(bufnr, row, col, ec, hl)
        if not matched and msg and not diag_done then
            emit_diag(diagnostics, row, col, ec, msg)
            diag_done = true
        end
    end

    -- 3. Too many args → highlight extra arg nodes red
    --    Skip when all placeholders are wrong-kind (the style is wrong, args aren't truly extra).
    --    SLF4J throwable tolerance: +1 arg is okay.
    local is_throwable = result.kind == "slf4j" and result.arg_count == result.placeholder_count + 1
    if not all_wrong and not is_throwable and result.arg_count > result.placeholder_count then
        local first_extra = result.arg_start + result.placeholder_count
        for i = first_extra, #result.all_args do
            local arg_node = result.all_args[i]
            local asr, asc, aer, aec = arg_node:range()
            buf_set_extmark(bufnr, NS_HL, asr, asc, {
                end_row = aer,
                end_col = aec,
                hl_group = "JavaFormatBad",
            })
            if msg and not diag_done then
                emit_diag(diagnostics, asr, asc, aec, msg)
                diag_done = true
            end
        end
    end
end

-- ---------------------------------------------------------------------------
--  Buffer scan
-- ---------------------------------------------------------------------------

local function scan_and_highlight(bufnr)
    local ok, parser = pcall(ts.get_parser, bufnr, "java")
    if not ok or not parser then
        return {}
    end
    local trees = parser:parse()
    if not trees or not trees[1] then
        return {}
    end
    local root = trees[1]:root()

    local query, cid = get_query()
    if not query then
        return {}
    end

    local diagnostics = {}
    for id, node in query:iter_captures(root, bufnr, 0, -1) do
        if id == cid then
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
    bufnr = bufnr or api.nvim_get_current_buf()
    if not buf_is_valid(bufnr) or not buf_is_loaded(bufnr) then
        return
    end
    api.nvim_buf_clear_namespace(bufnr, NS_HL, 0, -1)
    diag_set(NS, bufnr, scan_and_highlight(bufnr))
end

function M.setup()
    api.nvim_create_autocmd("BufWritePost", {
        group = api.nvim_create_augroup("java_format_check", { clear = true }),
        pattern = "*.java",
        callback = function(ev)
            M.apply(ev.buf)
        end,
    })
end

return M
