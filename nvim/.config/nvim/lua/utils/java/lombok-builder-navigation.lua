-- Lombok navigation utilities: map Lombok-generated members back to the source
-- field or record component they were generated from.
--
-- jdtls (running with the Lombok agent) answers `textDocument/definition` for a
-- generated member with the annotation that produced it whenever it cannot map
-- the member to a field: every member of a library class opened from a sources
-- jar (jdt:// buffers), and in-project `@Builder`/`@SuperBuilder` setters
-- (including `@Singular` forms), `@With` withers and `@Accessors(fluent/prefix)`
-- accessors. Standard in-project getters/setters already land on the field.
-- Whenever a single definition lands on an annotation line, this module derives
-- the candidate field names from the invoked member name and moves the cursor to
-- the matching field of the annotated type (or to the annotated field itself).
--
-- - goto_definition - resolve member invocations and method references through LSP,
--   then redirect annotation hits to the matching source field

local M = {}

-- Members that are never backed by a field: builder lifecycle and Object methods.
local ignored_member_names = {
    build = true,
    builder = true,
    toBuilder = true,
    toString = true,
    equals = true,
    hashCode = true,
    canEqual = true,
}

-- Lombok accessor prefixes whose remainder is the capitalized field name.
local accessor_prefixes = { "get", "is", "set", "with" }

-- Keywords that open a type body; `record` is treated separately because its
-- components live in the header parentheses.
local type_keywords = { "class", "interface", "enum", "record" }

--- Return one character from a 1-indexed string position.
---@param line string
---@param idx integer
---@return string
local function char_at(line, idx)
    if idx < 1 or idx > #line then
        return ""
    end
    return line:sub(idx, idx)
end

--- Check whether a character can belong to a Java identifier.
---@param ch string
---@return boolean
local function is_identifier_char(ch)
    return ch:match("[%w_$]") ~= nil
end

--- Move across whitespace in one direction from a string index.
---@param line string
---@param idx integer
---@param step integer
---@return integer
local function skip_spaces(line, idx, step)
    while idx >= 1 and idx <= #line and char_at(line, idx):match("%s") do
        idx = idx + step
    end
    return idx
end

--- Check whether a Java identifier is builder-related.
---@param name string
---@return boolean
local function is_builder_related_identifier(name)
    return name:lower():find("builder", 1, true) ~= nil
end

--- Return the receiver of a `receiver::method` reference whose method starts at the given index.
--- The receiver name is empty for expression receivers such as `v.toBuilder()::iban`.
---@param line string
---@param method_start_idx integer
---@return string|nil receiver_name, integer|nil receiver_byte_col Zero-based byte column of the receiver.
local function method_reference_receiver(line, method_start_idx)
    local separator_end_idx = skip_spaces(line, method_start_idx - 1, -1)
    if char_at(line, separator_end_idx) ~= ":" or char_at(line, separator_end_idx - 1) ~= ":" then
        return nil, nil
    end

    local receiver_end_idx = skip_spaces(line, separator_end_idx - 2, -1)
    local receiver_start_idx = receiver_end_idx
    while receiver_start_idx > 1 and is_identifier_char(char_at(line, receiver_start_idx - 1)) do
        receiver_start_idx = receiver_start_idx - 1
    end

    return line:sub(receiver_start_idx, receiver_end_idx), receiver_start_idx - 1
end

---@class java.LombokNavigationTarget
---@field member_name string Invoked member name (builder setter, accessor, wither, method reference).
---@field row integer Zero-based row of the member under the cursor.
---@field byte_col integer Zero-based byte column inside the member name used for the LSP request.
---@field receiver_row? integer Zero-based row of a builder method-reference receiver.
---@field receiver_byte_col? integer Zero-based byte column of a builder method-reference receiver.

--- Return the Java member invocation or method reference under the cursor.
--- Claims `receiver.member(` invocations and `receiver::member` references; the
--- receiver position is kept for builder-named receivers as a resolution fallback.
---@return java.LombokNavigationTarget|nil
local function get_navigation_target_under_cursor()
    local lnum, col = unpack(vim.api.nvim_win_get_cursor(0))
    local line = vim.api.nvim_get_current_line()
    local cursor_idx = col + 1

    if not is_identifier_char(char_at(line, cursor_idx)) then
        return nil
    end

    local start_idx = cursor_idx
    while start_idx > 1 and is_identifier_char(char_at(line, start_idx - 1)) do
        start_idx = start_idx - 1
    end

    local end_idx = cursor_idx
    while end_idx < #line and is_identifier_char(char_at(line, end_idx + 1)) do
        end_idx = end_idx + 1
    end

    local name = line:sub(start_idx, end_idx)
    if name == "" or ignored_member_names[name] then
        return nil
    end

    -- Resolve at the last character of the name: jdtls attributes the first character
    -- of a `receiver::member` name to the whole method-reference expression and answers
    -- with the functional interface method (`Function.apply`) instead of the member.
    -- Single-character names use the position right after the name for the same reason.
    local request_byte_col = end_idx > start_idx and end_idx - 1 or end_idx

    ---@type java.LombokNavigationTarget
    local target = { member_name = name, row = lnum - 1, byte_col = request_byte_col }

    local receiver_name, receiver_byte_col = method_reference_receiver(line, start_idx)
    if receiver_name then
        if receiver_name ~= "" and is_builder_related_identifier(receiver_name) then
            target.receiver_row = lnum - 1
            target.receiver_byte_col = receiver_byte_col
        end
        return target
    end

    local prev_idx = skip_spaces(line, start_idx - 1, -1)
    if char_at(line, prev_idx) ~= "." then
        return nil
    end

    local next_idx = skip_spaces(line, end_idx + 1, 1)
    if char_at(line, next_idx) ~= "(" then
        return nil
    end

    return target
end

--- Return the index of the closing quote of the string or char literal opening at `idx`.
--- Falls back to the end of the line for unterminated literals.
---@param line string
---@param idx integer
---@return integer
local function literal_end(line, idx)
    local quote = char_at(line, idx)
    local cursor = idx + 1
    while cursor <= #line do
        local ch = char_at(line, cursor)
        if ch == "\\" then
            cursor = cursor + 2
        elseif ch == quote then
            return cursor
        else
            cursor = cursor + 1
        end
    end
    return #line
end

--- Return the net balance of a bracket pair on one source line, ignoring string and
--- char literals and anything after a `//` comment.
---@param line string
---@param open string
---@param close string
---@return integer
local function bracket_delta(line, open, close)
    local delta = 0
    local idx = 1
    while idx <= #line do
        local ch = char_at(line, idx)
        if ch == '"' or ch == "'" then
            idx = literal_end(line, idx)
        elseif ch == "/" and char_at(line, idx + 1) == "/" then
            break
        elseif ch == open then
            delta = delta + 1
        elseif ch == close then
            delta = delta - 1
        end
        idx = idx + 1
    end
    return delta
end

--- Return the net brace balance for one source line.
---@param line string
---@return integer
local function brace_delta(line)
    return bracket_delta(line, "{", "}")
end

--- Check whether a trimmed line is a line or block comment.
---@param code string
---@return boolean
local function is_comment_line(code)
    return code:match("^//") ~= nil or code:match("^/%*") ~= nil or code:match("^%*") ~= nil
end

---@class java.FieldDeclaration
---@field name string
---@field lnum integer
---@field col integer Zero-based byte column of the field name.

--- Return the field declared on one type-body line, or nil for methods, enum
--- constants, nested types and comments. The field name is the last identifier
--- before the first `=` or `;` outside literals and annotation arguments; any other
--- parenthesis at depth zero marks a method or an enum constant.
---@param line string
---@param lnum integer
---@return java.FieldDeclaration|nil
local function declared_field(line, lnum)
    local code = line:match("^%s*(.-)%s*$")
    if code == "" or is_comment_line(code) then
        return nil
    end

    local paren_depth = 0
    local last_name, last_start_idx
    local after_annotation_name = false
    local idx = 1

    while idx <= #line do
        local ch = char_at(line, idx)

        if ch == '"' or ch == "'" then
            idx = literal_end(line, idx)
        elseif ch == "/" and char_at(line, idx + 1) == "/" then
            break
        elseif ch == "@" then
            local _, name_end_idx = line:find("^[%a_$][%w_$%.]*", idx + 1)
            idx = name_end_idx or idx
            after_annotation_name = true
        elseif ch == "(" then
            if paren_depth == 0 and not after_annotation_name then
                return nil
            end
            paren_depth = paren_depth + 1
            after_annotation_name = false
        elseif ch == ")" then
            paren_depth = math.max(paren_depth - 1, 0)
        elseif paren_depth == 0 then
            if ch == "=" or ch == ";" then
                if not last_name then
                    return nil
                end
                return { name = last_name, lnum = lnum, col = last_start_idx - 1 }
            elseif is_identifier_char(ch) then
                local _, name_end_idx = line:find("^[%w_$]+", idx)
                last_name = line:sub(idx, name_end_idx)
                last_start_idx = idx
                idx = name_end_idx
                after_annotation_name = false
            elseif not ch:match("%s") then
                after_annotation_name = false
            end
        end

        idx = idx + 1
    end

    return nil
end

--- Lowercase the first character of a name.
---@param name string
---@return string
local function lower_first(name)
    return name:sub(1, 1):lower() .. name:sub(2)
end

--- Return the field names a Lombok-generated member may have been derived from, most likely first.
--- `getIban`/`isActive`/`setIban`/`withIban` strip their accessor prefix (keeping the
--- `isX` boolean field spelling); any other name is a builder setter or fluent accessor
--- and also tries the plural spellings a `@Singular` builder method derives from.
---@param member_name string
---@return string[]
local function field_name_candidates(member_name)
    local candidates = { member_name }
    local seen = { [member_name] = true }

    local function add(name)
        if name ~= "" and not seen[name] then
            seen[name] = true
            table.insert(candidates, name)
        end
    end

    for _, prefix in ipairs(accessor_prefixes) do
        local rest = member_name:match("^" .. prefix .. "(%u[%w_$]*)$")
        if rest then
            add(lower_first(rest))
            add(rest)
            add("is" .. rest)
            return candidates
        end
    end

    add(member_name .. "s")
    add(member_name .. "es")
    if member_name:match("y$") then
        add(member_name:sub(1, -2) .. "ies")
    end
    return candidates
end

--- Strip an `@Accessors(prefix = ...)`-style prefix (`mName`, `_name`, `m_name`) from a field name.
--- Returns nil when the name has no such prefix.
---@param field_name string
---@return string|nil
local function strip_accessor_prefix(field_name)
    local stripped = field_name:match("^[%l$]*[_$]*(.*)$")
    if not stripped or stripped == "" or stripped == field_name then
        return nil
    end
    return stripped
end

--- Pick the field matching one of the candidate names: exact candidates in priority
--- order first, then a relaxed pass that ignores an `@Accessors` prefix and letter case.
---@param fields java.FieldDeclaration[]
---@param candidates string[]
---@return integer|nil lnum, integer|nil col
local function match_field(fields, candidates)
    for _, candidate in ipairs(candidates) do
        for _, field in ipairs(fields) do
            if field.name == candidate then
                return field.lnum, field.col
            end
        end
    end

    local relaxed = {}
    for _, candidate in ipairs(candidates) do
        relaxed[candidate:lower()] = true
    end
    for _, field in ipairs(fields) do
        local stripped = strip_accessor_prefix(field.name)
        if stripped and relaxed[stripped:lower()] then
            return field.lnum, field.col
        end
    end

    return nil, nil
end

--- Strip leading annotations (`@Name`, `@Name.Nested(args)`) from a line and return the trimmed rest.
--- Arguments that are not balanced on the line are left in place, starting with `(`.
---@param line string
---@return string
local function strip_leading_annotations(line)
    local code = line
    while true do
        local rest = code:match("^%s*@[%a_$][%w_$%.]*%s*%b()(.*)$") or code:match("^%s*@[%a_$][%w_$%.]*(.*)$")
        if not rest then
            return (code:match("^%s*(.-)%s*$"))
        end
        code = rest
    end
end

--- Return the kind of type a declaration line opens, or nil for members.
---@param code string Declaration code with leading annotations stripped.
---@return "class"|"record"|nil
local function declared_type_kind(code)
    for _, keyword in ipairs(type_keywords) do
        if code:match("^[%w_%s%-]*%f[%w_]" .. keyword .. "%s+[%a_$]") then
            return keyword == "record" and "record" or "class"
        end
    end
    return nil
end

---@class java.AnnotatedDeclaration
---@field kind "class"|"record"|"member" `class` covers class, interface and enum bodies.
---@field lnum integer

--- Find the declaration an annotation line belongs to: the type whose body follows the
--- annotation block, or the member (field) annotated on its own.
---@param lines string[]
---@param annotation_lnum integer
---@return java.AnnotatedDeclaration|nil
local function find_annotated_declaration(lines, annotation_lnum)
    local paren_depth = 0

    for lnum = annotation_lnum, #lines do
        local line = lines[lnum]
        if paren_depth > 0 then
            paren_depth = paren_depth + bracket_delta(line, "(", ")")
        else
            local code = strip_leading_annotations(line)
            if code:sub(1, 1) == "(" then
                paren_depth = bracket_delta(code, "(", ")")
            elseif code ~= "" and not is_comment_line(code) then
                return { kind = declared_type_kind(code) or "member", lnum = lnum }
            end
        end
    end

    return nil
end

--- Check whether a character is escaped by an odd number of backslashes.
---@param line string
---@param idx integer
---@return boolean
local function is_escaped_char(line, idx)
    local slash_count = 0
    local cursor = idx - 1
    while cursor >= 1 and char_at(line, cursor) == "\\" do
        slash_count = slash_count + 1
        cursor = cursor - 1
    end
    return slash_count % 2 == 1
end

--- Find the opening parenthesis of a Java record component list.
---@param line string
---@return integer|nil
local function record_open_paren(line)
    local record_start = line:find("%f[%w_]record%s+")
    if not record_start then
        return nil
    end
    return line:find("%(", record_start)
end

--- Return the final identifier in a record component segment when it matches the requested field.
---@param identifiers { name: string, lnum: integer, col: integer }[]
---@param field_name string
---@return integer|nil, integer|nil
local function record_component_from_identifiers(identifiers, field_name)
    local candidate = identifiers[#identifiers]
    if candidate and candidate.name == field_name then
        return candidate.lnum, candidate.col
    end
    return nil, nil
end

--- Find a record component by name, ignoring annotation arguments and generic type commas.
---@param lines string[]
---@param field_name string
---@param record_lnum integer
---@return integer|nil, integer|nil
local function find_record_component(lines, field_name, record_lnum)
    local paren_depth = 0
    local angle_depth = 0
    local in_string = false
    local identifiers = {}

    for lnum = record_lnum, #lines do
        local line = lines[lnum]
        local idx = 1

        if paren_depth == 0 then
            local open_paren = record_open_paren(line)
            if not open_paren then
                return nil, nil
            end
            paren_depth = 1
            idx = open_paren + 1
        end

        while idx <= #line do
            local ch = char_at(line, idx)

            if in_string then
                if ch == '"' and not is_escaped_char(line, idx) then
                    in_string = false
                end
            elseif ch == '"' then
                in_string = true
            elseif ch == "<" then
                angle_depth = angle_depth + 1
            elseif ch == ">" and angle_depth > 0 then
                angle_depth = angle_depth - 1
            elseif ch == "(" then
                paren_depth = paren_depth + 1
            elseif ch == ")" then
                if paren_depth == 1 then
                    return record_component_from_identifiers(identifiers, field_name)
                end
                paren_depth = paren_depth - 1
            elseif ch == "," and paren_depth == 1 and angle_depth == 0 then
                local found_lnum, found_col = record_component_from_identifiers(identifiers, field_name)
                if found_lnum then
                    return found_lnum, found_col
                end
                identifiers = {}
            elseif is_identifier_char(ch) then
                local start_idx = idx
                while idx <= #line and is_identifier_char(char_at(line, idx)) do
                    idx = idx + 1
                end
                table.insert(identifiers, {
                    name = line:sub(start_idx, idx - 1),
                    lnum = lnum,
                    col = start_idx - 1,
                })
                idx = idx - 1
            end

            idx = idx + 1
        end
    end

    return nil, nil
end

--- Collect the fields declared directly in the body of the type opened at `type_lnum`.
--- Only lines one brace level inside the type and outside multi-line parentheses
--- (annotation arguments, initializer calls, method signatures) are considered, so
--- nested types and continuation lines are skipped.
---@param lines string[]
---@param type_lnum integer
---@return java.FieldDeclaration[]
local function collect_type_fields(lines, type_lnum)
    local type_depth = 0
    for idx = 1, type_lnum - 1 do
        type_depth = type_depth + brace_delta(lines[idx] or "")
    end

    local fields = {}
    local depth = type_depth
    local paren_depth = 0
    local entered_body = false

    for idx = type_lnum, #lines do
        local line = lines[idx]
        if idx > type_lnum and depth == type_depth + 1 and paren_depth == 0 then
            local field = declared_field(line, idx)
            if field then
                table.insert(fields, field)
            end
        end
        paren_depth = math.max(paren_depth + bracket_delta(line, "(", ")"), 0)

        depth = depth + brace_delta(line)
        if depth > type_depth then
            entered_body = true
        elseif entered_body then
            break
        end
    end

    return fields
end

--- Find the field a Lombok member maps to inside the type (or on the field) an annotation belongs to.
---@param bufnr integer
---@param candidates string[] Field name candidates, most likely first.
---@param annotation_lnum integer
---@return integer|nil lnum, integer|nil col
local function find_field_for_annotation(bufnr, candidates, annotation_lnum)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local declaration = find_annotated_declaration(lines, annotation_lnum)
    if not declaration then
        return nil, nil
    end

    if declaration.kind == "record" then
        for _, candidate in ipairs(candidates) do
            local lnum, col = find_record_component(lines, candidate, declaration.lnum)
            if lnum then
                return lnum, col
            end
        end
        return nil, nil
    end

    if declaration.kind == "member" then
        local field = declared_field(lines[declaration.lnum], declaration.lnum)
        return match_field(field and { field } or {}, candidates)
    end

    return match_field(collect_type_fields(lines, declaration.lnum), candidates)
end

--- Jump from a Lombok annotation the definition landed on to the matching source field.
--- Does nothing when the cursor is not on an annotation line or no field matches.
---@param win integer
---@param bufnr integer
---@param member_name string
---@return boolean
local function jump_from_annotation_to_field(win, bufnr, member_name)
    if not vim.api.nvim_win_is_valid(win) or not vim.api.nvim_buf_is_valid(bufnr) then
        return false
    end

    local cursor = vim.api.nvim_win_get_cursor(win)
    local lnum = cursor[1]
    local line = vim.api.nvim_buf_get_lines(bufnr, lnum - 1, lnum, false)[1] or ""
    if not line:match("^%s*@[%a_$]") then
        return false
    end

    local field_lnum, col = find_field_for_annotation(bufnr, field_name_candidates(member_name), lnum)
    if not field_lnum then
        return false
    end

    vim.api.nvim_win_set_cursor(win, { field_lnum, col or 0 })
    vim.api.nvim_win_call(win, function()
        vim.cmd("normal! zv")
        vim.cmd("normal! zz")
    end)
    return true
end

--- Apply an LSP definition result and refine Lombok annotation hits to source fields.
---@param options vim.lsp.LocationOpts.OnList
---@param member_name string
---@param win integer
---@param tagname string
---@param from table
local function apply_definition_list(options, member_name, win, tagname, from)
    local items = options.items or {}
    if vim.tbl_isempty(items) then
        vim.notify("No locations found", vim.log.levels.INFO)
        return
    end

    if #items ~= 1 then
        vim.fn.setqflist({}, " ", options)
        vim.cmd("botright copen")
        return
    end

    local item = items[1]
    local bufnr = item.bufnr or vim.fn.bufadd(item.filename)

    vim.cmd("normal! m'")
    vim.fn.settagstack(vim.fn.win_getid(win), { items = { { tagname = tagname, from = from } } }, "t")

    vim.bo[bufnr].buflisted = true
    vim.api.nvim_win_set_buf(win, bufnr)
    vim.api.nvim_win_set_cursor(win, { item.lnum, item.col - 1 })
    vim.api.nvim_win_call(win, function()
        vim.cmd("normal! zv")
    end)

    jump_from_annotation_to_field(win, bufnr, member_name)
end

--- Request locations for an LSP method at an explicit buffer position and pass the
--- converted items to a list handler (with an empty list when nothing was found).
---@param bufnr integer
---@param method "textDocument/definition"|"textDocument/typeDefinition"
---@param row integer
---@param byte_col integer
---@param on_list fun(options: vim.lsp.LocationOpts.OnList)
local function request_locations_at(bufnr, method, row, byte_col, on_list)
    local clients = vim.lsp.get_clients({ bufnr = bufnr, method = method })
    if vim.tbl_isempty(clients) then
        on_list({ title = "LSP locations", items = {}, context = { bufnr = bufnr, method = method } })
        return
    end

    vim.lsp.buf_request_all(bufnr, method, function(client)
        local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1] or ""
        return {
            textDocument = vim.lsp.util.make_text_document_params(bufnr),
            position = {
                line = row,
                character = vim.str_utfindex(line, client.offset_encoding, byte_col, false),
            },
        }
    end, function(results)
        local items = {}

        for client_id, response in pairs(results) do
            local client = vim.lsp.get_client_by_id(client_id)
            local result = response and response.result
            if client and result then
                local locations = (result.uri or result.targetUri) and { result } or result
                vim.list_extend(items, vim.lsp.util.locations_to_items(locations, client.offset_encoding))
            end
        end

        on_list({
            title = "LSP locations",
            items = items,
            context = { bufnr = bufnr, method = method },
        })
    end)
end

--- Navigate from a Lombok member invocation or method reference to its source field.
--- Resolves the member through `textDocument/definition`; a builder method reference
--- that jdtls cannot resolve falls back to the receiver's type definition. Whenever
--- the single resulting location is a Lombok annotation, the cursor is redirected to
--- the matching source field or record component.
---@param ctx lang.LspNavigationContext
---@return boolean
function M.goto_definition(ctx)
    if ctx.filetype ~= "java" then
        return false
    end

    local target = get_navigation_target_under_cursor()
    if not target then
        return false
    end

    local win = vim.api.nvim_get_current_win()
    local from = vim.fn.getpos(".")
    from[1] = ctx.bufnr
    local tagname = target.member_name

    local function on_list(options)
        apply_definition_list(options, target.member_name, win, tagname, from)
    end

    request_locations_at(ctx.bufnr, "textDocument/definition", target.row, target.byte_col, function(options)
        if vim.tbl_isempty(options.items or {}) and target.receiver_row and target.receiver_byte_col then
            request_locations_at(
                ctx.bufnr,
                "textDocument/typeDefinition",
                target.receiver_row,
                target.receiver_byte_col,
                on_list
            )
            return
        end
        on_list(options)
    end)

    return true
end

return M
