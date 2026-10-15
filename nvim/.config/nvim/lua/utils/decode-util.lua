-- Decode base64 / JWT text from the visual selection into a bottom scratch split.
--
-- • decode_base64 — lenient base64 decode (standard + URL-safe alphabet, unpadded, line-wrapped)
-- • decode_jwt — split a compact JWS/JWE and decode its JSON header/payload
-- • decode — classify cleaned text as "jwt" / "jwe" / "base64" and decode it
-- • time_claim_lines — human-readable `iat` / `nbf` / `exp` comment lines
-- • render — lines + filetype shown for a decode result
-- • decode_selection — decode the last visual selection into a scratch split (`q` closes it)

local M = {}

local SPLIT_HEIGHT_RATIO = 0.5
local TIME_CLAIMS = { "iat", "nbf", "exp", "auth_time" }
local JWT_PART = "[A-Za-z0-9_%-]"
local JWS_PATTERN = ("^(%s+)%%.(%s+)%%.(%s*)$"):format(JWT_PART, JWT_PART, JWT_PART)
local JWE_PATTERN = ("^(%s+)%%.%s*%%.%s*%%.%s*%%.%s*$"):format(JWT_PART, JWT_PART, JWT_PART, JWT_PART, JWT_PART)

---@class DecodeResult
---@field kind "jwt"|"jwe"|"base64"
---@field header? table decoded JWT header (jwt / jwe)
---@field header_text? string raw JSON text of the header (jwt / jwe)
---@field payload? table decoded JWT payload when it is JSON (jwt)
---@field payload_text? string raw payload text, nil when not valid base64 (jwt)
---@field signature? string base64url signature, may be empty (jwt)
---@field decoded? string decoded bytes (base64)

--- Append all items to the target list.
---@param target string[]
---@param items string[]
local function extend(target, items)
    for _, item in ipairs(items) do
        table.insert(target, item)
    end
end

--- Join line-wrapped text (PEM-style base64, tokens wrapped in logs): drop line breaks and the indentation around them.
--- Spaces inside a line are kept on purpose, so prose such as "plain words" is not mistaken for base64.
---@param text string
---@return string
local function join_wrapped_lines(text)
    return (vim.trim(text):gsub("%s*[\r\n]%s*", ""))
end

--- Strip what usually gets selected together with a token: line wraps, quotes and a `Bearer ` prefix.
---@param text string
---@return string
local function clean_input(text)
    local cleaned = vim.trim(text)
    local quote = cleaned:sub(1, 1)
    if #cleaned >= 2 and (quote == '"' or quote == "'" or quote == "`") and cleaned:sub(-1) == quote then
        cleaned = vim.trim(cleaned:sub(2, -2))
    end
    cleaned = cleaned:gsub("^[Bb]earer%s+", "")
    return join_wrapped_lines(cleaned)
end

--- Normalize base64 to the strict standard alphabet with padding, nil when it cannot be base64.
---@param text string
---@return string|nil
local function normalize_base64(text)
    local normalized = text:gsub("-", "+"):gsub("_", "/"):gsub("=+$", "")
    if normalized == "" or normalized:find("[^A-Za-z0-9+/]") then
        return nil
    end

    local remainder = #normalized % 4
    if remainder == 1 then
        return nil
    end
    if remainder > 0 then
        normalized = normalized .. string.rep("=", 4 - remainder)
    end
    return normalized
end

--- Decode base64 leniently: standard or URL-safe alphabet, with or without padding, line wraps ignored.
---@param text string
---@return string|nil decoded nil when the text is not base64
function M.decode_base64(text)
    local normalized = normalize_base64(join_wrapped_lines(text))
    if not normalized then
        return nil
    end

    local ok, decoded = pcall(vim.base64.decode, normalized)
    if not ok then
        return nil
    end
    return decoded
end

--- Decode JSON when it is an object or array.
---@param text string
---@return table|nil
local function decode_json_table(text)
    local ok, decoded = pcall(vim.json.decode, text)
    if ok and type(decoded) == "table" then
        return decoded
    end
    return nil
end

--- Check whether text is wrapped like a JSON object or array.
---@param text string
---@return boolean
local function looks_like_json(text)
    local first, last = text:sub(1, 1), text:sub(-1)
    return (first == "{" and last == "}") or (first == "[" and last == "]")
end

--- Decode a compact JWS (3 parts) or JWE (5 parts); nil when the text is not one.
---@param text string
---@return DecodeResult|nil
function M.decode_jwt(text)
    local kind = "jwt"
    local header_b64, payload_b64, signature = text:match(JWS_PATTERN)
    if not header_b64 then
        kind = "jwe"
        header_b64 = text:match(JWE_PATTERN)
    end
    if not header_b64 then
        return nil
    end

    local header_text = M.decode_base64(header_b64)
    local header = header_text and decode_json_table(header_text) or nil
    if not header then
        return nil
    end

    local result = { kind = kind, header = header, header_text = header_text }
    if kind == "jwt" then
        result.payload_text = M.decode_base64(payload_b64)
        result.payload = result.payload_text and decode_json_table(result.payload_text) or nil
        result.signature = signature
    end
    return result
end

--- Classify and decode text; nil when it is neither a JWT nor base64.
---@param text string
---@return DecodeResult|nil
function M.decode(text)
    local cleaned = clean_input(text)
    if cleaned == "" then
        return nil
    end

    local jwt = M.decode_jwt(cleaned)
    if jwt then
        return jwt
    end

    local decoded = M.decode_base64(cleaned)
    if not decoded then
        return nil
    end
    return { kind = "base64", decoded = decoded }
end

--- Format seconds as at most two units, e.g. "1h 5m", "2d 3h", "42s".
---@param seconds number
---@return string
local function human_duration(seconds)
    seconds = math.floor(seconds)
    if seconds < 60 then
        return seconds .. "s"
    end

    local parts = {}
    for _, unit in ipairs({ { "d", 86400 }, { "h", 3600 }, { "m", 60 } }) do
        local count = math.floor(seconds / unit[2])
        if count > 0 then
            table.insert(parts, count .. unit[1])
            seconds = seconds - count * unit[2]
        end
        if #parts == 2 then
            break
        end
    end
    return table.concat(parts, " ")
end

--- Comment lines describing the payload time claims (UTC + relative to now). Millisecond stamps are accepted.
---@param payload table
---@param now? integer epoch seconds, defaults to os.time()
---@return string[]
function M.time_claim_lines(payload, now)
    now = now or os.time()
    local lines = {}
    for _, claim in ipairs(TIME_CLAIMS) do
        local value = payload[claim]
        if type(value) == "number" then
            local seconds = value > 1e11 and math.floor(value / 1000) or math.floor(value)
            local delta = seconds - now
            local relative
            if delta == 0 then
                relative = "now"
            elseif claim == "exp" then
                relative = delta > 0 and ("expires in " .. human_duration(delta))
                    or ("EXPIRED " .. human_duration(-delta) .. " ago")
            else
                relative = delta > 0 and ("in " .. human_duration(delta)) or (human_duration(-delta) .. " ago")
            end
            table.insert(lines, ("// %s: %s (%s)"):format(claim, os.date("!%Y-%m-%dT%H:%M:%SZ", seconds), relative))
        end
    end
    return lines
end

--- Pretty-print JSON (jq when available) and split it into lines.
---@param text string
---@return string[]
local function json_lines(text)
    local formatted = require("utils.json.normalize").format_json(text)
    return vim.split(vim.trim(formatted), "\n", { plain = true })
end

--- Check that every byte sequence is well-formed UTF-8 (lead byte + the right number of continuation bytes).
---@param text string
---@return boolean
local function is_valid_utf8(text)
    local index, length = 1, #text
    while index <= length do
        local lead = text:byte(index)
        local continuation_count
        if lead < 0x80 then
            continuation_count = 0
        elseif lead >= 0xC2 and lead <= 0xDF then
            continuation_count = 1
        elseif lead >= 0xE0 and lead <= 0xEF then
            continuation_count = 2
        elseif lead >= 0xF0 and lead <= 0xF4 then
            continuation_count = 3
        else
            return false
        end
        for offset = 1, continuation_count do
            local byte = text:byte(index + offset)
            if not byte or byte < 0x80 or byte > 0xBF then
                return false
            end
        end
        index = index + continuation_count + 1
    end
    return true
end

--- Binary when it holds control bytes or is not valid UTF-8 (keys, hashes, random secrets).
---@param text string
---@return boolean
local function is_binary(text)
    return text:find("[%z\1-\8\11\12\14-\31]") ~= nil or not is_valid_utf8(text)
end

--- Render bytes in `xxd` layout so the `xxd` syntax highlights them.
---@param data string
---@return string[]
local function hex_dump_lines(data)
    local lines = {}
    for offset = 1, #data, 16 do
        local chunk = data:sub(offset, offset + 15)
        local groups = {}
        for index = 1, #chunk, 2 do
            table.insert(groups, (chunk:sub(index, index + 1):gsub(".", function(char)
                return ("%02x"):format(char:byte())
            end)))
        end
        local ascii = chunk:gsub("[^\32-\126]", ".")
        table.insert(lines, ("%08x: %-39s  %s"):format(offset - 1, table.concat(groups, " "), ascii))
    end
    return lines
end

--- Build the lines and filetype shown in the split for a decode result.
---@param result DecodeResult
---@param now? integer epoch seconds used for the time claims (tests)
---@return string[] lines
---@return string filetype
function M.render(result, now)
    if result.kind == "base64" then
        local decoded = result.decoded
        if is_binary(decoded) then
            return hex_dump_lines(decoded), "xxd"
        end

        local trimmed = vim.trim(decoded)
        if looks_like_json(trimmed) and decode_json_table(trimmed) then
            return json_lines(trimmed), "json"
        end
        return vim.split(decoded, "\n", { plain = true }), "text"
    end

    local lines = { "// header" }
    extend(lines, json_lines(result.header_text))
    if result.kind == "jwe" then
        table.insert(lines, "// JWE: the payload is encrypted, only the header can be decoded")
        return lines, "json5"
    end

    table.insert(lines, "// payload")
    if result.payload then
        extend(lines, json_lines(result.payload_text))
        extend(lines, M.time_claim_lines(result.payload, now))
    elseif result.payload_text then
        extend(lines, vim.split(result.payload_text, "\n", { plain = true }))
    else
        table.insert(lines, "// payload is not valid base64")
    end

    table.insert(lines, ("// signature (%d chars)"):format(#result.signature))
    table.insert(lines, result.signature ~= "" and result.signature or "// (none, unsigned token)")
    return lines, "json5"
end

--- Return the last visual selection ('< .. '>) as one string.
---@return string
local function visual_selection_text()
    local start_pos = vim.fn.getpos("'<")
    local end_pos = vim.fn.getpos("'>")
    local start_row, start_col = start_pos[2] - 1, start_pos[3] - 1
    local end_row, end_col = end_pos[2] - 1, end_pos[3]

    -- `'>` reports a huge column for linewise / end-of-line selections
    local end_line = vim.api.nvim_buf_get_lines(0, end_row, end_row + 1, false)[1] or ""
    end_col = math.min(end_col, #end_line)

    local lines = vim.api.nvim_buf_get_text(0, start_row, start_col, end_row, end_col, {})
    return table.concat(lines, "\n")
end

--- Show lines in a bottom scratch split (`q` closes it and restores the previous window).
---@param lines string[]
---@param filetype string
---@param kind string
local function open_split(lines, filetype, kind)
    local bufnr = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines)
    pcall(vim.api.nvim_buf_set_name, bufnr, ("decoded-%s-%d"):format(kind, bufnr))
    require("utils.buffer-util").open_scratch_split(bufnr, {
        filetype = filetype,
        max_height = math.max(10, math.floor(vim.o.lines * SPLIT_HEIGHT_RATIO)),
    })
end

--- Decode the last visual selection (JWT or base64) into a scratch split below.
function M.decode_selection()
    local result = M.decode(visual_selection_text())
    if not result then
        vim.notify("Selection is neither a JWT nor base64", vim.log.levels.WARN)
        return
    end

    local lines, filetype = M.render(result)
    open_split(lines, filetype, result.kind)
end

return M
