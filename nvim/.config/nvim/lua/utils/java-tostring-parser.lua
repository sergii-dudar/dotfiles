local M = {}

local RAW_NUM_SENTINEL = "%%__RAWNUM__%%"

-- Recursive descent parser for Java toString() output.
-- Supports two formats:
--   1. Lombok: ClassName(key=value, key2=NestedClass(k=v), arr=[Item(k=v)])
--   2. Swagger/OpenAPI: class ClassName {\n    key: value\n    arr: [class Nested {...}]\n}

local Parser = {}
Parser.__index = Parser

function Parser.new(input)
    local self = setmetatable({}, Parser)
    -- Normalize literal escape sequences to real characters
    self.input = input:gsub("\\n", "\n"):gsub("\\t", "    ")
    self.pos = 1
    self.len = #self.input
    return self
end

function Parser:peek()
    if self.pos > self.len then
        return ""
    end
    return self.input:sub(self.pos, self.pos)
end

function Parser:advance(n)
    self.pos = self.pos + (n or 1)
end

function Parser:remaining()
    if self.pos > self.len then
        return ""
    end
    return self.input:sub(self.pos)
end

function Parser:skip_ws()
    while self.pos <= self.len do
        local ch = self:peek()
        if ch == " " or ch == "\t" or ch == "\n" or ch == "\r" then
            self:advance()
        else
            break
        end
    end
end

function Parser:read_until(chars)
    local start = self.pos
    while self.pos <= self.len do
        if chars:find(self:peek(), 1, true) then
            break
        end
        self:advance()
    end
    return self.input:sub(start, self.pos - 1)
end

function Parser:starts_with(prefix)
    return self:remaining():sub(1, #prefix) == prefix
end

function Parser:match_ahead(pattern)
    return self:remaining():match("^" .. pattern)
end

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

function M.parse(input)
    if not input or input == "" then
        return nil, "empty input"
    end
    local parser = Parser.new(input)
    parser:skip_ws()
    local ok, result = pcall(function()
        return parser:parse_root_value()
    end)
    if not ok then
        return nil, "parse error: " .. tostring(result)
    end
    return result
end

function Parser:parse_root_value()
    self:skip_ws()
    if self:match_ahead("class%s+") then
        return self:parse_swagger_object()
    elseif self:peek() == "{" then
        return self:parse_bare_object()
    elseif self:match_ahead("%u[%w_%.]*%(") then
        return self:parse_lombok_object()
    elseif self:peek() == "(" then
        return self:parse_bare_lombok_object()
    elseif self:peek() == "[" then
        return self:parse_auto_array()
    else
        -- Attempt to parse as raw value text
        local raw = vim.trim(self:remaining())
        self.pos = self.len + 1
        return self:interpret_primitive(raw)
    end
end

--------------------------------------------------------------------------------
-- Lombok format: ClassName(key=value, key2=value2)
-- Also supports bare format: (key=value, key2=value2)
--------------------------------------------------------------------------------

function Parser:parse_lombok_object()
    -- Skip class name
    self:read_until("(")
    return self:parse_lombok_object_body()
end

function Parser:parse_bare_lombok_object()
    return self:parse_lombok_object_body()
end

function Parser:parse_lombok_object_body()
    self:advance() -- skip '('
    self:skip_ws()

    local result = {}

    while self.pos <= self.len and self:peek() ~= ")" do
        self:skip_ws()
        if self:peek() == ")" then
            break
        end

        -- Read key
        local key = self:read_until("=")
        self:advance() -- skip '='

        -- Parse value
        local value = self:parse_lombok_value()
        result[key] = value

        self:skip_ws()
        if self:peek() == "," then
            self:advance()
            self:skip_ws()
        end
    end

    if self:peek() == ")" then
        self:advance()
    end

    return result
end

function Parser:parse_lombok_value()
    self:skip_ws()

    -- Array
    if self:peek() == "[" then
        return self:parse_lombok_array()
    end

    -- Nested lombok object (ClassName(...))
    if self:match_ahead("%u[%w_%.]*%(") then
        return self:parse_lombok_object()
    end

    -- null / true / false
    local keyword = self:try_keyword_lombok()
    if keyword ~= nil then
        return keyword
    end

    -- Simple value: read until ',' or ')' at depth 0
    return self:read_lombok_simple_value()
end

function Parser:try_keyword_lombok()
    local remaining = self:remaining()
    if remaining:match("^null[,%)%]%s]") or remaining == "null" then
        self:advance(4)
        return vim.NIL
    end
    if remaining:match("^true[,%)%]%s]") or remaining == "true" then
        self:advance(4)
        return true
    end
    if remaining:match("^false[,%)%]%s]") or remaining == "false" then
        self:advance(5)
        return false
    end
    return nil
end

function Parser:read_lombok_simple_value()
    local start = self.pos
    local depth_paren = 0
    local depth_bracket = 0

    while self.pos <= self.len do
        local ch = self:peek()
        if ch == "(" then
            depth_paren = depth_paren + 1
        elseif ch == ")" then
            if depth_paren == 0 then
                break
            end
            depth_paren = depth_paren - 1
        elseif ch == "[" then
            depth_bracket = depth_bracket + 1
        elseif ch == "]" then
            if depth_bracket == 0 then
                break
            end
            depth_bracket = depth_bracket - 1
        elseif ch == "," and depth_paren == 0 and depth_bracket == 0 then
            break
        end
        self:advance()
    end

    local raw = vim.trim(self.input:sub(start, self.pos - 1))
    return self:interpret_primitive(raw)
end

function Parser:parse_lombok_array()
    self:advance() -- skip '['
    self:skip_ws()
    local result = {}

    while self.pos <= self.len and self:peek() ~= "]" do
        self:skip_ws()
        if self:peek() == "]" then
            break
        end

        local value = self:parse_lombok_value()
        table.insert(result, value)

        self:skip_ws()
        if self:peek() == "," then
            self:advance()
            self:skip_ws()
        end
    end

    if self:peek() == "]" then
        self:advance()
    end

    return result
end

--------------------------------------------------------------------------------
-- Swagger format: class ClassName {\n    key: value\n}
-- Also supports bare format: {\n    key: value\n}
--------------------------------------------------------------------------------

function Parser:parse_swagger_object()
    -- Skip "class "
    self:advance(6)
    -- Skip class name until '{'
    self:read_until("{")
    return self:parse_swagger_object_body()
end

function Parser:parse_bare_object()
    return self:parse_swagger_object_body()
end

function Parser:parse_swagger_object_body()
    self:advance() -- skip '{'
    self:skip_ws()

    local result = {}

    while self.pos <= self.len do
        self:skip_ws()
        if self:peek() == "}" then
            self:advance()
            break
        end
        if self.pos > self.len then
            break
        end

        -- Read key (until ':')
        local key = vim.trim(self:read_until(":"))
        if key == "" then
            -- No valid key found, skip character to avoid infinite loop
            self:advance()
        else
            self:advance() -- skip ':'
            -- Skip space after colon (but not newlines yet)
            while self.pos <= self.len and (self:peek() == " " or self:peek() == "\t") do
                self:advance()
            end

            -- Parse value
            local value = self:parse_swagger_value()
            result[key] = value
        end
    end

    return result
end

function Parser:parse_swagger_value()
    -- Array
    if self:peek() == "[" then
        return self:parse_swagger_array()
    end

    -- Nested swagger object (with class prefix)
    if self:match_ahead("class%s+") then
        return self:parse_swagger_object()
    end

    -- Nested bare object
    if self:peek() == "{" then
        return self:parse_bare_object()
    end

    -- Nested lombok object inside swagger
    if self:match_ahead("%u[%w_%.]*%(") then
        return self:parse_lombok_object()
    end

    -- Simple value: read until newline or end
    local start = self.pos
    while self.pos <= self.len do
        local ch = self:peek()
        if ch == "\n" then
            break
        end
        self:advance()
    end

    local raw = vim.trim(self.input:sub(start, self.pos - 1))

    if raw == "null" then
        return vim.NIL
    end
    if raw == "true" then
        return true
    end
    if raw == "false" then
        return false
    end

    return self:interpret_primitive(raw)
end

function Parser:parse_swagger_array()
    self:advance() -- skip '['
    self:skip_ws()
    local result = {}

    while self.pos <= self.len and self:peek() ~= "]" do
        self:skip_ws()
        if self:peek() == "]" then
            break
        end

        local value
        if self:match_ahead("class%s+") then
            value = self:parse_swagger_object()
        elseif self:peek() == "{" then
            value = self:parse_bare_object()
        elseif self:match_ahead("%u[%w_%.]*%(") then
            value = self:parse_lombok_object()
        else
            value = self:read_swagger_array_simple_value()
        end

        table.insert(result, value)

        self:skip_ws()
        if self:peek() == "," then
            self:advance()
            self:skip_ws()
        end
    end

    if self:peek() == "]" then
        self:advance()
    end

    return result
end

function Parser:read_swagger_array_simple_value()
    local start = self.pos
    local depth = 0

    while self.pos <= self.len do
        local ch = self:peek()
        if ch == "[" or ch == "{" then
            depth = depth + 1
        elseif ch == "]" or ch == "}" then
            if depth == 0 then
                break
            end
            depth = depth - 1
        elseif ch == "," and depth == 0 then
            break
        end
        self:advance()
    end

    local raw = vim.trim(self.input:sub(start, self.pos - 1))
    if raw == "null" then
        return vim.NIL
    end
    if raw == "true" then
        return true
    end
    if raw == "false" then
        return false
    end
    return self:interpret_primitive(raw)
end

--------------------------------------------------------------------------------
-- Auto-detect array (could contain lombok or swagger elements)
--------------------------------------------------------------------------------

function Parser:parse_auto_array()
    self:advance() -- skip '['
    self:skip_ws()
    local result = {}

    while self.pos <= self.len and self:peek() ~= "]" do
        self:skip_ws()
        if self:peek() == "]" then
            break
        end

        local value
        if self:match_ahead("class%s+") then
            value = self:parse_swagger_object()
        elseif self:peek() == "{" then
            value = self:parse_bare_object()
        elseif self:match_ahead("%u[%w_%.]*%(") then
            value = self:parse_lombok_object()
        elseif self:peek() == "[" then
            value = self:parse_auto_array()
        else
            value = self:read_swagger_array_simple_value()
        end

        table.insert(result, value)

        self:skip_ws()
        if self:peek() == "," then
            self:advance()
            self:skip_ws()
        end
    end

    if self:peek() == "]" then
        self:advance()
    end

    return result
end

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

function Parser:interpret_primitive(str)
    if str == "" then
        return str
    end
    -- Number: integer or decimal (possibly negative) — preserve original precision
    if str:match("^-?%d+%.?%d*$") then
        return RAW_NUM_SENTINEL .. str
    end
    return str
end

--- Encode parsed result to JSON string, preserving original number precision.
function M.to_json(result)
    local json_str = vim.json.encode(result)
    -- Replace quoted sentinels with raw number literals: "%%__RAWNUM__%%100.00" → 100.00
    json_str = json_str:gsub('"' .. RAW_NUM_SENTINEL:gsub("%%", "%%%%") .. '([^"]-)"', "%1")
    return json_str
end

return M
