-- Glob matching used to resolve file-template rules.
--
-- Supported glob syntax (translated to anchored Lua patterns):
--   *  any sequence of characters, including none
--   ?  exactly one character
-- Everything else matches literally.

require("modules.common.file-template.types")

local M = {}

---@type table<string, string>
local pattern_cache = {}

local MAGIC_CHARS = "^$()%.[]*+-?"

--- Convert a glob expression into a fully anchored Lua pattern.
---@param glob string
---@return string
function M.to_pattern(glob)
    local cached = pattern_cache[glob]
    if cached then
        return cached
    end

    local parts = { "^" }
    for index = 1, #glob do
        local char = glob:sub(index, index)
        if char == "*" then
            parts[#parts + 1] = ".*"
        elseif char == "?" then
            parts[#parts + 1] = "."
        elseif MAGIC_CHARS:find(char, 1, true) then
            parts[#parts + 1] = "%" .. char
        else
            parts[#parts + 1] = char
        end
    end
    parts[#parts + 1] = "$"

    local pattern = table.concat(parts)
    pattern_cache[glob] = pattern
    return pattern
end

---@param value string|nil
---@param glob string
---@param ignore_case boolean|nil
---@return boolean
function M.matches(value, glob, ignore_case)
    if value == nil then
        return false
    end
    if ignore_case then
        value, glob = value:lower(), glob:lower()
    end
    return value:match(M.to_pattern(glob)) ~= nil
end

--- Match a value against a list of alternatives. A `nil`/empty list means the
--- rule does not constrain this field, which counts as a match.
---@param value string|nil
---@param globs string[]|string|nil
---@param ignore_case boolean|nil
---@return boolean
function M.matches_any(value, globs, ignore_case)
    if globs == nil then
        return true
    end
    if type(globs) == "string" then
        globs = { globs }
    end
    if #globs == 0 then
        return true
    end

    for _, glob in ipairs(globs) do
        if M.matches(value, glob, ignore_case) then
            return true
        end
    end
    return false
end

---@param rule file_template.Rule
---@param ctx file_template.Context
---@return boolean
function M.rule_matches(rule, ctx)
    if not M.matches_any(ctx.package, rule.packages, true) then
        return false
    end
    if not M.matches_any(ctx.basename, rule.filename, false) then
        return false
    end
    if not M.matches_any(ctx.path, rule.path, false) then
        return false
    end
    if not M.matches_any(ctx.source_set, rule.source_set, true) then
        return false
    end
    if rule.when and not rule.when(ctx) then
        return false
    end
    return true
end

--- First matching rule, evaluated top-to-bottom.
---@param rules file_template.Rule[]|nil
---@param ctx file_template.Context
---@return file_template.Rule|nil
function M.first_match(rules, ctx)
    for _, rule in ipairs(rules or {}) do
        if M.rule_matches(rule, ctx) then
            return rule
        end
    end
    return nil
end

return M
