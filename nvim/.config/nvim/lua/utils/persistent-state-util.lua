-- Persistent key/value state shared by the whole config: one flat JSON object in
-- `persistant.state.json` at the config root (`~/.config/nvim` is the stow symlink to
-- `~/dotfiles/nvim/.config/nvim`). Keys are dotted strings prefixed with the owning
-- language/area (`java.references.show_tests`); values are any JSON value.
--
-- The file is read once, on the first `get`, into a Lua table that serves every later
-- `get`. `set` re-reads the file (so a key written meanwhile by another Neovim instance
-- survives), changes the one key, rewrites the whole file with vim.fn.json_encode and
-- keeps the merged table as the new cache. A missing or unparsable file counts as empty.
--
-- Usage:
--   local state = require("utils.persistent-state-util")
--   state.get("java.references.show_tests", false)
--   state.set("java.references.show_tests", true)
--   state.toggle("java.references.show_tests", false)

local M = {}

M.config = {
    path = vim.fn.stdpath("config") .. "/persistant.state.json",
}

---@return table<string, any>
local function read()
    local ok, lines = pcall(vim.fn.readfile, M.config.path)
    if not ok or #lines == 0 then
        return {}
    end
    local decoded_ok, tbl = pcall(vim.fn.json_decode, table.concat(lines, "\n"))
    if not decoded_ok or type(tbl) ~= "table" then
        vim.notify("persistent-state: cannot parse " .. M.config.path .. ", treating it as empty", vim.log.levels.WARN)
        return {}
    end
    return tbl
end

---@param tbl table<string, any>
local function write(tbl)
    -- "b": no newline after the last line (editorconfig: insert_final_newline = false)
    if vim.fn.writefile({ vim.fn.json_encode(tbl) }, M.config.path, "b") ~= 0 then
        vim.notify("persistent-state: cannot write " .. M.config.path, vim.log.levels.ERROR)
    end
end

local cache = nil ---@type table<string, any>|nil

--- The cached state, loaded from the file on first use.
---@return table<string, any>
local function data()
    if cache == nil then
        cache = read()
    end
    return cache
end

--- Read a key; `default` when it is absent (or JSON null).
---@param key string
---@param default? any
---@return any
function M.get(key, default)
    local value = data()[key]
    if value == nil or value == vim.NIL then
        return default
    end
    return value
end

--- Write a key (nil removes it): re-read the file, change the key, save the whole table,
--- and keep it as the cache.
---@param key string
---@param value any
function M.set(key, value)
    local tbl = read()
    tbl[key] = value
    write(tbl)
    cache = tbl
end

--- Flip a boolean key (absent counts as `default`), save, and return the new value.
---@param key string
---@param default? boolean
---@return boolean
function M.toggle(key, default)
    local value = not (M.get(key, default) == true)
    M.set(key, value)
    return value
end

---@return string
function M.path()
    return M.config.path
end

return M
