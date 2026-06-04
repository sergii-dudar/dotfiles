-- Registry that maps a filetype to its test-report language adapter.
-- Each adapter pack registers itself when its public module is first required.

local M = {}

---@type table<string, test_report.LangAdapter>
local adapters = {}

---@param filetype string
---@param adapter test_report.LangAdapter
function M.register(filetype, adapter)
    adapters[filetype] = adapter
end

---@param filetype string
---@return test_report.LangAdapter|nil
function M.get(filetype)
    return adapters[filetype]
end

---@param filetype string
---@return boolean
function M.has(filetype)
    return adapters[filetype] ~= nil
end

---@return table<string, test_report.LangAdapter>
function M.all()
    return adapters
end

return M
