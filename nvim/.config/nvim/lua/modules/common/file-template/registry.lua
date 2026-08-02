-- Registry that maps a filetype to its file-template language adapter.
-- Each adapter registers itself when its public module is first required.

require("modules.common.file-template.types")

local M = {}

---@type table<string, file_template.Adapter>
local adapters = {}

--- Filetype -> module that registers the adapter for it. The module is required
--- lazily, on the first buffer of that filetype, so nothing language specific is
--- loaded in unrelated projects.
---@type table<string, string>
local adapter_modules = {
    java = "modules.java.file-template",
}

---@param adapter file_template.Adapter
function M.register(adapter)
    for _, filetype in ipairs(adapter.filetypes or {}) do
        adapters[filetype] = adapter
    end
end

--- Declare an additional filetype -> adapter module mapping (for new languages).
---@param filetype string
---@param module string
function M.register_module(filetype, module)
    adapter_modules[filetype] = module
end

--- Filetypes that may have a template adapter. Used as the autocmd pattern, so
--- it must be resolvable without loading any adapter.
---@return string[]
function M.filetypes()
    return vim.tbl_keys(adapter_modules)
end

---@param filetype string
---@return boolean
function M.has(filetype)
    return adapter_modules[filetype] ~= nil or adapters[filetype] ~= nil
end

---@param filetype string
---@return file_template.Adapter|nil
function M.get(filetype)
    if adapters[filetype] then
        return adapters[filetype]
    end

    local module = adapter_modules[filetype]
    if not module then
        return nil
    end

    local ok, err = pcall(require, module)
    if not ok then
        vim.notify("file-template: failed to load " .. module .. ": " .. tostring(err), vim.log.levels.ERROR)
        return nil
    end

    return adapters[filetype]
end

---@return table<string, file_template.Adapter>
function M.all()
    return adapters
end

return M
