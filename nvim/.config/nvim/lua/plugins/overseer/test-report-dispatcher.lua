-- Filetype-aware test-report dispatcher.
-- Resolves the right language module (which registers its adapter on require)
-- and then forwards calls to the generic core. Keymaps in plugins/overseer/init.lua
-- go through here so they work across languages without per-lang knowledge.

local M = {}
local lang_registry = require("utils.lang.registry")

---@return string|nil
local function current_filetype()
    local ft = vim.bo.filetype
    if ft == nil or ft == "" then
        return nil
    end
    return ft
end

--- Ensure the language module is loaded (which triggers adapter registration).
--- Returns the core module after registration, or nil if unsupported.
---@param ft string|nil
---@return table|nil
local function ensure_loaded(ft)
    if not ft then
        return nil
    end
    local report = lang_registry.report_for_filetype(ft)
    if not report then
        return nil
    end
    local ok, err = pcall(require, report.module)
    if not ok then
        vim.notify("test-report: failed to load " .. report.module .. ": " .. tostring(err), vim.log.levels.ERROR)
        return nil
    end
    return require("modules.common.test-report")
end

local function notify_unsupported(action)
    vim.notify(
        "test-report: " .. action .. " not supported for filetype: " .. tostring(vim.bo.filetype),
        vim.log.levels.WARN
    )
end

function M.show_output()
    local core = ensure_loaded(current_filetype())
    if not core then
        notify_unsupported("show output")
        return
    end
    core.show_test_output()
end

function M.hide_output()
    local core = ensure_loaded(current_filetype())
    if not core then
        return
    end
    core.hide_test_output()
end

function M.load_existing()
    local core = ensure_loaded(current_filetype())
    if not core then
        notify_unsupported("load existing report")
        return
    end
    core.load_existing()
end

function M.open_tree_view()
    local core = ensure_loaded(current_filetype())
    if not core then
        notify_unsupported("tree view")
        return
    end
    core.open_tree_view()
end

function M.trouble_toggle()
    local ft = current_filetype()
    local report = lang_registry.report_for_filetype(ft)
    local source = report and report.trouble_source
    if not source then
        notify_unsupported("trouble diagnostics")
        return
    end
    vim.cmd("Trouble " .. source .. " toggle")
end

function M.picker_diagnostics()
    local ft = current_filetype()
    local report = lang_registry.report_for_filetype(ft)
    local source = report and report.diagnostic_source
    if not source then
        notify_unsupported("diagnostics picker")
        return
    end
    Snacks.picker.diagnostics({
        filter = {
            filter = function(item)
                return item.item.source == source
            end,
        },
    })
end

return M
