-- Filetype-aware test-report dispatcher.
-- Resolves the right language module (which registers its adapter on require)
-- and then forwards calls to the generic core. Keymaps in plugins/overseer/init.lua
-- go through here so they work across languages without per-lang knowledge.

local M = {}

-- filetype -> module path of the language test-report shim
local ft_to_module = {
    java = "modules.java.test-report",
    rust = "modules.rust.test-report",
    go = "modules.go.test-report",
    lua = "modules.lua.test-report",
    sh = "modules.bash.test-report",
    bash = "modules.bash.test-report",
}

-- filetype -> Trouble source name used by that adapter's diagnostics
local ft_to_trouble_source = {
    java = "junit_diagnostics",
    rust = "cargo_test_diagnostics",
    go = "go_test_diagnostics",
    lua = "busted_test_diagnostics",
    sh = "bashunit_test_diagnostics",
    bash = "bashunit_test_diagnostics",
}

-- filetype -> diagnostic source name (matches adapter.diagnostic_source)
local ft_to_diagnostic_source = {
    java = "junit",
    rust = "cargo-test",
    go = "go-test",
    lua = "busted",
    sh = "bashunit",
    bash = "bashunit",
}

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
    local mod_path = ft_to_module[ft]
    if not mod_path then
        return nil
    end
    pcall(require, mod_path)
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
    local source = ft and ft_to_trouble_source[ft]
    if not source then
        notify_unsupported("trouble diagnostics")
        return
    end
    vim.cmd("Trouble " .. source .. " toggle")
end

function M.picker_diagnostics()
    local ft = current_filetype()
    local source = ft and ft_to_diagnostic_source[ft]
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
