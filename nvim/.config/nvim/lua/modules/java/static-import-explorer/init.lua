local util = require("modules.java.static-import-explorer.util")
local picker = require("modules.java.static-import-explorer.picker")

local M = {}

local settings = {
    -- "wildcard": import static pkg.Class.*;
    -- "explicit": import static pkg.Class.memberName;
    import_mode = "explicit",
    -- auto-import without showing multiselect when only one result found
    auto_apply_single = true,
}

local state = {
    source_bufnr = nil,
    include_deps = false,
    include_all_deps = false,
    starts_with = false,
    current_word = "",
    default_glob = "*.java",
}

function M.find()
    state.source_bufnr = vim.api.nvim_get_current_buf()
    state.current_word = vim.fn.expand("<cword>")

    local dirs = util.get_search_dirs(state)
    if #dirs == 0 then
        vim.notify("[Static Import] No search directories found", vim.log.levels.WARN)
        return
    end

    picker.open(settings, state)
end

return M
