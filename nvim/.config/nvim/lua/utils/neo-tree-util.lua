local M = {}

M.is_enable_neo_tree = function()
    local current_file = vim.fn.getcwd() -- vim.fn.expand("%:p")
    local list_util = require("utils.list-util")
    local open_if_dir_contains = {
        "dotfiles",
        "git\\.",
        "git\\-",
        --"myforks/my%-dwm",
    }
    -- vim.notify("result: " .. tostring(list_util.any_match(current_file, open_if_dir_contains)), vim.log.levels.INFO)
    return list_util.any_match(current_file, open_if_dir_contains)
end

return M
