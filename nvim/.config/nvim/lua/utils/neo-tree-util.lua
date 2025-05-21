local M = {}

M.is_enable_neo_tree = function()
    local current_file = vim.fn.getcwd() -- vim.fn.expand("%:p")
    local list_util = require("utils.list-util")
    local open_if_dir_contains = {
        "dotfiles",
        "git",
        "work",
        --"myforks/my%-dwm",
    }
    -- vim.notify("result: " .. tostring(list_util.any_match(current_file, open_if_dir_contains)), vim.log.levels.INFO)

    -- load neotree only to lister dirs, and disable for direct file[s] open (like `nvim [files...]`)
    return list_util.any_match(current_file, open_if_dir_contains) and vim.fn.argc() == 0
end

return M
