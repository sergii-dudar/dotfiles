local M = {}

M.is_multifile_proj = function()
    local current_file = vim.fn.getcwd() -- vim.fn.expand("%:p")
    local list_util = require("utils.list-util")
    local multifile_projs_dirs = {
        "dotfiles",
        "git",
        "work",
        "nvim",
        ".tmux",
        "personal",
        "serhii.home",
        "myforks/my%-dwm",
    }
    -- vim.notify("result: " .. tostring(list_util.any_match(current_file, open_if_dir_contains)), vim.log.levels.INFO)

    -- load neotree only to lister dirs, and disable for direct file[s] open (like `nvim [files...]`)
    -- vim.notify("result 1: " .. tostring(list_util.any_match(current_file, open_if_dir_contains)), vim.log.levels.INFO)
    -- vim.notify("result 2: " .. tostring(vim.fn.argc()), vim.log.levels.INFO)

    -- log_table(vim.fn.argv())
    -- print(vim.fn.argc())

    return list_util.any_match(current_file, multifile_projs_dirs) and vim.fn.argc() == 0
end

return M
