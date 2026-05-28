-- Project detection: determine if cwd is a multi-file project (for neo-tree auto-open).
--
-- • is_multifile_proj — check if cwd matches known project directories
-- • is_init_open_neotree — decide if neo-tree should open on startup

local M = {}

--- Check whether the current directory is a known multi-file project.
function M.is_multifile_proj()
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
        "myforks",
        "tools",
    }
    -- vim.notify("result: " .. tostring(list_util.any_match(current_file, open_if_dir_contains)), vim.log.levels.INFO)

    -- load neotree only to lister dirs, and disable for direct file[s] open (like `nvim [files...]`)
    -- vim.notify("result 1: " .. tostring(list_util.any_match(current_file, open_if_dir_contains)), vim.log.levels.INFO)
    -- vim.notify("result 2: " .. tostring(vim.fn.argc()), vim.log.levels.INFO)

    -- log_table(vim.fn.argv())
    -- print(vim.fn.argc())

    return list_util.any_match(current_file, multifile_projs_dirs) and vim.fn.argc() == 0
end

--- Check whether neo-tree should open on startup.
function M.is_init_open_neotree()
    local current_file = vim.fn.getcwd() -- vim.fn.expand("%:p")
    local list_util = require("utils.list-util")
    local multifile_projs_dirs = {
        "dotfiles",
        "work",
        "nvim",
        "personal",
        "serhii.home",
    }

    return list_util.any_match(current_file, multifile_projs_dirs) and vim.fn.argc() == 0
end

return M
