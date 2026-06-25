-- Project detection: determine if cwd is a multi-file project (for neo-tree auto-open).
--
-- • is_multifile_proj — check if cwd matches known project directories
-- • is_init_open_neotree — decide if neo-tree should open on startup

local M = {}

local multifile_proj_dirs = {
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

local init_open_neotree_dirs = {
    "dotfiles",
    "work",
    "nvim",
    "personal",
    "serhii.home",
}

--- Check whether a path contains one of the supplied directory names as a literal segment.
---@param path string
---@param dirs string[]
---@return boolean
local function has_dir_segment(path, dirs)
    local dir_set = {}
    for _, dir in ipairs(dirs) do
        dir_set[dir] = true
    end

    for segment in vim.fs.normalize(path):gmatch("[^/]+") do
        if dir_set[segment] then
            return true
        end
    end
    return false
end

--- Check whether the startup argument list includes files.
---@return boolean
local function has_file_args()
    for _, arg in ipairs(vim.fn.argv()) do
        local path = vim.fn.fnamemodify(arg, ":p")
        if vim.fn.isdirectory(path) == 0 then
            return true
        end
    end
    return false
end

--- Check whether startup should use project behavior for the current directory.
---@param dirs string[]
---@return boolean
local function is_project_startup(dirs)
    return has_dir_segment(vim.fn.getcwd(), dirs) and not has_file_args()
end

--- Check whether the current directory is a known multi-file project.
function M.is_multifile_proj()
    return is_project_startup(multifile_proj_dirs)
end

--- Check whether neo-tree should open on startup.
function M.is_init_open_neotree()
    return is_project_startup(init_open_neotree_dirs)
end

return M
