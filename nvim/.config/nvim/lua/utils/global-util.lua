local home = os.getenv("HOME")
local M = {}

function M.dotfiles_path(relative_path)
    return vim.fs.joinpath(home, "dotfiles", relative_path)
end

_G.global = M

return M
