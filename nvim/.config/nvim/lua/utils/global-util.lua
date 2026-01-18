local home = os.getenv("HOME")
local M = {}

M.dotfiles_path = function(relative_path)
    return vim.fs.joinpath(home, "dotfiles", relative_path)
end

_G.global = M

return M
