local home = os.getenv("HOME")
local M = {}

function M.dotfiles_path(relative_path)
    return vim.fs.joinpath(home, "dotfiles", relative_path)
end

local is_limited = (vim.env["LIMITED"] ~= nil) and true or false
M.is_not_limited = not is_limited

_G.global = M

return M
