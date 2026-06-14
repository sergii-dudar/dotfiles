-- Global namespace (_G.global) for dotfiles-wide utilities.
--
-- • dotfiles_path(relative) — resolve absolute path under ~/dotfiles
-- • is_limited / is_not_limited — feature gate based on LIMITED env var

local home = os.getenv("HOME")
local M = {}

--- Resolve an absolute path under ~/dotfiles.
function M.dotfiles_path(relative_path)
    return vim.fs.joinpath(home, "dotfiles", relative_path)
end

M.is_limited = (vim.env["LIMITED"] ~= nil) and true or false
M.is_not_limited = not M.is_limited
M.is_all = (vim.env["LOAD_ALL"] ~= nil) and true or false

_G.global = M

return M