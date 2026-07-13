-- Restore project-wide `gf` in multi-module Java projects.
--
-- nvim-jdtls overwrites the buffer-local 'path' with ONE module's source dirs
-- (whichever buffer first hit ServiceReady) and stamps that same path onto every
-- jdtls buffer via its own LspAttach handler. In a multi-module repo that breaks
-- `gf` on classpath resources that live in a sibling module, since the buffer's
-- 'path' no longer contains that module's resource dirs.
--
-- • restore_multimodule_gf_path — re-append a repo-root recursive entry ("**") so
--   `gf` can resolve resources in any module again

local M = {}

--- Re-append a repo-root recursive entry ("**") to the buffer's 'path' so `gf`
--- resolves resources across all modules, not just the one nvim-jdtls baked in.
---
--- Deferred via vim.schedule so it runs AFTER nvim-jdtls' own (later-registered)
--- LspAttach handler, which would otherwise clobber this append.
---@param bufnr integer
function M.restore_multimodule_gf_path(bufnr)
    vim.schedule(function()
        if not vim.api.nvim_buf_is_valid(bufnr) then
            return
        end
        local path = vim.bo[bufnr].path
        if not vim.tbl_contains(vim.split(path, ","), "**") then
            vim.bo[bufnr].path = (path == "" and "**" or path .. ",**")
        end
    end)
end

return M
