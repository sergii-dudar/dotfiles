-- Restore project-wide `gf` in multi-module Java projects.
--
-- nvim-jdtls overwrites the buffer-local 'path' with ONE module's source dirs
-- (whichever buffer first hit ServiceReady) and stamps that same path onto every
-- jdtls buffer via its own LspAttach handler. In a multi-module repo that breaks
-- `gf` on classpath resources that live in a sibling module, since the buffer's
-- 'path' no longer contains that module's resource dirs.
--
-- • restore_multimodule_gf_path — re-append recursive entries so `gf` can resolve
--   resources in any module again, preferring `src/` and deprioritizing `target/`

local M = {}

-- Entries appended (in order) to the buffer 'path'. 'path' is searched left-to-right
-- and `gf` takes the first match, so ordering encodes priority:
--   • "**/src/**" — every module's `src/` tree at any depth; searched FIRST so a
--     resource always resolves from source over any stale/generated `target/` copy.
--     A bare "**" alone would match the `target/` copy first (verified).
--   • "**"        — repo-wide fallback, searched LAST, only for files that exist
--     nowhere but `target/` (e.g. generated sources). Drop this entry to exclude
--     `target/` from `gf` entirely.
local GF_PATH_ENTRIES = { "**/src/**", "**" }

--- Re-append recursive `gf` entries to the buffer's 'path' so `gf` resolves
--- resources across all modules, not just the one nvim-jdtls baked in, while
--- preferring `src/` over `target/` (see GF_PATH_ENTRIES).
---
--- Deferred via vim.schedule so it runs AFTER nvim-jdtls' own (later-registered)
--- LspAttach handler, which would otherwise clobber this append.
---@param bufnr integer
function M.restore_multimodule_gf_path(bufnr)
    vim.schedule(function()
        if not vim.api.nvim_buf_is_valid(bufnr) then
            return
        end
        local existing = vim.split(vim.bo[bufnr].path, ",")
        local to_add = {}
        for _, entry in ipairs(GF_PATH_ENTRIES) do
            if not vim.tbl_contains(existing, entry) then
                table.insert(to_add, entry)
            end
        end
        if #to_add == 0 then
            return
        end
        local added = table.concat(to_add, ",")
        local path = vim.bo[bufnr].path
        vim.bo[bufnr].path = (path == "" and added or path .. "," .. added)
    end)
end

return M
