-- Canonical transformation detection and destination correction.
-- Detects the true old→new prefix transformation from file-level moves (always accurate),
-- then corrects directory-level changes that may have wrong destinations from file managers.

local M = {}

local string_util = require("utils.string-util")
local consts = require("modules.java.refactor.constants")

local log = consts.log
local package_roots = consts.package_roots

---@class CanonicalTransformation
---@field old_prefix string The old package prefix (e.g., "ua/raiffeisen/payments")
---@field new_prefix string The new package prefix (e.g., "ua/obama/govern/test/other")
---@field root string The matched package root (e.g., "src/test/java/")

--- Detect canonical transformation from file-level changes.
--- Compares a file's old dir vs new dir to find what prefix changed.
---@param all_changes java.rejactor.FileMove[]
---@return CanonicalTransformation|nil
function M.detect(all_changes)
    for _, change in ipairs(all_changes) do
        if change.src:match("%.java$") then
            -- Find matching root
            local root_match = nil
            for _, root in ipairs(package_roots) do
                if string_util.contains(change.src, root) and string_util.contains(change.dst, root) then
                    root_match = root
                    break
                end
            end
            if root_match then
                -- Strip root and filename to get directory parts
                local old_pkg = vim.split(change.src, root_match)[2]
                local new_pkg = vim.split(change.dst, root_match)[2]
                old_pkg = old_pkg:match("(.+)/[^/]+$") or ""
                new_pkg = new_pkg:match("(.+)/[^/]+$") or ""

                -- Find common suffix by comparing segments from the end
                local old_segs = vim.split(old_pkg, "/")
                local new_segs = vim.split(new_pkg, "/")
                local common_suffix_count = 0
                for k = 0, math.min(#old_segs, #new_segs) - 1 do
                    if old_segs[#old_segs - k] == new_segs[#new_segs - k] then
                        common_suffix_count = common_suffix_count + 1
                    else
                        break
                    end
                end

                -- The changed part is everything before the common suffix
                local old_prefix_parts = {}
                for k = 1, #old_segs - common_suffix_count do
                    table.insert(old_prefix_parts, old_segs[k])
                end
                local new_prefix_parts = {}
                for k = 1, #new_segs - common_suffix_count do
                    table.insert(new_prefix_parts, new_segs[k])
                end

                local old_pref = table.concat(old_prefix_parts, "/")
                local new_pref = table.concat(new_prefix_parts, "/")

                if old_pref ~= "" and new_pref ~= "" then
                    log.info("Canonical transformation detected:", old_pref, "->", new_pref)
                    return {
                        old_prefix = old_pref,
                        new_prefix = new_pref,
                        root = root_match,
                    }
                end
            end
        end
    end
    return nil
end

--- Correct directory change destinations using the canonical transformation.
--- File managers may emit intermediate directory events with WRONG destinations.
--- This uses the canonical prefix to recompute correct destinations.
---@param all_changes java.rejactor.FileMove[]
---@param canonical CanonicalTransformation
function M.correct_destinations(all_changes, canonical)
    for _, change in ipairs(all_changes) do
        if not change.src:match("%.java$") then
            -- This is a directory change — verify and correct its destination
            local root_match = nil
            for _, root in ipairs(package_roots) do
                if string_util.contains(change.src, root) then
                    root_match = root
                    break
                end
            end
            if root_match then
                local src_pkg = vim.split(change.src, root_match)[2]
                if src_pkg then
                    src_pkg = src_pkg:gsub("/$", "")
                    -- Check if this directory starts with the old prefix
                    if
                        src_pkg == canonical.old_prefix
                        or src_pkg:find("^" .. vim.pesc(canonical.old_prefix) .. "/")
                    then
                        -- Compute correct destination by replacing old prefix with new
                        local suffix = ""
                        if #src_pkg > #canonical.old_prefix then
                            suffix = src_pkg:sub(#canonical.old_prefix + 1)
                        end
                        local correct_dst_pkg = canonical.new_prefix .. suffix
                        local base_path = vim.split(change.src, root_match)[1]
                        local correct_dst = base_path .. root_match .. correct_dst_pkg

                        if change.dst ~= correct_dst then
                            log.info(
                                "Correcting directory change destination:",
                                change.dst,
                                "->",
                                correct_dst,
                                "(src:",
                                change.src,
                                ")"
                            )
                            change.dst = correct_dst
                        end
                    end
                end
            end
        end
    end
end

--- Check if a directory change is a PARENT of the canonical old prefix.
--- Parent-of-canonical changes are intermediate/partial filesystem events that should be skipped.
---@param change java.rejactor.FileMove
---@param canonical CanonicalTransformation|nil
---@return boolean
function M.is_parent_of_canonical(change, canonical)
    if not canonical then
        return false
    end
    if change.src:match("%.java$") then
        return false
    end

    local root_match = nil
    for _, root in ipairs(package_roots) do
        if string_util.contains(change.src, root) then
            root_match = root
            break
        end
    end
    if not root_match then
        return false
    end

    local src_pkg = vim.split(change.src, root_match)[2]
    if not src_pkg then
        return false
    end
    src_pkg = src_pkg:gsub("/$", "")

    -- A strict parent means canonical starts with this path followed by "/"
    return src_pkg ~= canonical.old_prefix and canonical.old_prefix:find("^" .. vim.pesc(src_pkg) .. "/") ~= nil
end

return M
