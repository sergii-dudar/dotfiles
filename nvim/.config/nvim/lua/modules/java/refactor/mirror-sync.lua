-- Test↔Src mirror synchronization.
-- Detects structural refactorings, computes individual mirrors, filters parent-of-canonical,
-- deduplicates mirrors, performs physical moves, and cleans up empty directories.

local M = {}

local string_util = require("utils.string-util")
local buffer_util = require("utils.buffer-util")
local consts = require("modules.java.refactor.constants")
local canonical_mod = require("modules.java.refactor.canonical")

local log = consts.log
local shell_escape = consts.shell_escape

--- Detect structural refactoring patterns.
--- Multiple directories moving from the same parent to a common new parent
--- indicates a bulk structural move (e.g., adapter/* -> adapter/code/*).
---@param all_changes java.rejactor.FileMove[]
---@return table<string, string> structural_refactorings {src_parent -> dst_parent}
local function detect_structural_refactorings(all_changes)
    local structural_refactorings = {}

    local dir_moves = {}
    for _, change in ipairs(all_changes) do
        if
            (string_util.contains(change.src, "src/main/java/") or string_util.contains(change.src, "src/test/java/"))
            and not change.src:match("%.java$")
        then
            local src_parent = change.src:match("(.+)/[^/]+$")
            local dst_parent = change.dst:match("(.+)/[^/]+$")

            if src_parent and dst_parent and src_parent ~= dst_parent then
                if not dir_moves[src_parent] then
                    dir_moves[src_parent] = {}
                end
                table.insert(dir_moves[src_parent], { src_parent = src_parent, dst_parent = dst_parent })
            end
        end
    end

    -- If we have multiple directories moving from the same parent, it's a structural refactoring
    for src_parent, moves in pairs(dir_moves) do
        if #moves >= 2 then
            local common_dst_parent = moves[1].dst_parent
            local all_same = true
            for _, move in ipairs(moves) do
                if move.dst_parent ~= common_dst_parent then
                    all_same = false
                    break
                end
            end

            if all_same then
                structural_refactorings[src_parent] = common_dst_parent
                log.info("Detected structural refactoring:", src_parent, "->", common_dst_parent)
            end
        end
    end

    return structural_refactorings
end

--- Compute mirrors from structural refactorings.
--- Finds ALL counterpart subdirectories and adds them as mirrors.
---@param structural_refactorings table<string, string>
---@param test_mirrors java.rejactor.FileMove[]
---@param test_mirror_dirs table<string, string>
local function compute_structural_mirrors(structural_refactorings, test_mirrors, test_mirror_dirs)
    for src_parent, dst_parent in pairs(structural_refactorings) do
        local mirror_src_parent, mirror_dst_parent
        if string_util.contains(src_parent, "src/main/java/") then
            mirror_src_parent = src_parent:gsub("src/main/java/", "src/test/java/")
            mirror_dst_parent = dst_parent:gsub("src/main/java/", "src/test/java/")
        elseif string_util.contains(src_parent, "src/test/java/") then
            mirror_src_parent = src_parent:gsub("src/test/java/", "src/main/java/")
            mirror_dst_parent = dst_parent:gsub("src/test/java/", "src/main/java/")
        else
            goto continue_structural
        end

        log.debug("Checking for counterpart subdirectories in:", mirror_src_parent)
        if vim.fn.isdirectory(mirror_src_parent) == 1 then
            local fd_cmd = "fd --max-depth 1 --type d . " .. shell_escape(mirror_src_parent)
            log.debug("Running fd command:", fd_cmd)
            local handle = io.popen(fd_cmd)
            if handle then
                local found_count = 0
                for subdir in handle:lines() do
                    subdir = subdir:gsub("/$", "")
                    local subdir_name = subdir:match(".+/([^/]+)$")
                    log.debug("Found counterpart subdirectory:", subdir, "name:", subdir_name)

                    -- Skip the destination directory itself
                    if subdir_name and not subdir:match("/" .. vim.pesc(dst_parent:match(".+/([^/]+)$")) .. "$") then
                        local dst_subdir = mirror_dst_parent .. "/" .. subdir_name

                        if not test_mirror_dirs[subdir] then
                            test_mirror_dirs[subdir] = dst_subdir
                            table.insert(test_mirrors, { src = subdir, dst = dst_subdir })
                            log.info("Auto-mirroring counterpart subdirectory (structural):", subdir, "->", dst_subdir)
                            found_count = found_count + 1
                        else
                            log.debug("Subdirectory already in mirror list, skipping:", subdir)
                        end
                    else
                        log.debug("Skipping destination directory:", subdir)
                    end
                end
                handle:close()
                log.info("Found", found_count, "counterpart subdirectories for structural refactoring")
            else
                log.error("Failed to execute fd command")
            end
        else
            log.info("Counterpart source parent does not exist:", mirror_src_parent)
        end
        ::continue_structural::
    end
end

--- Compute individual mirrors from directory/file moves.
--- Supports BOTH directions: src/main/java <-> src/test/java
---@param all_changes java.rejactor.FileMove[]
---@param canonical table|nil The canonical transformation (from canonical.detect)
---@param test_mirrors java.rejactor.FileMove[]
---@param test_mirror_dirs table<string, string>
local function compute_individual_mirrors(all_changes, canonical, test_mirrors, test_mirror_dirs)
    for _, change in ipairs(all_changes) do
        -- Skip directory changes that are PARENTS of the canonical old prefix.
        if canonical_mod.is_parent_of_canonical(change, canonical) then
            log.info("Skipping parent-of-canonical directory change (partial rename):", change.src, "->", change.dst)
            goto continue_mirror_loop
        end

        local mirror_src, mirror_dst

        if string_util.contains(change.src, "src/main/java/") then
            mirror_src = change.src:gsub("src/main/java/", "src/test/java/")
            mirror_dst = change.dst:gsub("src/main/java/", "src/test/java/")
        elseif string_util.contains(change.src, "src/test/java/") then
            mirror_src = change.src:gsub("src/test/java/", "src/main/java/")
            mirror_dst = change.dst:gsub("src/test/java/", "src/main/java/")
        end

        if mirror_src and mirror_dst then
            -- For directory moves: mirror the directory directly
            if vim.fn.isdirectory(mirror_src) == 1 then
                if not test_mirror_dirs[mirror_src] then
                    test_mirror_dirs[mirror_src] = mirror_dst
                    table.insert(test_mirrors, { src = mirror_src, dst = mirror_dst })
                    log.info("Auto-mirroring counterpart directory:", mirror_src, "->", mirror_dst)
                end
            -- For file moves: infer directory-level mirror
            elseif change.src:match("%.java$") then
                local src_dir = change.src:match("(.+)/[^/]+$")
                local dst_dir = change.dst:match("(.+)/[^/]+$")

                if src_dir and dst_dir and src_dir ~= dst_dir then
                    local is_subdirectory_move = dst_dir:find("^" .. vim.pesc(src_dir) .. "/")
                        or src_dir:find("^" .. vim.pesc(dst_dir) .. "/")

                    if is_subdirectory_move then
                        -- For subdirectory moves, mirror individual test/main files
                        local mirror_file_src, mirror_file_dst
                        if string_util.contains(change.src, "src/main/java/") then
                            mirror_file_src = change.src:gsub("src/main/java/", "src/test/java/")
                            mirror_file_dst = change.dst:gsub("src/main/java/", "src/test/java/")
                        elseif string_util.contains(change.src, "src/test/java/") then
                            mirror_file_src = change.src:gsub("src/test/java/", "src/main/java/")
                            mirror_file_dst = change.dst:gsub("src/test/java/", "src/main/java/")
                        end

                        if mirror_file_src and vim.fn.filereadable(mirror_file_src) == 1 then
                            if not test_mirror_dirs[mirror_file_src] then
                                test_mirror_dirs[mirror_file_src] = mirror_file_dst
                                table.insert(test_mirrors, { src = mirror_file_src, dst = mirror_file_dst })
                                log.info(
                                    "Auto-mirroring counterpart file (subdirectory move):",
                                    mirror_file_src,
                                    "->",
                                    mirror_file_dst
                                )
                            end
                        else
                            log.debug("No counterpart file for subdirectory move:", mirror_file_src or "nil")
                        end
                    else
                        local mirror_src_dir, mirror_dst_dir
                        if string_util.contains(src_dir, "src/main/java/") then
                            mirror_src_dir = src_dir:gsub("src/main/java/", "src/test/java/")
                            mirror_dst_dir = dst_dir:gsub("src/main/java/", "src/test/java/")
                        elseif string_util.contains(src_dir, "src/test/java/") then
                            mirror_src_dir = src_dir:gsub("src/test/java/", "src/main/java/")
                            mirror_dst_dir = dst_dir:gsub("src/test/java/", "src/main/java/")
                        end

                        if
                            mirror_src_dir
                            and vim.fn.isdirectory(mirror_src_dir) == 1
                            and not test_mirror_dirs[mirror_src_dir]
                        then
                            test_mirror_dirs[mirror_src_dir] = mirror_dst_dir
                            table.insert(test_mirrors, { src = mirror_src_dir, dst = mirror_dst_dir })
                            log.info(
                                "Auto-mirroring counterpart directory (inferred from file move):",
                                mirror_src_dir,
                                "->",
                                mirror_dst_dir
                            )
                        end
                    end
                else
                    log.debug("Skipping mirror for same-directory file rename")
                end
            end
        end
        ::continue_mirror_loop::
    end
end

--- Deduplicate mirrors — keep only the SHALLOWEST mirror per branch.
--- The shallowest mirror's physical move covers ALL files underneath.
---@param test_mirrors java.rejactor.FileMove[]
---@return java.rejactor.FileMove[] deduped_mirrors
---@return table<string, string> deduped_dirs
local function deduplicate_mirrors(test_mirrors)
    if #test_mirrors <= 1 then
        local dirs = {}
        for _, mirror in ipairs(test_mirrors) do
            dirs[mirror.src] = mirror.dst
        end
        return test_mirrors, dirs
    end

    -- Sort by source path length ascending (shallowest first)
    table.sort(test_mirrors, function(a, b)
        return #a.src < #b.src
    end)

    local filtered_mirrors = {}
    for _, mirror in ipairs(test_mirrors) do
        local is_covered_by_shallower = false
        for _, kept in ipairs(filtered_mirrors) do
            if mirror.src:find("^" .. vim.pesc(kept.src) .. "/") then
                is_covered_by_shallower = true
                log.info(
                    "Removing redundant child mirror:",
                    mirror.src,
                    "->",
                    mirror.dst,
                    "(covered by shallower:",
                    kept.src,
                    ")"
                )
                break
            end
        end
        if not is_covered_by_shallower then
            table.insert(filtered_mirrors, mirror)
        end
    end

    local dirs = {}
    for _, mirror in ipairs(filtered_mirrors) do
        dirs[mirror.src] = mirror.dst
    end

    return filtered_mirrors, dirs
end

--- Track opened buffers in mirror sources before physical moves.
---@param test_mirrors java.rejactor.FileMove[]
---@param opened_buffers_to_reopen table Buffer tracking list to append to
local function track_mirror_buffers(test_mirrors, opened_buffers_to_reopen)
    for _, mirror in ipairs(test_mirrors) do
        if vim.fn.isdirectory(mirror.src) == 1 then
            local handle = io.popen("fd -e java . " .. shell_escape(mirror.src))
            if handle then
                for file_path in handle:lines() do
                    local buf_id = buffer_util.find_buf_by_path(file_path)
                    if buf_id then
                        local new_path = file_path:gsub("^" .. vim.pesc(mirror.src), mirror.dst)
                        table.insert(opened_buffers_to_reopen, {
                            old_path = file_path,
                            new_path = new_path,
                            buf_id = buf_id,
                        })
                        log.info("Will reopen buffer:", file_path, "->", new_path)
                    end
                end
                handle:close()
            end
        elseif mirror.src:match("%.java$") then
            local buf_id = buffer_util.find_buf_by_path(mirror.src)
            if buf_id then
                table.insert(opened_buffers_to_reopen, {
                    old_path = mirror.src,
                    new_path = mirror.dst,
                    buf_id = buf_id,
                })
                log.info("Will reopen buffer:", mirror.src, "->", mirror.dst)
            end
        end
    end
end

--- Perform physical file/directory moves for mirrors.
---@param test_mirrors java.rejactor.FileMove[]
local function perform_physical_moves(test_mirrors)
    for _, mirror in ipairs(test_mirrors) do
        -- Create destination directory
        local dst_parent = mirror.dst:match("(.+)/[^/]+$")
        if dst_parent then
            vim.fn.mkdir(dst_parent, "p")
        end

        local is_file = vim.fn.filereadable(mirror.src) == 1
        local is_dir = vim.fn.isdirectory(mirror.src) == 1

        if is_file or is_dir then
            local move_result = os.rename(mirror.src, mirror.dst)
            if move_result then
                log.info("Moved test:", mirror.src, "->", mirror.dst)
            else
                -- Fallback to shell command for cross-device moves
                local cmd = string.format("mv %s %s", shell_escape(mirror.src), shell_escape(mirror.dst))
                local exit_code = os.execute(cmd)
                if exit_code == 0 or exit_code == true then
                    log.info("Moved test (via shell):", mirror.src, "->", mirror.dst)
                else
                    log.error("Failed to move test:", mirror.src, "->", mirror.dst)
                end
            end
        end
    end
end

--- Clean up ALL empty directories in the module's java source trees.
--- Uses `find -type d -empty` (not fd — fd lacks an -empty flag for directories)
--- and removes bottom-up (deepest first).
---@param module_path string|nil
function M.cleanup_empty_dirs(module_path)
    log.info("Cleaning up empty directories...")
    local cleanup_count = 0

    local base_module = module_path or consts.get_project_root()
    local java_roots_to_clean = {}
    for _, root in ipairs({ "src/main/java", "src/test/java" }) do
        local full_root = base_module .. "/" .. root
        if vim.fn.isdirectory(full_root) == 1 then
            table.insert(java_roots_to_clean, full_root)
        end
    end

    for _, java_root in ipairs(java_roots_to_clean) do
        local handle = io.popen("find " .. shell_escape(java_root) .. " -type d -empty 2>/dev/null")
        if handle then
            local empty_dirs = {}
            for dir in handle:lines() do
                table.insert(empty_dirs, dir)
            end
            handle:close()

            -- Remove bottom-up (sort by depth descending so deepest dirs are removed first)
            table.sort(empty_dirs, function(a, b)
                return #a > #b
            end)
            for _, dir in ipairs(empty_dirs) do
                vim.fn.delete(dir, "d")
                log.info("Removed empty directory:", dir)
                cleanup_count = cleanup_count + 1
            end
        end
    end

    if cleanup_count > 0 then
        log.info("Cleaned up", cleanup_count, "empty directories")
    else
        log.info("No empty directories to clean up")
    end
end

--- Main entry point: compute and execute test↔src mirror synchronization.
--- Returns the list of mirrors that were applied (for adding to registered changes).
---@param all_changes java.rejactor.FileMove[]
---@param canonical table|nil The canonical transformation
---@param module_path string|nil
---@param opened_buffers_to_reopen table Buffer tracking list
---@return java.rejactor.FileMove[] test_mirrors Applied mirrors
function M.sync(all_changes, canonical, module_path, opened_buffers_to_reopen)
    local test_mirrors = {}
    local test_mirror_dirs = {}

    -- 1. Detect structural refactorings and compute their mirrors
    local structural_refactorings = detect_structural_refactorings(all_changes)
    compute_structural_mirrors(structural_refactorings, test_mirrors, test_mirror_dirs)

    -- 2. Compute individual mirrors from directory/file moves
    compute_individual_mirrors(all_changes, canonical, test_mirrors, test_mirror_dirs)

    -- 3. Deduplicate mirrors
    test_mirrors, test_mirror_dirs = deduplicate_mirrors(test_mirrors)

    -- 4. Physically move counterpart files/directories
    if #test_mirrors > 0 then
        log.info("Physically moving", #test_mirrors, "counterpart packages to match structure")

        -- Track opened counterpart buffers before moving
        track_mirror_buffers(test_mirrors, opened_buffers_to_reopen)

        -- Perform physical moves
        perform_physical_moves(test_mirrors)

        -- Clean up empty directories after mirroring
        M.cleanup_empty_dirs(module_path)
    end

    return test_mirrors
end

return M
