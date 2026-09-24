-- Test↔Src mirror synchronization.
-- Detects structural refactorings, computes individual mirrors, filters parent-of-canonical,
-- deduplicates mirrors, performs physical moves, and cleans up empty directories.

local M = {}

local string_util = require("utils.string-util")
local consts = require("modules.java.refactor.constants")
local canonical_mod = require("modules.java.refactor.canonical")
local buffer_manager = require("modules.java.refactor.buffer-manager")

local log = consts.log
local shell_escape = consts.shell_escape

-- Suffixes that mark a test class for a production type: Foo -> FooTest / FooTests / FooIT / FooSomethingTest
local TEST_SUFFIXES = { "Tests", "Test", "IT" }

--- Check whether `file_name` is a test counterpart of `type_name`:
--- `Foo.java`, `FooTest.java`, `FooTests.java`, `FooIT.java` or `Foo<Infix>Test.java` where `<Infix>` starts with an
--- uppercase letter (so `CardUtilityTest.java` is NOT a counterpart of `CardUtil`).
---@param file_name string
---@param type_name string
---@return boolean
local function is_test_counterpart_name(file_name, type_name)
    if file_name == type_name .. ".java" then
        return true
    end
    local rest = file_name:match("^" .. vim.pesc(type_name) .. "(.+)%.java$")
    if not rest then
        return false
    end
    for _, suffix in ipairs(TEST_SUFFIXES) do
        local infix = rest:match("^(.*)" .. suffix .. "$")
        if infix and (infix == "" or infix:match("^%u")) then
            return true
        end
    end
    return false
end

--- Find the counterpart files of a single file move.
--- main -> test: the same-named file plus the test classes named after the type (FooTest, FooIT, ...); when the
--- type is renamed the counterparts are renamed the same way (CardUtilTest -> CardHelperTest).
--- test -> main: only the same-named file — a test-only reorganisation must never relocate production code.
---@param change java.rejactor.FileMove
---@param mirror_file_src string counterpart path with the same file name as `change.src`
---@param mirror_file_dst string counterpart path with the same file name as `change.dst`
---@return java.rejactor.FileMove[]
local function find_counterpart_files(change, mirror_file_src, mirror_file_dst)
    local counterparts = {}
    if vim.fn.filereadable(mirror_file_src) == 1 then
        table.insert(counterparts, { src = mirror_file_src, dst = mirror_file_dst })
    end
    if not string_util.contains(change.src, "src/main/java/") then
        return counterparts
    end

    local old_type_name = change.src:match("([^/]+)%.java$")
    local new_type_name = change.dst:match("([^/]+)%.java$")
    local mirror_src_dir = mirror_file_src:match("(.+)/[^/]+$")
    local mirror_dst_dir = mirror_file_dst:match("(.+)/[^/]+$")
    if not old_type_name or not new_type_name or not mirror_src_dir or not mirror_dst_dir then
        return counterparts
    end
    if vim.fn.isdirectory(mirror_src_dir) ~= 1 then
        return counterparts
    end

    for _, entry in ipairs(vim.fn.readdir(mirror_src_dir)) do
        if entry ~= old_type_name .. ".java" and is_test_counterpart_name(entry, old_type_name) then
            local dst_entry = new_type_name .. entry:sub(#old_type_name + 1)
            table.insert(
                counterparts,
                { src = mirror_src_dir .. "/" .. entry, dst = mirror_dst_dir .. "/" .. dst_entry }
            )
        end
    end
    return counterparts
end

--- Register file-level counterpart mirrors for a single file move.
--- Each mirror remembers the change it belongs to (`counterpart_of_src`) so that, once physically moved,
--- it can be attached to that change as `counterparts` for the command builder.
---@param change java.rejactor.FileMove
---@param reason string Log label
---@param test_mirrors java.rejactor.FileMove[]
---@param test_mirror_dirs table<string, string>
local function add_file_counterpart_mirrors(change, reason, test_mirrors, test_mirror_dirs)
    local mirror_file_src, mirror_file_dst
    if string_util.contains(change.src, "src/main/java/") then
        mirror_file_src = change.src:gsub("src/main/java/", "src/test/java/")
        mirror_file_dst = change.dst:gsub("src/main/java/", "src/test/java/")
    elseif string_util.contains(change.src, "src/test/java/") then
        mirror_file_src = change.src:gsub("src/test/java/", "src/main/java/")
        mirror_file_dst = change.dst:gsub("src/test/java/", "src/main/java/")
    end
    if not mirror_file_src or not mirror_file_dst then
        return
    end

    local counterparts = find_counterpart_files(change, mirror_file_src, mirror_file_dst)
    if vim.tbl_isempty(counterparts) then
        log.debug("No counterpart file for " .. reason .. ":", mirror_file_src)
        return
    end
    for _, counterpart in ipairs(counterparts) do
        if not test_mirror_dirs[counterpart.src] then
            test_mirror_dirs[counterpart.src] = counterpart.dst
            counterpart.counterpart_of_src = change.src
            table.insert(test_mirrors, counterpart)
            log.info("Auto-mirroring counterpart file (" .. reason .. "):", counterpart.src, "->", counterpart.dst)
        end
    end
end

--- A file move empties its package when every registered `.java` move out of `src_dir` targets `dst_dir` and
--- nothing (Java files or sub-packages) is left behind in `src_dir` (the file manager has already moved the files).
--- Only then may the counterpart directory be mirrored as a whole — for a partial move that would drag unrelated
--- tests along and register a package rename that rewrites every `old.pkg` reference in the module.
---@param src_dir string
---@param dst_dir string
---@param all_changes java.rejactor.FileMove[]
---@return boolean
local function is_whole_directory_move(src_dir, dst_dir, all_changes)
    for _, change in ipairs(all_changes) do
        if change.src:match("%.java$") and change.src:match("(.+)/[^/]+$") == src_dir then
            if change.dst:match("(.+)/[^/]+$") ~= dst_dir then
                return false
            end
        end
    end
    if vim.fn.isdirectory(src_dir) == 1 then
        for _, entry in ipairs(vim.fn.readdir(src_dir)) do
            if entry:match("%.java$") or vim.fn.isdirectory(src_dir .. "/" .. entry) == 1 then
                return false
            end
        end
    end
    return true
end

--- Move a file or directory. Merges into an already existing destination directory (a plain
--- `mv src existing_dir` would nest `src` INSIDE it) and never overwrites an existing file.
---@param src string
---@param dst string
---@return boolean success
local function move_path(src, dst)
    local src_is_dir = vim.fn.isdirectory(src) == 1

    if vim.fn.isdirectory(dst) == 1 then
        if not src_is_dir then
            log.error("Refusing to move file onto existing directory:", src, "->", dst)
            return false
        end
        log.info("Destination directory exists, merging:", src, "->", dst)
        local ok = true
        for _, entry in ipairs(vim.fn.readdir(src)) do
            if not move_path(src .. "/" .. entry, dst .. "/" .. entry) then
                ok = false
            end
        end
        if ok and vim.fn.delete(src, "d") ~= 0 then
            log.warn("Could not remove merged source directory:", src)
        end
        return ok
    end
    if vim.fn.filereadable(dst) == 1 then
        log.error("Refusing to overwrite existing file:", src, "->", dst)
        return false
    end

    -- Create destination directory
    local dst_parent = dst:match("(.+)/[^/]+$")
    if dst_parent then
        vim.fn.mkdir(dst_parent, "p")
    end

    if os.rename(src, dst) then
        return true
    end
    -- Fallback to shell command for cross-device moves
    local cmd = string.format("mv %s %s", shell_escape(src), shell_escape(dst))
    local exit_code = os.execute(cmd)
    if exit_code == 0 or exit_code == true then
        log.info("Moved (via shell):", src, "->", dst)
        return true
    end
    return false
end

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
                for raw_subdir in handle:lines() do
                    local subdir = raw_subdir:gsub("/$", "")
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
                        add_file_counterpart_mirrors(change, "subdirectory move", test_mirrors, test_mirror_dirs)
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
                            if is_whole_directory_move(src_dir, dst_dir, all_changes) then
                                -- Every file left the package: the counterpart package follows as a whole
                                test_mirror_dirs[mirror_src_dir] = mirror_dst_dir
                                table.insert(test_mirrors, { src = mirror_src_dir, dst = mirror_dst_dir })
                                log.info(
                                    "Auto-mirroring counterpart directory (inferred from file move):",
                                    mirror_src_dir,
                                    "->",
                                    mirror_dst_dir
                                )
                            else
                                -- Only some files leave the package: mirror just the counterpart files of the
                                -- moved type. Mirroring the whole counterpart directory would drag unrelated
                                -- tests along and register a package rename that rewrites every `old.pkg`
                                -- reference in the module (breaking the files that stayed in the old package).
                                log.info(
                                    "Partial package move, mirroring counterpart files only (not the directory):",
                                    change.src,
                                    "->",
                                    change.dst
                                )
                                add_file_counterpart_mirrors(
                                    change,
                                    "partial package move",
                                    test_mirrors,
                                    test_mirror_dirs
                                )
                            end
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
    buffer_manager.track_buffers_into(test_mirrors, opened_buffers_to_reopen)
end

--- Perform physical file/directory moves for mirrors.
--- Returns only the mirrors that were actually moved: a mirror that failed (or no longer exists) must not be
--- registered as a change, or the package sed would rewrite files that never left the old location.
---@param test_mirrors java.rejactor.FileMove[]
---@return java.rejactor.FileMove[] applied_mirrors
local function perform_physical_moves(test_mirrors)
    local applied_mirrors = {}
    for _, mirror in ipairs(test_mirrors) do
        local is_file = vim.fn.filereadable(mirror.src) == 1
        local is_dir = vim.fn.isdirectory(mirror.src) == 1

        if is_file or is_dir then
            if move_path(mirror.src, mirror.dst) then
                log.info("Moved test:", mirror.src, "->", mirror.dst)
                table.insert(applied_mirrors, mirror)
            else
                log.error("Failed to move test (left out of refactoring, fix manually):", mirror.src, "->", mirror.dst)
            end
        else
            log.warn("Mirror source no longer exists, skipping:", mirror.src)
        end
    end
    return applied_mirrors
end

--- Attach applied file-level counterpart mirrors to the change they belong to (`change.counterparts`),
--- so the command builder can fix type references inside them at their new location.
---@param all_changes java.rejactor.FileMove[]
---@param applied_mirrors java.rejactor.FileMove[]
local function attach_counterparts(all_changes, applied_mirrors)
    local changes_by_src = {}
    for _, change in ipairs(all_changes) do
        changes_by_src[change.src] = change
    end
    for _, mirror in ipairs(applied_mirrors) do
        local owner = mirror.counterpart_of_src and changes_by_src[mirror.counterpart_of_src]
        if owner then
            owner.counterparts = owner.counterparts or {}
            table.insert(owner.counterparts, { src = mirror.src, dst = mirror.dst })
        end
        mirror.counterpart_of_src = nil
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

        -- Perform physical moves (keep only the mirrors that really moved)
        test_mirrors = perform_physical_moves(test_mirrors)
        attach_counterparts(all_changes, test_mirrors)

        -- Clean up empty directories after mirroring
        M.cleanup_empty_dirs(module_path)
    end

    return test_mirrors
end

return M
