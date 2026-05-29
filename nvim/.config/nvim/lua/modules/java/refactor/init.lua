-- Java refactor module: batch-fixes moved Java files/packages.
-- Works with file managers: fyler.nvim, neo-tree, oil.nvim, Snacks.rename.
-- For batch processing use: register_change() + process_registerd_changes()
-- For single file use: process_single_file_change()
--
-- Dependencies: ripgrep, fd (rust-based find), sed (gsed on macOS)

local M = {}

local consts = require("modules.java.refactor.constants")
local canonical_mod = require("modules.java.refactor.canonical")
local mirror_sync = require("modules.java.refactor.mirror-sync")
local buffer_manager = require("modules.java.refactor.buffer-manager")
local cmd_builder = require("modules.java.refactor.cmd-builder")
local executor = require("modules.java.refactor.executor")

local log = consts.log
local string_util = require("utils.string-util")
local package_roots = consts.package_roots

-- Test mode: when true, executes commands directly without UI
M.test_mode = false

---@class java.rejactor.FileMove
---@field src string
---@field dst string
---@field siblings? java.rejactor.FileMove[]

local all_registered_changes = {}

---@param src string
---@param dst string
function M.register_change(src, dst)
    log.debug("Registering change:", src, "->", dst)
    table.insert(all_registered_changes, {
        src = src,
        dst = dst,
    })
end

-- ============================================================================
-- Internal: Build all refactoring operations from registered changes.
-- Handles package-move detection, deduplication, and individual file processing.
-- ============================================================================

---@param all_changes java.rejactor.FileMove[]
---@param canonical CanonicalTransformation|nil
---@param module_path string|nil
---@return RefactorOperation[]
local function build_operations(all_changes, canonical, module_path)
    local global_operations = {}

    -- Find all directory/package moves (not files)
    local package_moves = {}
    for _, change in ipairs(all_changes) do
        if not change.src:match("%.java$") then
            if change.src == change.dst then
                log.debug("Skipping no-op directory move (src == dst):", change.src)
            else
                local root_match = nil
                for _, root in ipairs(package_roots) do
                    if string_util.contains(change.src, root) then
                        root_match = root
                        break
                    end
                end

                if root_match then
                    local package_path = vim.split(change.src, root_match)[2]
                    package_path = package_path:gsub("/$", "")

                    -- Skip directory changes that are PARENTS of the canonical old prefix
                    if canonical then
                        if
                            package_path ~= canonical.old_prefix
                            and canonical.old_prefix:find("^" .. vim.pesc(package_path) .. "/")
                        then
                            log.info(
                                "Skipping parent-of-canonical package move (partial rename):",
                                package_path,
                                "(canonical:",
                                canonical.old_prefix,
                                ")"
                            )
                            goto continue_pkg_loop
                        end
                    end

                    -- Calculate depth: com/example = 2 levels (1 slash + 1)
                    local slash_count = select(2, package_path:gsub("/", ""))
                    local package_depth = slash_count + 1
                    table.insert(package_moves, {
                        change = change,
                        depth = package_depth,
                        package_path = package_path,
                    })
                    log.debug("Found package move:", package_path, "depth:", package_depth)
                end
            end
        end
        ::continue_pkg_loop::
    end

    if #package_moves == 0 then
        log.info("No explicit directory moves found, will process files individually")
    end

    -- Filter out overly broad moves (depth < 2)
    local valid_moves = {}
    for _, move in ipairs(package_moves) do
        if move.depth >= 2 then
            table.insert(valid_moves, move)
        else
            log.warn("Skipping overly broad package move at depth", move.depth, ":", move.package_path)
        end
    end

    if #valid_moves == 0 then
        log.info("No valid package moves found (all had depth < 2)")
    end

    -- Deduplicate overlapping package moves (keep shallowest per branch)
    if #valid_moves > 1 then
        table.sort(valid_moves, function(a, b)
            return a.depth < b.depth
        end)

        local deduped_moves = {}
        for _, move in ipairs(valid_moves) do
            local is_covered = false
            local src_pkg = move.package_path:gsub("/", ".")
            for _, kept in ipairs(deduped_moves) do
                local kept_pkg = kept.package_path:gsub("/", ".")
                if src_pkg:find("^" .. vim.pesc(kept_pkg) .. "%.") or src_pkg == kept_pkg then
                    is_covered = true
                    log.info("Removing redundant child package move:", src_pkg, "(already covered by:", kept_pkg, ")")
                    break
                end
            end
            if not is_covered then
                table.insert(deduped_moves, move)
            end
        end
        valid_moves = deduped_moves
        log.info("After deduplication:", #valid_moves, "package moves remaining")
    end

    -- Sort by depth (deepest first) for processing
    table.sort(valid_moves, function(a, b)
        return a.depth > b.depth
    end)

    -- Process all valid package moves or file changes
    if #valid_moves > 0 then
        log.info("Processing", #valid_moves, "package-level refactorings")

        for _, move in ipairs(valid_moves) do
            move.change.siblings = cmd_builder.get_all_src_siblings(move.change, all_changes)
            if move.change.siblings and #move.change.siblings > 0 then
                log.debug("Found", #move.change.siblings, "siblings for", move.change.src)
            end
            local operations = cmd_builder.build_fix_commands(move.change, module_path)
            if operations then
                log.debug("Adding", #operations, "operations for:", move.change.dst)
                for _, op in ipairs(operations) do
                    table.insert(global_operations, op)
                end
            end
        end
    else
        -- Process all file changes individually
        log.info("No valid package moves found, processing files individually")
        for _, value in ipairs(all_changes) do
            if value.src:match("%.java$") then
                value.siblings = cmd_builder.get_all_src_siblings(value, all_changes)
                if value.siblings and #value.siblings > 0 then
                    log.debug("Found", #value.siblings, "siblings for", value.src)
                end
                local operations = cmd_builder.build_fix_commands(value, module_path)
                if operations then
                    log.debug("Adding", #operations, "operations for:", value.dst)
                    for _, op in ipairs(operations) do
                        table.insert(global_operations, op)
                    end
                end
            end
        end
    end

    return global_operations
end

-- ============================================================================
-- Public API
-- ============================================================================

--- Main orchestration: process all registered changes.
--- Steps: detect module → track buffers → canonical correction → mirror sync →
--- build commands → delete old buffers → execute → reopen buffers → cleanup.
function M.process_registerd_changes()
    if vim.tbl_isempty(all_registered_changes) then
        log.warn("No registered changes to process")
        if not M.test_mode then
            vim.notify("No any registered changes")
        end
        return nil
    end

    log.info("Starting processing of", #all_registered_changes, "registered changes")
    log.debug("All registered changes:", all_registered_changes)

    -- Step 1: Detect module scope
    local module_path = nil
    if all_registered_changes[1] then
        module_path = consts.detect_module_path(all_registered_changes[1].src)
        if module_path then
            log.info("==============================================")
            log.info("DETECTED MODULE SCOPE:", module_path)
            log.info("All refactoring operations will be limited to this module only")
            log.info("==============================================")
        else
            log.warn("Could not detect module path, operations will be project-wide")
        end
    end

    -- Step 2: Track opened buffers before any moves
    local opened_buffers_to_reopen = buffer_manager.track_buffers(all_registered_changes)

    -- Step 3: Canonical transformation detection and destination correction
    local canonical = nil
    if #all_registered_changes > 1 then
        canonical = canonical_mod.detect(all_registered_changes)
        if canonical then
            canonical_mod.correct_destinations(all_registered_changes, canonical)
        end
    end

    -- Step 4: Mirror sync (test↔src physical moves + cleanup)
    local test_mirrors = mirror_sync.sync(all_registered_changes, canonical, module_path, opened_buffers_to_reopen)

    -- Add test mirrors to registered changes for package declaration updates
    for _, mirror in ipairs(test_mirrors) do
        table.insert(all_registered_changes, mirror)
        log.debug("Added test mirror for refactoring:", mirror.src, "->", mirror.dst)
    end
    if #test_mirrors > 0 then
        log.info("Added", #test_mirrors, "test package mirrors for declaration updates")
    end

    -- Step 5: Build refactoring commands (package moves + file moves)
    local global_operations = build_operations(all_registered_changes, canonical, module_path)
    log.info("Total operations to execute:", #global_operations)

    -- Step 6: Separate shell and Lua operations
    local shell_cmds, lua_operations = executor.separate_operations(global_operations)

    -- Use '; ' to join commands so one failure doesn't abort subsequent operations
    local global_cmd_run = table.concat(shell_cmds, " ; ")
    log.debug("Full shell command chain length:", #global_cmd_run, "characters")

    -- Step 7: Execute in test mode (directly, without UI)
    if M.test_mode then
        local success = executor.execute_test_mode(shell_cmds, lua_operations)
        all_registered_changes = {}
        return success
    end

    -- Step 8: Delete old buffers before applying changes
    buffer_manager.delete_old_buffers(opened_buffers_to_reopen)

    -- Step 9: Build composite callback for post-execution
    local composite_callback = function()
        -- Execute Lua operations after shell commands complete
        if #lua_operations > 0 then
            log.info("Executing", #lua_operations, "Lua operations after shell commands")
            for _, op in ipairs(lua_operations) do
                log.info("Executing:", op.description)
                local success = op.fn()
                if not success then
                    log.error("Lua operation failed:", op.description)
                end
            end
            log.info("All Lua operations completed")
        end

        -- Reopen buffers from new locations
        buffer_manager.reopen_buffers(opened_buffers_to_reopen)

        -- Final empty directory cleanup
        mirror_sync.cleanup_empty_dirs(module_path)
    end

    -- Step 10: Execute shell commands via terminal, then run composite callback
    if #shell_cmds > 0 then
        executor.run_cmd(global_cmd_run, composite_callback)
    else
        log.info("No shell commands to execute, running Lua operations directly")
        vim.schedule(function()
            composite_callback()
        end)
    end

    log.info("Clearing registered changes")
    all_registered_changes = {}
    return true
end

---@param src string
---@param dst string
function M.process_single_file_change(src, dst)
    log.info("Processing single file change:", src, "->", dst)
    M.register_change(src, dst)
    M.process_registerd_changes()
end

return M
