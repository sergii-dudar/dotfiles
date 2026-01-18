-- INFO:: Module to batch fixing moved java file/files with external plugins, like: [ neo-tree, nvim-tree, oil.nvim, Snacks.rename ], for batch - [ fyler.nvim ]
--  currently module developed mostly for batch processing by `fyler.nvim`, for signle file change per action use method - process_single_file_change.
--  THIS MODULE IS NOT FINISHED YET, AND IN VERY EARLY DRAFT VERSION, BUT I'M USING IT HAVILY IN DAILY WORK AS JAVA DEV!

-- TODO:
--  󰱒 Batch move processing of java files from dir A to dir B with proper types usage resolving.
--  󰱒 Batch move processing of java files from dir A to dir B,C,D... with proper types usage resolving.

-- NOTE: - Dependencies: ripgrep, fd, sed, work/java/remane/fix-java-sibling-usage.sh, work/java/remane/fix-old-imports.sh

local M = {}

local util = require("utils.common-util")
local string_util = require("utils.string-util")
local spinner = require("utils.ui.spinner")
local list_util = require("utils.list-util")
local buffer_util = require("utils.buffer-util")
local logging = require("utils.logging-util")
local global = require("utils.global-util")

-- Create logger for java refactoring
local log = logging.new({ name = "java-refactor", filename = "java-refactor.log" })

-- Test mode: when true, executes commands directly without UI
M.test_mode = false

---@class java.rejactor.FileMove
---@field src string
---@field dst string
---@field siblings? java.rejactor.FileMove[]

local current_term_win = nil
---@param cmd_args string
local function run_cmd(cmd_args, on_success_callback)
    log.info("Starting Java refactoring command execution")
    log.debug("Command:", cmd_args)
    -- vim.notify("🚀 Java Refactoring Started", vim.log.levels.INFO)
    spinner.start("🚀 " .. "Java Refactoring...")

    vim.cmd("botright split")
    -- vim.cmd("resize 15")

    util.close_window_if_exists(current_term_win) -- close prev term win if opened
    current_term_win = vim.api.nvim_get_current_win()
    local term_buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_win_set_buf(current_term_win, term_buf)

    local job_id = vim.fn.jobstart(cmd_args, {
        term = true,
        stdout_buffered = false,
        stderr_buffered = false,
        on_exit = function(_, code)
            if code == 0 then
                log.info("Java refactoring completed successfully")
            else
                log.error("Java refactoring failed with exit code:", code)
            end
            spinner.stop(code == 0, "Java refactoring")
            if code == 0 then
                util.close_window_if_exists(current_term_win)
                -- Call success callback if provided
                if on_success_callback then
                    vim.schedule(function()
                        on_success_callback()
                    end)
                end
            end
        end,
    })

    vim.cmd("startinsert")
    if job_id <= 0 then
        log.error("Failed to start command via jobstart()")
        vim.notify("Failed to start cmd via jobstart()", vim.log.levels.ERROR)
    else
        log.debug("Command started with job_id:", job_id)
    end
end

local main_dir = "src/main/java/"
local test_dir = "src/test/java/"
local main_resource_dir = "src/main/resources/"
local test_resource_dir = "src/test/resources/"
local package_roots = { main_dir, test_dir, main_resource_dir, test_resource_dir }
-- local test_path = "."
-- local test_path = "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem" -- TODO: change to . after finish
-- IMPORTANT: Get project root dynamically, not at module load time
-- This allows tests to change directory before using the module
local function get_project_root()
    return vim.fn.getcwd()
end

-- Detect OS and set appropriate sed -i flag
-- macOS (BSD sed) requires -i '' while Linux (GNU sed) uses -i
local sed_inplace_flag = vim.loop.os_uname().sysname == "Darwin" and "-i ''" or "-i"
log.debug("Detected OS:", vim.loop.os_uname().sysname, "- using sed flag:", sed_inplace_flag)

---@param result_cmds table
---@param root string
---@param context java.rejactor.FileMove
local build_fix_java_file_after_change_cmds = function(result_cmds, root, context)
    log.debug("Building fix commands for file move:", context.src, "->", context.dst)
    -- TODO: use `local process_root_path = vim.fs.joinpath(get_project_root(), [empty or package sub path in case multimodule, or sub module], root)`

    local src = context.src
    local dst = context.dst

    -- com/example/EmployeeManagementSystem/service/ServiceEmployee
    local package_src_path = vim.split(src, root)[2]:gsub("%.java", "")
    local package_dst_path = vim.split(dst, root)[2]:gsub("%.java", "")

    -- com.example.EmployeeManagementSystem.service.ServiceEmployee
    local package_src_classpath = package_src_path:gsub("/", ".")
    local package_dst_classpath = package_dst_path:gsub("/", ".")

    -- com\.example\.EmployeeManagementSystem\.service\.ServiceEmployee
    local package_src_classpath_escaped = package_src_classpath:gsub("%.", "\\.")

    -- com\/example\/EmployeeManagementSystem\/service\/ServiceEmployee
    local package_src_path_escaped = package_src_path:gsub("/", "\\/")
    local package_dst_path_escaped = package_dst_path:gsub("/", "\\/")

    -- ServiceEmployee
    local old_type_name = package_src_path:match("([^/]+)$")
    local new_type_name = package_dst_path:match("([^/]+)$")

    log.debug("Type rename:", old_type_name, "->", new_type_name)

    -- com.example.EmployeeManagementSystem.service
    local package_declaration_src = package_src_classpath:match("(.+)%.%w+$")
    local package_declaration_dst = package_dst_classpath:match("(.+)%.%w+$")

    if not package_declaration_src or not package_declaration_dst then
        log.error("Failed to extract package declarations")
        log.error("package_src_classpath:", package_src_classpath)
        log.error("package_dst_classpath:", package_dst_classpath)
        log.error("package_declaration_src:", package_declaration_src)
        log.error("package_declaration_dst:", package_declaration_dst)
        return {}
    end

    log.debug("Package change:", package_declaration_src, "->", package_declaration_dst)

    -- com\.example\.EmployeeManagementSystem\.service
    local package_declaration_src_escaped = package_declaration_src:gsub("%.", "\\.")

    -- java file rename fixes (priority is very important):
    -- ==========================================================================
    -- ==========================================================================
    -- 1. fix type decration in changed file.
    local fix_type_declaration_cmd = string.format(
        "sed %s -E 's/(class|interface|enum|record) %s[[:space:]]/\\1 %s /g' %s",
        sed_inplace_flag,
        old_type_name,
        new_type_name,
        dst
    )
    -- vim.notify(fix_type_declaration_cmd)
    table.insert(result_cmds, fix_type_declaration_cmd)

    -- ==========================================================================
    -- ==========================================================================
    -- 2. fix type symbols (simple java name) where type is imported.
    local fix_type_symbols_where_imported = string.format(
        "rg --color=never -l 'import\\s+%s' "
            .. get_project_root()
            .. " | xargs sed %s -E 's/([[:space:],;(}<])%s([[:space:],;(}\\.>])/\\1%s\\2/g' || echo 'skipped'",
        package_src_classpath_escaped,
        sed_inplace_flag,
        old_type_name,
        new_type_name
    )
    -- vim.notify(fix_type_symbols_where_imported)
    table.insert(result_cmds, fix_type_symbols_where_imported)

    -- ==========================================================================
    -- ==========================================================================
    -- 2.1. fix imports of siblings java files (in case moved to other packages)
    if context.siblings and not vim.tbl_isempty(context.siblings) then
        log.debug("Processing", #context.siblings, "sibling files")
        for _, sibling in ipairs(context.siblings) do
            -- com/example/EmployeeManagementSystem/service/ServiceEmployee
            local sibling_package_src_path = vim.split(sibling.src, root)[2]:gsub("%.java", "")
            local sibling_package_dst_path = vim.split(sibling.dst, root)[2]:gsub("%.java", "")

            -- com.example.EmployeeManagementSystem.service.ServiceEmployee
            local sibling_package_dst_classpath = sibling_package_dst_path:gsub("/", ".")

            -- ServiceEmployee
            local sibling_old_type_name = sibling_package_src_path:match("([^/]+)$")
            local sibling_new_type_name = sibling_package_dst_path:match("([^/]+)$")

            -- com.example.EmployeeManagementSystem.service
            local sibling_package_declaration_dst = sibling_package_dst_classpath:match("(.+)%.%w+$")

            local fix_type_sibling_where_using = string.format(
                '"%s" "%s" "%s" "%s" "%s" "%s"',
                global.dotfiles_path("work/java/remane/fix-java-sibling-usage.sh"),
                dst, -- FILE_PATH_TO_APPLY_FIX
                sibling_package_declaration_dst, -- NEW_PACKAGE (sibling's destination)
                sibling_old_type_name, -- OLD_TYPE_NAME
                sibling_new_type_name, -- NEW_TYPE_NAME
                package_declaration_dst -- FILE_DST_PACKAGE (current file's destination)
            )
            -- vim.notify(fix_type_sibling_where_using)
            table.insert(result_cmds, fix_type_sibling_where_using)
        end
    end

    -- ==========================================================================
    -- ==========================================================================
    -- 3. fix type full qualified names (acroll all files - java,yaml,properties etc)
    local fix_type_full_qualified_names = string.format(
        "rg --color=never -l '%s' "
            .. get_project_root()
            .. " | xargs sed %s -E 's/%s([;.$\"]|$)/%s\\1/g' || echo 'skipped'",
        -- sed %s -E 's/ServiceEmployee([^[:alnum:]_]|$)/ServiceEmployeeUser\1/g'
        package_src_classpath_escaped,
        sed_inplace_flag,
        package_src_classpath_escaped,
        package_dst_classpath
    )
    -- vim.notify(fix_type_symbols_where_imported)
    table.insert(result_cmds, fix_type_full_qualified_names)

    -- ==========================================================================
    -- ==========================================================================
    -- 4. fix packaged decration in changed file.
    local fix_package_declaration = string.format(
        "sed %s -E 's/package[[:space:]]+%s;/package %s;/g' %s",
        sed_inplace_flag,
        package_declaration_src_escaped,
        package_declaration_dst,
        dst
    )
    -- vim.notify(fix_package_declaration)
    table.insert(result_cmds, fix_package_declaration)

    -- ==========================================================================
    -- ==========================================================================
    -- 4.1. Remove imports from the same package (they're unnecessary)
    local package_declaration_dst_escaped = package_declaration_dst:gsub("%.", "\\.")
    local remove_same_package_imports =
        string.format("sed %s '/^import %s\\./d' %s", sed_inplace_flag, package_declaration_dst_escaped, dst)
    table.insert(result_cmds, remove_same_package_imports)

    -- ==========================================================================
    -- ==========================================================================
    -- 5. add import declarations of the new class name to the classes of the old folder
    -- OLD_DIR="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service"
    -- OLD_PACKAGE="com.example.EmployeeManagementSystem.service"
    -- NEW_FILE_PATH="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"

    -- Build list of sibling type names (files being moved from same directory)
    local sibling_types = {}
    if context.siblings and not vim.tbl_isempty(context.siblings) then
        for _, sibling in ipairs(context.siblings) do
            local sibling_type = sibling.src:match("([^/]+)%.java$")
            if sibling_type then
                table.insert(sibling_types, sibling_type)
                log.debug("Sibling type being moved:", sibling_type)
            end
        end
    end
    local sibling_types_str = table.concat(sibling_types, ",")

    local fix_old_file_imports = string.format(
        '"%s" "%s" "%s" "%s" "%s" "%s" "%s" "%s"',
        global.dotfiles_path("work/java/remane/fix-old-imports.sh"),
        src:match("(.+)/[^/]+$"), -- OLD_DIR
        package_declaration_src, -- OLD_PACKAGE
        package_declaration_dst, -- NEW_PACKAGE
        dst, -- NEW_FILE_PATH
        old_type_name, -- OLD_TYPE_NAME
        new_type_name, -- NEW_TYPE_NAME
        sibling_types_str -- SIBLING_TYPES (comma-separated)
    )
    log.debug("Import fix command with siblings:", sibling_types_str)
    -- vim.notify(fix_old_file_imports)
    table.insert(result_cmds, fix_old_file_imports)
    -- ==========================================================================
    -- ==========================================================================
    -- 6. fix file path/resources path

    local fix_file_paht_declaration = string.format(
        "rg --color=never -l '%s' "
            .. get_project_root()
            .. " | xargs sed %s -E 's/%s([;.\"]|$)/%s\\1/g' || echo 'skipped'",
        package_src_path_escaped,
        sed_inplace_flag,
        package_src_path_escaped,
        package_dst_path_escaped
    )
    -- vim.notify(fix_file_paht_declaration)
    table.insert(result_cmds, fix_file_paht_declaration)
end

---@param result_cmds table
---@param root string
---@param context java.rejactor.FileMove
local build_fix_java_package_after_change_cmds = function(result_cmds, root, context)
    log.debug("Building fix commands for package move:", context.src, "->", context.dst)
    local src = context.src
    local dst = context.dst

    -- com/example/EmployeeManagementSystem/service
    local package_src_path = vim.split(src, root)[2]
    local package_dst_path = vim.split(dst, root)[2]

    -- Remove trailing slashes if present
    package_src_path = package_src_path:gsub("/$", "")
    package_dst_path = package_dst_path:gsub("/$", "")

    -- com.example.EmployeeManagementSystem.service
    local package_src_classpath = package_src_path:gsub("/", ".")
    local package_dst_classpath = package_dst_path:gsub("/", ".")

    log.debug("Package rename (directory):", package_src_classpath, "->", package_dst_classpath)

    -- CRITICAL: For root package changes, we need to be very specific
    -- Don't do simple replacements like "com" -> "ua" which can break things
    -- Only do full package path replacements

    -- Minimum package depth check - avoid overly broad replacements
    local src_depth = select(2, package_src_classpath:gsub("%.", "")) + 1
    if src_depth < 2 then
        log.warn("Skipping overly broad package replacement:", package_src_classpath, "-> only affects depth", src_depth)
        log.warn("This would be too dangerous. Please move more specific subdirectories instead.")
        return
    end

    -- com\.example\.EmployeeManagementSystem\.service
    local package_src_classpath_escaped = package_src_classpath:gsub("%.", "\\.")

    -- com\/example\/EmployeeManagementSystem\/service\/ServiceEmployee
    local package_src_path_escaped = package_src_path:gsub("/", "\\/")
    local package_dst_path_escaped = package_dst_path:gsub("/", "\\/")

    -- java package rename fixes (priority is very important):
    -- ==========================================================================
    -- ==========================================================================
    -- 1. fix package full qualified names (across all files - java,yaml,properties etc)
    local fix_package_full_qualified_names = string.format(
        "rg --color=never -l '%s' "
            .. get_project_root()
            .. " | xargs sed %s -E 's/%s([;.$\"]|$)/%s\\1/g' || echo 'skipped'",
        package_src_classpath_escaped,
        sed_inplace_flag,
        package_src_classpath_escaped,
        package_dst_classpath
    )
    log.debug("Package replacement command:", fix_package_full_qualified_names)
    table.insert(result_cmds, fix_package_full_qualified_names)

    -- ==========================================================================
    -- ==========================================================================
    -- 2. fix package path/resources path
    local fix_file_paht_declaration = string.format(
        "rg --color=never -l '%s' "
            .. get_project_root()
            .. " | xargs sed %s -E 's/%s([;.\"\\/]|$)/%s\\1/g' || echo 'skipped'",
        package_src_path_escaped,
        sed_inplace_flag,
        package_src_path_escaped,
        package_dst_path_escaped
    )
    table.insert(result_cmds, fix_file_paht_declaration)
end

---@param context java.rejactor.FileMove
---@return table
local build_fix_java_proj_after_change_cmds = function(context)
    local result_cmds = {}
    local is_dir = util.is_dir(context.dst)
    local is_file = util.is_file(context.dst)
    for _, root in ipairs(package_roots) do
        -- TODO: after applying on relative path, rg search need apply ". root" to take in account test, main, resourses
        if string_util.contains(context.src, root) and string_util.contains(context.dst, root) then
            if is_file then
                -- Buffer management is now handled at batch level in process_registerd_changes()
                -- See lines 454-499 (tracking) and 1032-1087 (delete/reopen)
                build_fix_java_file_after_change_cmds(result_cmds, root, context)
            elseif is_dir then
                build_fix_java_package_after_change_cmds(result_cmds, root, context)
            end
        end
    end
    return result_cmds
end

--- Fix java project after remaning java file, or package name
---@param context java.rejactor.FileMove
---@return string|nil
local build_fix_java_proj_after_change_cmd = function(context)
    if not context.src:match("src/.*/java/") then
        return nil
    end
    if not context.dst:match("src/.*/java/") then
        return nil
    end
    local cmds = build_fix_java_proj_after_change_cmds(context) --  -- src, dst
    -- dd(cmds)
    local cmd_to_run = table.concat(cmds, " && ")
    -- dd(cmd_to_run)

    -- vim.notify(cmd_to_run)
    return cmd_to_run
    -- run_cmd(cmd_to_run)
end

local all_registered_changes = {}

---@param src string
---@param dst string
M.register_change = function(src, dst)
    log.debug("Registering change:", src, "->", dst)
    table.insert(all_registered_changes, {
        src = src,
        dst = dst,
    })
end

---@param context java.rejactor.FileMove
---@param all_changes java.rejactor.FileMove[]
---@return java.rejactor.FileMove[]
local get_all_src_siblings = function(context, all_changes)
    local context_src_dir = context.src:match("(.+)/[^/]+$")
    -- print(context_src_dir)
    local context_src_siblings = {}
    for _, value in ipairs(all_changes) do
        local current_src_dir = value.src:match("(.+)/[^/]+$")
        if context_src_dir == current_src_dir and context.dst ~= value.dst then
            -- print(current_src_dir)
            table.insert(context_src_siblings, value)
        end
    end
    return context_src_siblings
end

M.process_registerd_changes = function()
    if vim.tbl_isempty(all_registered_changes) then
        log.warn("No registered changes to process")
        if not M.test_mode then
            vim.notify("No any registered changes")
        end
        return nil
    end

    log.debug(">>>>>+++++++++++++++++++++++++++++++++")
    log.debug("first: " .. all_registered_changes[1].src)
    log.debug("last: " .. all_registered_changes[#all_registered_changes].src)
    log.debug("all: ")
    for _, value in ipairs(all_registered_changes) do
        log.debug(value.src)
    end
    log.debug("<<<<<+++++++++++++++++++++++++++++++++")

    log.info("Starting processing of", #all_registered_changes, "registered changes")
    log.debug("All registered changes:", all_registered_changes)

    -- ENHANCEMENT: Track opened buffers before changes
    -- Save list of currently opened Java file buffers that will be moved
    -- After changes, we'll reopen them from new locations
    local opened_buffers_to_reopen = {}
    log.info("Tracking opened buffers before applying changes...")

    for _, change in ipairs(all_registered_changes) do
        -- Check if this is a directory move - need to find all files under it
        if vim.fn.isdirectory(change.src) == 1 then
            -- Find all Java files in this directory
            log.debug("Scanning directory for open buffers:", change.src)
            local handle = io.popen("find '" .. change.src .. "' -name '*.java' 2>/dev/null")
            if handle then
                local file_count = 0
                for file_path in handle:lines() do
                    file_count = file_count + 1
                    log.debug("Checking file:", file_path)
                    local buf_id = buffer_util.find_buf_by_path(file_path)
                    if buf_id then
                        -- Calculate new path for this file
                        local new_path = file_path:gsub("^" .. vim.pesc(change.src), change.dst)
                        table.insert(opened_buffers_to_reopen, {
                            old_path = file_path,
                            new_path = new_path,
                            buf_id = buf_id
                        })
                        log.info("Will reopen buffer:", file_path, "->", new_path)
                    else
                        log.debug("File not open in buffer:", file_path)
                    end
                end
                handle:close()
                log.debug("Scanned", file_count, "files in", change.src)
            end
        elseif change.src:match("%.java$") then
            -- Single file move
            log.debug("Checking single file:", change.src)
            local buf_id = buffer_util.find_buf_by_path(change.src)
            if buf_id then
                table.insert(opened_buffers_to_reopen, {
                    old_path = change.src,
                    new_path = change.dst,
                    buf_id = buf_id
                })
                log.info("Will reopen buffer:", change.src, "->", change.dst)
            else
                log.debug("File not open in buffer:", change.src)
            end
        end
    end

    if #opened_buffers_to_reopen > 0 then
        log.info("Found", #opened_buffers_to_reopen, "opened buffers to reopen after changes")
    else
        log.info("No opened buffers found for files being moved")
    end

    -- ENHANCEMENT: Auto-mirror test packages
    -- When main package is moved, automatically move corresponding test package
    -- This includes PHYSICALLY MOVING the test files/directories
    local test_mirrors = {}
    for _, change in ipairs(all_registered_changes) do
        -- Check if this is a main/java move
        if string_util.contains(change.src, "src/main/java/") then
            -- Calculate corresponding test path
            local test_src = change.src:gsub("src/main/java/", "src/test/java/")
            local test_dst = change.dst:gsub("src/main/java/", "src/test/java/")

            -- Only add if test path actually exists
            if vim.fn.isdirectory(test_src) == 1 or vim.fn.filereadable(test_src) == 1 then
                table.insert(test_mirrors, { src = test_src, dst = test_dst })
                log.info("Auto-mirroring test package:", test_src, "->", test_dst)
            end
        end
    end

    -- Physically move test files/directories to match main package structure
    if #test_mirrors > 0 then
        log.info("Physically moving", #test_mirrors, "test packages to match main structure")

        -- Track opened test buffers before moving
        for _, mirror in ipairs(test_mirrors) do
            if vim.fn.isdirectory(mirror.src) == 1 then
                -- Find all Java files in this test directory
                local handle = io.popen("find '" .. mirror.src .. "' -name '*.java' 2>/dev/null")
                if handle then
                    for file_path in handle:lines() do
                        local buf_id = buffer_util.find_buf_by_path(file_path)
                        if buf_id then
                            -- Calculate new path for this test file
                            local new_path = file_path:gsub("^" .. vim.pesc(mirror.src), mirror.dst)
                            table.insert(opened_buffers_to_reopen, {
                                old_path = file_path,
                                new_path = new_path,
                                buf_id = buf_id
                            })
                            log.info("Will reopen test buffer:", file_path, "->", new_path)
                        end
                    end
                    handle:close()
                end
            elseif mirror.src:match("%.java$") then
                -- Single test file
                local buf_id = buffer_util.find_buf_by_path(mirror.src)
                if buf_id then
                    table.insert(opened_buffers_to_reopen, {
                        old_path = mirror.src,
                        new_path = mirror.dst,
                        buf_id = buf_id
                    })
                    log.info("Will reopen test buffer:", mirror.src, "->", mirror.dst)
                end
            end
        end

        -- Perform physical moves
        for _, mirror in ipairs(test_mirrors) do
            -- Create destination directory
            local dst_parent = mirror.dst:match("(.+)/[^/]+$")
            if dst_parent then
                vim.fn.mkdir(dst_parent, "p")
            end

            -- Move file or directory
            local is_file = vim.fn.filereadable(mirror.src) == 1
            local is_dir = vim.fn.isdirectory(mirror.src) == 1

            if is_file or is_dir then
                local move_result = os.rename(mirror.src, mirror.dst)
                if move_result then
                    log.info("Moved test:", mirror.src, "->", mirror.dst)
                else
                    -- Fallback to shell command for cross-device moves
                    local cmd = string.format("mv '%s' '%s'", mirror.src, mirror.dst)
                    local exit_code = os.execute(cmd)
                    if exit_code == 0 or exit_code == true then
                        log.info("Moved test (via shell):", mirror.src, "->", mirror.dst)
                    else
                        log.error("Failed to move test:", mirror.src, "->", mirror.dst)
                    end
                end
            end
        end

        -- Clean up empty parent directories in test
        log.info("Cleaning up empty test directories...")
        local cleanup_count = 0
        for _, mirror in ipairs(test_mirrors) do
            local src_parent = mirror.src:match("(.+)/[^/]+$")
            while src_parent and src_parent:match("src/test/java/") do
                if vim.fn.isdirectory(src_parent) == 1 then
                    -- Check if directory is empty
                    local handle = io.popen("find '" .. src_parent .. "' -mindepth 1 -maxdepth 1 | wc -l")
                    local count = tonumber(handle:read("*all"))
                    handle:close()

                    if count == 0 then
                        vim.fn.delete(src_parent, "d")
                        log.info("Removed empty test directory:", src_parent)
                        cleanup_count = cleanup_count + 1
                        src_parent = src_parent:match("(.+)/[^/]+$")
                    else
                        break
                    end
                else
                    break
                end
            end
        end
        if cleanup_count > 0 then
            log.info("Cleaned up", cleanup_count, "empty test directories")
        else
            log.info("No empty test directories to clean up")
        end
    end

    -- Add test mirrors to registered changes for package declaration updates
    for _, mirror in ipairs(test_mirrors) do
        table.insert(all_registered_changes, mirror)
        log.debug("Added test mirror for refactoring:", mirror.src, "->", mirror.dst)
    end

    if #test_mirrors > 0 then
        log.info("Added", #test_mirrors, "test package mirrors for declaration updates")
    end

    -- Strategy: Find the deepest common package path change
    -- For example, if moving com/example/... -> ua/dsm/corp/example/...
    -- We want to process com/example (depth 2), not com (depth 1)

    -- Find all directory/package moves (not files)
    local package_moves = {}
    for _, change in ipairs(all_registered_changes) do
        if not change.src:match("%.java$") then
            -- This is a directory move
            -- Calculate package depth
            local root_match = nil
            for _, root in ipairs(package_roots) do
                if string_util.contains(change.src, root) then
                    root_match = root
                    break
                end
            end

            if root_match then
                local package_path = vim.split(change.src, root_match)[2]
                -- Remove trailing slash if present
                package_path = package_path:gsub("/$", "")
                -- Calculate depth: com/example = 2 levels (1 slash + 1)
                local slash_count = select(2, package_path:gsub("/", ""))
                local package_depth = slash_count + 1
                table.insert(package_moves, {
                    change = change,
                    depth = package_depth,
                    package_path = package_path
                })
                log.debug("Found package move:", package_path, "depth:", package_depth)
            end
        end
    end

    -- ENHANCEMENT: Infer directory moves from file moves
    -- This is needed when using file managers like fyler.nvim that only register file moves
    -- Strategy: Find the common ancestor package directory for all file moves
    if #package_moves == 0 then
        log.info("No explicit directory moves found, attempting to infer from file moves")

        -- Extract package paths for all file moves
        local src_packages = {}
        local dst_packages = {}
        local file_count = 0

        for _, change in ipairs(all_registered_changes) do
            if change.src:match("%.java$") then
                -- Find which package root this file belongs to
                local root_match = nil
                for _, root in ipairs(package_roots) do
                    if string_util.contains(change.src, root) then
                        root_match = root
                        break
                    end
                end

                if root_match then
                    -- Extract package path after the root
                    -- e.g., "ua/example/service/ServiceA.java" -> "ua/example/service"
                    local src_relative = vim.split(change.src, root_match)[2]
                    local dst_relative = vim.split(change.dst, root_match)[2]

                    -- Remove filename to get directory
                    local src_dir = src_relative:match("(.+)/[^/]+$")
                    local dst_dir = dst_relative:match("(.+)/[^/]+$")

                    if src_dir and dst_dir then
                        src_packages[src_dir] = true
                        dst_packages[dst_dir] = true
                        file_count = file_count + 1
                    end
                end
            end
        end

        if file_count > 0 then
            -- Find the common prefix for all source packages
            local src_list = {}
            for pkg, _ in pairs(src_packages) do
                table.insert(src_list, pkg)
            end
            table.sort(src_list)

            -- Find common prefix of all source packages
            local common_src = src_list[1]
            for i = 2, #src_list do
                local pkg = src_list[i]
                -- Find common prefix between common_src and pkg
                local min_len = math.min(#common_src, #pkg)
                local common_len = 0
                for j = 1, min_len do
                    if common_src:sub(j, j) == pkg:sub(j, j) then
                        common_len = j
                    else
                        break
                    end
                end
                -- Truncate to last complete directory (not partial)
                -- If we stopped mid-directory, find the last slash before that point
                common_src = common_src:sub(1, common_len)
                -- Only strip back if we ended mid-directory (not on a slash)
                if common_len > 0 and common_src:sub(common_len, common_len) ~= "/" then
                    local last_slash = common_src:match("^.*()/")
                    if last_slash and last_slash > 1 then
                        common_src = common_src:sub(1, last_slash - 1)
                    end
                else
                    -- Remove trailing slash if present
                    common_src = common_src:gsub("/$", "")
                end
            end

            -- Find common prefix for all destination packages
            local dst_list = {}
            for pkg, _ in pairs(dst_packages) do
                table.insert(dst_list, pkg)
            end
            table.sort(dst_list)

            local common_dst = dst_list[1]
            for i = 2, #dst_list do
                local pkg = dst_list[i]
                local min_len = math.min(#common_dst, #pkg)
                local common_len = 0
                for j = 1, min_len do
                    if common_dst:sub(j, j) == pkg:sub(j, j) then
                        common_len = j
                    else
                        break
                    end
                end
                -- Truncate to last complete directory (not partial)
                common_dst = common_dst:sub(1, common_len)
                -- Only strip back if we ended mid-directory (not on a slash)
                if common_len > 0 and common_dst:sub(common_len, common_len) ~= "/" then
                    local last_slash = common_dst:match("^.*()/")
                    if last_slash and last_slash > 1 then
                        common_dst = common_dst:sub(1, last_slash - 1)
                    end
                else
                    -- Remove trailing slash if present
                    common_dst = common_dst:gsub("/$", "")
                end
            end

            if common_src and common_dst and common_src ~= common_dst then
                log.info(
                    "Inferred common package move from",
                    file_count,
                    "files:",
                    common_src,
                    "->",
                    common_dst
                )

                -- Calculate depth and create package move
                local slash_count = select(2, common_src:gsub("/", ""))
                local package_depth = slash_count + 1

                -- Find the root to construct full paths
                local root_match = nil
                for _, root in ipairs(package_roots) do
                    if string_util.contains(all_registered_changes[1].src, root) then
                        root_match = root
                        break
                    end
                end

                if root_match then
                    local full_src = root_match:gsub("/$", "") .. "/" .. common_src
                    local full_dst = root_match:gsub("/$", "") .. "/" .. common_dst

                    table.insert(package_moves, {
                        change = { src = full_src, dst = full_dst },
                        depth = package_depth,
                        package_path = common_src
                    })
                    log.info("Inferred package move:", common_src, "depth:", package_depth)
                end
            end
        end
    end

    -- OPTIMIZATION: Find the most efficient package move
    -- Strategy: Select the SHALLOWEST valid move (depth >= 2) that covers all file changes
    -- This is more efficient than processing many deep changes individually

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

    -- Sort by depth (SHALLOWEST first) - this is the key optimization!
    -- Shallowest = broadest valid change = processes more files at once = more efficient
    table.sort(valid_moves, function(a, b)
        return a.depth < b.depth
    end)

    -- Verify that the shallowest move actually covers the file moves
    local target_change = nil
    if #valid_moves > 0 then
        local candidate = valid_moves[1]

        -- Check if this move's package path is a prefix of all file move paths
        local covers_all_files = true
        local covered_file_count = 0

        for _, change in ipairs(all_registered_changes) do
            if change.src:match("%.java$") then
                -- Extract package path from file
                local file_package_path = nil
                for _, root in ipairs(package_roots) do
                    if string_util.contains(change.src, root) then
                        local relative = vim.split(change.src, root)[2]
                        local dir = relative:match("(.+)/[^/]+$")
                        if dir then
                            file_package_path = dir
                            break
                        end
                    end
                end

                -- Check if candidate covers this file
                if file_package_path and string_util.contains(file_package_path, candidate.package_path) then
                    covered_file_count = covered_file_count + 1
                elseif file_package_path then
                    print(
                        "[DEBUG] Package move",
                        candidate.package_path,
                        "does NOT cover",
                        file_package_path
                    )
                    log.debug("Package move", candidate.package_path, "does not cover", file_package_path)
                    covers_all_files = false
                end
            end
        end


        -- If no file moves were registered, we can't verify coverage, so just use the candidate
        local has_file_moves = false
        for _, change in ipairs(all_registered_changes) do
            if change.src:match("%.java$") then
                has_file_moves = true
                break
            end
        end

        if not has_file_moves then
            target_change = candidate.change
            -- Still need to find the correct change by package path
            for _, change in ipairs(all_registered_changes) do
                if not change.src:match("%.java$") then
                    for _, root in ipairs(package_roots) do
                        if string_util.contains(change.src, root) then
                            local pkg_path = vim.split(change.src, root)[2]:gsub("/$", "")
                            if pkg_path == candidate.package_path then
                                target_change = change
                                break
                            end
                        end
                    end
                    if target_change ~= candidate.change then
                        break
                    end
                end
            end
        elseif covers_all_files and covered_file_count > 0 then
            -- Find the actual change that matches this package path
            -- Need to match the exact package path, not just any change
            for _, change in ipairs(all_registered_changes) do
                if not change.src:match("%.java$") then
                    -- Check if this directory change matches the selected package path
                    for _, root in ipairs(package_roots) do
                        if string_util.contains(change.src, root) then
                            local pkg_path = vim.split(change.src, root)[2]:gsub("/$", "")
                            if pkg_path == candidate.package_path then
                                target_change = change
                                break
                            end
                        end
                    end
                    if target_change then
                        break
                    end
                end
            end

            if not target_change then
                log.error("Failed to find matching change for package path:", candidate.package_path)
                target_change = candidate.change -- fallback
            end

            log.info(
                "Selected SHALLOWEST valid package move at depth",
                candidate.depth,
                ":",
                candidate.package_path,
                "(covers",
                covered_file_count,
                "files)"
            )
        else
            -- Files don't fully match - still use shallowest valid move
            -- This handles cases where some files might be at different depths
            log.warn(
                "Shallowest move doesn't fully cover files (covers_all="
                    .. tostring(covers_all_files)
                    .. ", count="
                    .. covered_file_count
                    .. "), but still using shallowest valid depth"
            )

            -- Find the correct change by package path
            for _, change in ipairs(all_registered_changes) do
                if not change.src:match("%.java$") then
                    for _, root in ipairs(package_roots) do
                        if string_util.contains(change.src, root) then
                            local pkg_path = vim.split(change.src, root)[2]:gsub("/$", "")
                            if pkg_path == candidate.package_path then
                                target_change = change
                                break
                            end
                        end
                    end
                    if target_change then
                        break
                    end
                end
            end

            if not target_change then
                log.error("Failed to find change for shallowest package:", candidate.package_path)
                target_change = candidate.change
            end

            log.info(
                "Selected SHALLOWEST valid package move at depth",
                candidate.depth,
                ":",
                candidate.package_path,
                "(best available option)"
            )
        end
    end

    -- If no valid package move found, process all file changes
    local global_cmds_table = {}
    if target_change then
        -- Process the selected package-level change
        log.info("Processing package-level refactoring")
        target_change.siblings = get_all_src_siblings(target_change, all_registered_changes)
        if target_change.siblings and #target_change.siblings > 0 then
            log.debug("Found", #target_change.siblings, "siblings for", target_change.src)
        end
        local change_cmd = build_fix_java_proj_after_change_cmd(target_change)
        if change_cmd then
            log.debug("Adding command for:", target_change.dst)
            table.insert(global_cmds_table, change_cmd)
        else
        end
    else
        -- Process all file changes individually
        log.info("No valid package move found, processing files individually")
        for _, value in ipairs(all_registered_changes) do
            if value.src:match("%.java$") then
                value.siblings = get_all_src_siblings(value, all_registered_changes)
                if value.siblings and #value.siblings > 0 then
                    log.debug("Found", #value.siblings, "siblings for", value.src)
                end
                local change_cmd = build_fix_java_proj_after_change_cmd(value)
                if change_cmd then
                    log.debug("Adding command for:", value.dst)
                    table.insert(global_cmds_table, change_cmd)
                end
            end
        end
    end
    log.info("Total commands to execute:", #global_cmds_table)
    local global_cmd_run = table.concat(global_cmds_table, " && ")
    log.debug("Full command chain length:", #global_cmd_run, "characters")

    -- In test mode, execute directly and return result
    if M.test_mode then
        log.info("Test mode: executing commands directly")
        log.debug("Command:", global_cmd_run)
        local exit_code = os.execute(global_cmd_run)
        log.info("Command execution completed with exit code:", exit_code)
        all_registered_changes = {}
        return exit_code == 0 or exit_code == true
    end

    -- ENHANCEMENT: Delete old buffers BEFORE applying changes
    -- This prevents having buffers pointing to non-existent files during refactoring
    if #opened_buffers_to_reopen > 0 then
        log.info("Deleting", #opened_buffers_to_reopen, "old buffers before applying changes...")

        for i, buf_info in ipairs(opened_buffers_to_reopen) do
            log.debug("Processing buffer", i, "of", #opened_buffers_to_reopen)
            if vim.api.nvim_buf_is_valid(buf_info.buf_id) then
                -- Store window ID BEFORE deleting buffer
                buf_info.win_id = vim.fn.bufwinid(buf_info.buf_id)
                log.debug("Buffer", buf_info.buf_id, "in window", buf_info.win_id)

                -- Delete old buffer
                local success, err = pcall(vim.api.nvim_buf_delete, buf_info.buf_id, { force = false })
                if success then
                    log.info("Deleted old buffer:", buf_info.old_path, "(was in window", buf_info.win_id, ")")
                else
                    log.warn("Failed to delete buffer:", buf_info.old_path, "Error:", err)
                end
            else
                log.debug("Buffer already invalid:", buf_info.old_path)
            end
        end
    end

    -- Normal mode: use UI to apply refactoring changes
    -- vim.notify(global_cmd_run)
    -- vim.notify(table.concat(global_cmds_table, "\n# "))

    -- ENHANCEMENT: Reopen buffers from new locations AFTER refactoring completes
    -- Pass callback to run_cmd to execute after successful completion
    local reopen_buffers_callback = nil
    if #opened_buffers_to_reopen > 0 then
        reopen_buffers_callback = function()
            log.info("Reopening", #opened_buffers_to_reopen, "buffers from new locations...")

            for i, buf_info in ipairs(opened_buffers_to_reopen) do
                log.debug("Reopening buffer", i, "of", #opened_buffers_to_reopen, ":", buf_info.new_path)
                -- Open new buffer from new location
                if vim.fn.filereadable(buf_info.new_path) == 1 then
                    log.debug("File exists at new location:", buf_info.new_path)
                    if buf_info.win_id and buf_info.win_id ~= -1 and vim.api.nvim_win_is_valid(buf_info.win_id) then
                        -- Buffer was displayed in a window, open new file in that window
                        log.debug("Reopening in window", buf_info.win_id)
                        vim.api.nvim_win_call(buf_info.win_id, function()
                            vim.cmd("edit " .. vim.fn.fnameescape(buf_info.new_path))
                            vim.cmd("filetype detect")
                        end)
                        log.info("Reopened buffer in window", buf_info.win_id, ":", buf_info.new_path)
                    else
                        -- Buffer was not displayed or window no longer valid, just load it
                        log.debug("Loading as hidden buffer (win_id:", buf_info.win_id, ")")
                        vim.cmd("badd " .. vim.fn.fnameescape(buf_info.new_path))
                        log.info("Loaded hidden buffer:", buf_info.new_path)
                    end
                else
                    log.warn("New file not found, cannot reopen:", buf_info.new_path)
                end
            end
            log.info("Buffer reopening completed")
        end
    end

    run_cmd(global_cmd_run, reopen_buffers_callback)

    log.info("Clearing registered changes")
    all_registered_changes = {}
    return true
end

---@param src string
---@param dst string
M.process_single_file_change = function(src, dst)
    log.info("Processing single file change:", src, "->", dst)
    M.register_change(src, dst)
    M.process_registerd_changes()
end

return M
