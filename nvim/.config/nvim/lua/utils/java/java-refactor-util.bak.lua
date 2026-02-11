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

---@class java.rejactor.FileMove
---@field src string
---@field dst string
---@field siblings? java.rejactor.FileMove[]

local current_term_win = nil
---@param cmd_args string
local function run_cmd(cmd_args)
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
            spinner.stop(code == 0, "Java refactoring")
            if code == 0 then
                util.close_window_if_exists(current_term_win)
            end
        end,
    })

    vim.cmd("startinsert")
    if job_id <= 0 then
        vim.notify("Failed to start cmd via jobstart()", vim.log.levels.ERROR)
    end
end

local main_dir = "src/main/java/"
local test_dir = "src/test/java/"
local main_resource_dir = "src/main/resources/"
local test_resource_dir = "src/test/resources/"
local package_roots = { main_dir, test_dir, main_resource_dir, test_resource_dir }
-- local test_path = "."
-- local test_path = "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem" -- TODO: change to . after finish
local project_root_path = vim.fn.getcwd()

---@param result_cmds table
---@param root string
---@param context java.rejactor.FileMove
local build_fix_java_file_after_change_cmds = function(result_cmds, root, context)
    -- TODO: use `local process_root_path = vim.fs.joinpath(project_root_path, [empty or package sub path in case multimodule, or sub module], root)`

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

    -- com.example.EmployeeManagementSystem.service
    local package_declaration_src = package_src_classpath:match("(.+)%.%w+$")
    local package_declaration_dst = package_dst_classpath:match("(.+)%.%w+$")

    -- com\.example\.EmployeeManagementSystem\.service
    local package_declaration_src_escaped = package_declaration_src:gsub("%.", "\\.")

    -- java file rename fixes (priority is very important):
    -- ==========================================================================
    -- ==========================================================================
    -- 1. fix type decration in changed file.
    local fix_type_declaration_cmd = string.format(
        "sed -i -E 's/(class|interface|enum|record) %s\\s/\\1 %s /g' %s",
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
            .. project_root_path
            .. " | xargs sed -i -E 's/([[:space:],;(}<])%s([[:space:],;(}\\.>])/\\1%s\\2/g' || echo 'skipped'",
        package_src_classpath_escaped,
        old_type_name,
        new_type_name
    )
    -- vim.notify(fix_type_symbols_where_imported)
    table.insert(result_cmds, fix_type_symbols_where_imported)

    -- ==========================================================================
    -- ==========================================================================
    -- 2.1. fix imports of siblings java files (in case moved to other packages)
    if context.siblings and not vim.tbl_isempty(context.siblings) then
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
                '"%s" "%s" "%s" "%s" "%s"',
                global.dotfiles_path("work/java/remane/fix-java-sibling-usage.sh"),
                dst, -- FILE_PATH_TO_APPLY_FIX
                sibling_package_declaration_dst, -- NEW_PACKAGE
                sibling_old_type_name, -- OLD_TYPE_NAME
                sibling_new_type_name -- NEW_TYPE_NAME
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
            .. project_root_path
            .. " | xargs sed -i -E 's/%s([;.$\"]|$)/%s\\1/g' || echo 'skipped'",
        -- sed -i -E 's/ServiceEmployee([^[:alnum:]_]|$)/ServiceEmployeeUser\1/g'
        package_src_classpath_escaped,
        package_src_classpath_escaped,
        package_dst_classpath
    )
    -- vim.notify(fix_type_symbols_where_imported)
    table.insert(result_cmds, fix_type_full_qualified_names)

    -- ==========================================================================
    -- ==========================================================================
    -- 4. fix packaged decration in changed file.
    local fix_package_declaration = string.format(
        "sed -i -E 's/package\\s+%s;/package %s;/g' %s",
        package_declaration_src_escaped,
        package_declaration_dst,
        dst
    )
    -- vim.notify(fix_package_declaration)
    table.insert(result_cmds, fix_package_declaration)

    -- ==========================================================================
    -- ==========================================================================
    -- 5. add import declarations of the new class name to the classes of the old folder
    -- OLD_DIR="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service"
    -- OLD_PACKAGE="com.example.EmployeeManagementSystem.service"
    -- NEW_FILE_PATH="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"

    local fix_old_file_imports = string.format(
        '"%s" "%s" "%s" "%s" "%s" "%s" "%s"',
        global.dotfiles_path("work/java/remane/fix-old-imports.sh"),
        src:match("(.+)/[^/]+$"), -- OLD_DIR
        package_declaration_src, -- OLD_PACKAGE
        package_declaration_dst, -- NEW_PACKAGE
        dst, -- NEW_FILE_PATH
        old_type_name, -- OLD_TYPE_NAME
        new_type_name -- NEW_TYPE_NAME
    )
    -- vim.notify(fix_old_file_imports)
    table.insert(result_cmds, fix_old_file_imports)
    -- ==========================================================================
    -- ==========================================================================
    -- 6. fix file path/resources path

    local fix_file_paht_declaration = string.format(
        "rg --color=never -l '%s' "
            .. project_root_path
            .. " | xargs sed -i -E 's/%s([;.\"]|$)/%s\\1/g' || echo 'skipped'",
        package_src_path_escaped,
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
    local src = context.src
    local dst = context.dst

    -- com/example/EmployeeManagementSystem/service
    local package_src_path = vim.split(src, root)[2]
    local package_dst_path = vim.split(dst, root)[2]

    -- com.example.EmployeeManagementSystem.service
    local package_src_classpath = package_src_path:gsub("/", ".")
    local package_dst_classpath = package_dst_path:gsub("/", ".")

    -- com\.example\.EmployeeManagementSystem\.service
    local package_src_classpath_escaped = package_src_classpath:gsub("%.", "\\.")

    -- com\/example\/EmployeeManagementSystem\/service\/ServiceEmployee
    local package_src_path_escaped = package_src_path:gsub("/", "\\/")
    local package_dst_path_escaped = package_dst_path:gsub("/", "\\/")

    -- java package rename fixes (priority is very important):
    -- ==========================================================================
    -- ==========================================================================
    -- 1. fix package full qualified names (acroll all files - java,yaml,properties etc)
    local fix_package_full_qualified_names = string.format(
        "rg --color=never -l '%s' "
            .. project_root_path
            .. " | xargs sed -i -E 's/%s([;.$\"]|$)/%s\\1/g' || echo 'skipped'",
        package_src_classpath_escaped,
        package_src_classpath_escaped,
        package_dst_classpath
    )
    -- vim.notify(fix_package_full_qualified_names)
    table.insert(result_cmds, fix_package_full_qualified_names)

    -- ==========================================================================
    -- ==========================================================================
    -- 2. fix package path/resources path
    local fix_file_paht_declaration = string.format(
        "rg --color=never -l '%s' "
            .. project_root_path
            .. " | xargs sed -i -E 's/%s([;.\"\\/]|$)/%s\\1/g' || echo 'skipped'",
        package_src_path_escaped,
        package_src_path_escaped,
        package_dst_path_escaped
    )
    -- vim.notify(fix_file_paht_declaration)
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
                local src_buffer_id = buffer_util.find_buf_by_path(context.src)
                if src_buffer_id then
                    vim.api.nvim_buf_delete(src_buffer_id, { force = false })
                end
                build_fix_java_file_after_change_cmds(result_cmds, root, context)
                if src_buffer_id then
                    --[[ vim.cmd.edit(context.dst)
                    -- reload new place of opened buffers and push filety detect to reattach lsp, trisitter etc.
                    local dst_buffer_id = buffer_util.find_buf_by_path(context.dst)
                    if dst_buffer_id then
                        vim.api.nvim_buf_call(dst_buffer_id, function()
                            vim.cmd("filetype detect")
                        end)
                    end ]]

                    vim.cmd("edit " .. vim.fn.fnameescape(context.dst) .. " | filetype detect")
                end
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
function M.register_change(src, dst)
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

function M.process_registerd_changes()
    if vim.tbl_isempty(all_registered_changes) then
        vim.notify("No any registered changes")
    else
        -- dd(all_registered_changes)
        local global_cmds_table = {}
        -- for _, value in ipairs(all_registered_changes) do
        for i = #all_registered_changes, 1, -1 do
            local value = all_registered_changes[i]
            value.siblings = get_all_src_siblings(value, all_registered_changes)
            local change_cmd = build_fix_java_proj_after_change_cmd(value)
            if change_cmd then
                table.insert(global_cmds_table, change_cmd)
            end
        end
        local global_cmd_run = table.concat(global_cmds_table, " && ")
        -- vim.notify(global_cmd_run)
        -- vim.notify(table.concat(global_cmds_table, "\n# "))
        run_cmd(global_cmd_run)
    end
    all_registered_changes = {}
end

---@param src string
---@param dst string
function M.process_single_file_change(src, dst)
    M.register_change(src, dst)
    M.process_registerd_changes()
end

return M
