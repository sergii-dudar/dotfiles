-- INFO: this module is not finished yet
local M = {}

local util = require("utils.common-util")
local string_util = require("utils.string-util")
local spinner = require("utils.ui.spinner")
local list_util = require("utils.list-util")
local home = os.getenv("HOME")

local current_term_win = nil
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
local test_path = "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem" -- TODO: change to . after finish
-- local test_path = vim.fn.getcwd()

local build_fix_java_file_after_change_cmds = function(result_cmds, root, src, dst)
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
            .. test_path
            .. " | xargs sed -i -E 's/([[:space:],;(}<])%s([[:space:],;(}\\.>])/\\1%s\\2/g' || echo 'skipped'",
        package_src_classpath_escaped,
        old_type_name,
        new_type_name
    )
    -- vim.notify(fix_type_symbols_where_imported)
    table.insert(result_cmds, fix_type_symbols_where_imported)

    -- ==========================================================================
    -- ==========================================================================
    -- 3. fix type full qualified names (acroll all files - java,yaml,properties etc)
    local fix_type_full_qualified_names = string.format(
        "rg --color=never -l '%s' " .. test_path .. " | xargs sed -i -E 's/%s([;.$\"]|$)/%s\\1/g' || echo 'skipped'",
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
        "rg --color=never -l '%s' " .. test_path .. " | xargs sed -i -E 's/%s([;.\"]|$)/%s\\1/g' || echo 'skipped'",
        package_src_path_escaped,
        package_src_path_escaped,
        package_dst_path_escaped
    )
    -- vim.notify(fix_file_paht_declaration)
    table.insert(result_cmds, fix_file_paht_declaration)
end

local build_fix_java_package_after_change_cmds = function(result_cmds, root, src, dst)
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
        "rg --color=never -l '%s' " .. test_path .. " | xargs sed -i -E 's/%s([;.$\"]|$)/%s\\1/g' || echo 'skipped'",
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
        "rg --color=never -l '%s' " .. test_path .. " | xargs sed -i -E 's/%s([;.\"\\/]|$)/%s\\1/g' || echo 'skipped'",
        package_src_path_escaped,
        package_src_path_escaped,
        package_dst_path_escaped
    )
    -- vim.notify(fix_file_paht_declaration)
    table.insert(result_cmds, fix_file_paht_declaration)
end

local build_fix_java_proj_after_change_cmds = function(src, dst)
    local result_cmds = {}
    local is_dir = util.is_dir(dst)
    local is_file = util.is_file(dst)
    for _, root in ipairs(package_roots) do
        -- TODO: after applying on relative path, rg search need apply ". root" to take in account test, main, resourses
        if string_util.contains(src, root) and string_util.contains(dst, root) then
            if is_file then
                build_fix_java_file_after_change_cmds(result_cmds, root, src, dst)
            elseif is_dir then
                build_fix_java_package_after_change_cmds(result_cmds, root, src, dst)
            end
        end
    end
    return result_cmds
end

--- Fix java project after remaning java file, or package name
---@param src string [old java file/dir path]
---@param dst string [new  java file/dir path]
---@return string|nil
local build_fix_java_proj_after_change_cmd = function(src, dst)
    if not src:match("src/.*/java/") then
        return nil
    end
    if not dst:match("src/.*/java/") then
        return nil
    end
    local cmds = build_fix_java_proj_after_change_cmds(src, dst)
    dd(cmds)
    local cmd_to_run = table.concat(cmds, " && ")
    -- dd(cmd_to_run)

    -- vim.notify(cmd_to_run)
    return cmd_to_run
    -- run_cmd(cmd_to_run)
end

local all_registered_changes = {}
M.register_change = function(src, dst)
    table.insert(all_registered_changes, {
        src = src,
        dst = dst,
    })
end

-- local list = {}
-- table.insert(list, "test 1")
-- table.insert(list, "name 2")
-- table.insert(list, "issua 3")
-- table.insert(list, "astral 4")
-- table.insert(list, "an 5")
--
-- --local list_util = require("utils.list-util")
-- for i, value in list_util.sorted_iter(list) do
--     print(string.format("%d, %s, %s", i, value, list[i]))
-- end

M.process_registerd_changes = function()
    if vim.tbl_isempty(all_registered_changes) then
        vim.notify("No any registered changes")
    else
        dd(all_registered_changes)
        local global_cmds_table = {}
        for _, value in list_util.sorted_iter(all_registered_changes) do
            local change_cmd = build_fix_java_proj_after_change_cmd(value.src, value.dst)
            if change_cmd then
                table.insert(global_cmds_table, change_cmd)
            end
        end
        local global_cmd_run = table.concat(global_cmds_table, " && ")
        -- vim.notify(global_cmd_run)
        run_cmd(global_cmd_run)
    end
    all_registered_changes = {}
end

-- M.fix_java_proj_after_change(
--     "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/ServiceEmployee.java",
--     "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"
-- )

--[[ M.fix_java_proj_after_change(
    "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service",
    "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/ua/tld/example/EmployeeManagementSystem/service"
)
M.fix_java_proj_after_change(
    "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example",
    "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/ua/tld/example"
)
M.fix_java_proj_after_change(
    "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/ua/tld/example/EmployeeManagementSystem/service/ServiceEmployee.java",
    "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/ua/tld/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"
) ]]

return M
