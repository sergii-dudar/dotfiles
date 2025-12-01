local M = {}

local util = require("utils.common-util")
local string_util = require("utils.string-util")
local spinner = require("utils.ui.spinner")

local current_term_win = nil
local function run_cmd(cmd_args)
    vim.notify("🚀 Java Refactoring Started", vim.log.levels.INFO)
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

--[[
MOVE src/main/java/com/example/EmployeeManagementSystem/dto > src/main/java/com/serhii/application/EmployeeManagementSystem/dto
MOVE src/main/java/com/example/EmployeeManagementSystem/model > src/main/java/com/serhii/application/EmployeeManagementSystem/model
MOVE src/main/java/com/example/EmployeeManagementSystem/exception > src/main/java/com/serhii/application/EmployeeManagementSystem/exception
MOVE src/main/java/com/example/EmployeeManagementSystem/repository > src/main/java/com/serhii/application/EmployeeManagementSystem/repository
MOVE src/main/java/com/example/EmployeeManagementSystem/mapper > src/main/java/com/serhii/application/EmployeeManagementSystem/mapper
MOVE src/main/java/com/example/EmployeeManagementSystem/controller > src/main/java/com/serhii/application/EmployeeManagementSystem/controller
MOVE src/main/java/com/example/EmployeeManagementSystem/service > src/main/java/com/serhii/application/EmployeeManagementSystem/service

MOVE src/main/java/com/example/EmployeeManagementSystem/service/ServiceEmployeeIn.java > src/main/java/com/serhii/application/EmployeeManagementSystem/service/ServiceEmployeeIn.java
MOVE src/main/java/com/example/EmployeeManagementSystem/EmployeeManagementSystemApplication.java > src/main/java/com/serhii/application/EmployeeManagementSystem/EmployeeManagementSystemApplication.java
MOVE src/main/java/com/example/EmployeeManagementSystem > src/main/java/com/serhii/application/EmployeeManagementSystem


MOVE src/test/resources/test/rest/request/request-success.json > src/test/resources/api/data/rest/request/request-success.json
MOVE src/test/resources/test/rest/request/request-fail.json > src/test/resources/api/data/rest/request/request-fail.json
MOVE src/test/resources/test/rest/request > src/test/resources/api/data/rest/request
MOVE src/test/resources/test/rest > src/test/resources/api/data/rest
MOVE src/test/resources/test > src/test/resources/api

result changes to apply:
MOVE src/main/java/com/example/EmployeeManagementSystem/service/ServiceEmployee.java > src/main/java/com/serhii/application/EmployeeManagementSystem/service/ServiceTest.java
MOVE src/main/java/com/example > src/main/java/com/serhii
]]

-- local src_path = "src/main/java/com/example/EmployeeManagementSystem/dto"
-- local destination_path = "src/main/java/com/serhii/application/EmployeeManagementSystem/dto"
--
-- local src_path_parts = vim.split(src_path, "/")
-- local destination_path_parts = vim.split(destination_path, "/")
--
-- print(src_path_parts)
-- print(destination_path_parts)
--
-- print(util.is_dir(src_path))
-- print(util.is_dir(destination_path))

--[[
fd . -e java -x sed -i 's/^package com\.example\.EmployeeManagementSystem/package com.example.core/'
rg --color=never -l '^package com\.example\.EmployeeManagementSystem\.service' | xargs sed -i 's/^package com\.example\.EmployeeManagementSystem\.service/package com.example.core.service/g'

rg --color=never -l '^package com\.example\.EmployeeManagementSystem' | xargs sed -i 's/^package com\.example\.EmployeeManagementSystem/package com.example.core/g'
rg --color=never -l -g '*.java' '(class|interface|enum|record) ServiceEmployee' src/main/java | xargs -d '\n' sed -i -E 's/(class|interface|enum|record) ServiceEmployee/\1 ServiceUser/g'

-------------
rg --color=never -l -g '*.java' '(class|interface|enum|record) ServiceEmployee' . | xargs sed -i -E 's/(class|interface|enum|record) ServiceEmployee/\1 ServiceUser/g'
rg --color=never -l -g '*.java' '(package|import|import static) com.example.EmployeeManagementSystem' . | xargs sed -i -E 's/(package|import|import static) com.example.EmployeeManagementSystem/\1 com.serhii.application/g'

rg --color=never -l -g '*.java' 'com.example.EmployeeManagementSystem' . | xargs sed -i -E 's/com.example.EmployeeManagementSystem/com.serhii.dudar.application/g'
rg --color=never -l -g '*.java' 'com.serhii.dudar.application' . | xargs sed -i -E 's/com.serhii.dudar.application/com.example.EmployeeManagementSystem/g'




============================================================================
============================================================================
============================================================================
java file rename fixes:
priority is very important:

1. fix type decration in changed file.
sed -i -E 's/(class|interface|enum|record) ServiceEmployee\s/\1 ServiceUser /g' /home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/ServiceEmployee.java
sed -i -E 's/(class|interface|enum|record) ServiceUser\s/\1 ServiceEmployee /g'  /home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/ServiceUser.java

(split by [ src/main/java, src/test/java ])
2. fix type symbols (simple java name) where type is imported.
rg --color=never -l 'import com\.example\.EmployeeManagementSystem\.dto\.DtoEmployee' . | xargs sed -i -E 's/DtoEmployee/DtoEmployeeUser/g'
rg --color=never -l 'import com\.example\.EmployeeManagementSystem\.dto\.DtoEmployeeUser' . | xargs sed -i -E 's/DtoEmployeeUser/DtoEmployee/g'

3. fix type full qualified names (acroll all files - java,yaml,properties etc)
-- global symbols usabe (with full qualified name), also that can be used in properties/yaml
rg --color=never -l 'com\.example\.EmployeeManagementSystem\.dto\.DtoEmployee' . | xargs sed -i -E 's/com\.example\.EmployeeManagementSystem\.dto\.DtoEmployee/com.example.EmployeeManagementSystem.dto.DtoEmployeeUser/g'
rg --color=never -l 'com\.example\.EmployeeManagementSystem\.dto\.DtoEmployeeUser' . | xargs sed -i -E 's/com\.example\.EmployeeManagementSystem\.dto\.DtoEmployeeUser/com.example.EmployeeManagementSystem.dto.DtoEmployee/g'

4. fix file path
rg --color=never -l 'com\/example\/EmployeeManagementSystem\/dto\/DtoEmployee' . | xargs sed -i -E 's/com\/example\/EmployeeManagementSystem\/dto\/DtoEmployee/com\/example\/EmployeeManagementSystem\/dto\/DtoEmployeeUser/g'
rg --color=never -l 'com\/example\/EmployeeManagementSystem\/dto\/DtoEmployeeUser' . | xargs sed -i -E 's/com\/example\/EmployeeManagementSystem\/dto\/DtoEmployeeUser/com\/example\/EmployeeManagementSystem\/dto\/DtoEmployee/g'

(split by [ src/main/resources, src/test/resources ])
5. fix resources path
rg --color=never -l 'com\/example\/EmployeeManagementSystem\/dto\/DtoEmployee' . | xargs sed -i -E 's/com\/example\/EmployeeManagementSystem\/dto\/DtoEmployee/com\/example\/EmployeeManagementSystem\/dto\/DtoEmployeeUser/g'
rg --color=never -l 'com\/example\/EmployeeManagementSystem\/dto\/DtoEmployeeUser' . | xargs sed -i -E 's/com\/example\/EmployeeManagementSystem\/dto\/DtoEmployeeUser/com\/example\/EmployeeManagementSystem\/dto\/DtoEmployee/g'

============================================================================
============================================================================
============================================================================

packages|dirs changes fixes:
-- NOTE: maybe will be need separare to (import path, package path)

rg --color=never -l 'com\/example\/EmployeeManagementSystem' . | xargs sed -i -E 's/com\/example\/EmployeeManagementSystem/com\/serhii\/dudar\/application/g'
rg --color=never -l 'com\/serhii\/dudar\/application' . | xargs sed -i -E 's/com\/serhii\/dudar\/application/com\/example\/EmployeeManagementSystem/g'

--]]

M.fix_java_proj_after_file_rename = function(src_file, destination_file)
    vim.notify(string.format("Apply java class change: %s -> %s", src_file, destination_file))
end

M.fix_java_proj_after_path_rename = function(src_path, destination_path)
    vim.notify(string.format("Apply package change: %s -> %s", src_path, destination_path))
end

local main_dir = "src/main/java/"
local test_dir = "src/test/java/"
local main_resource_dir = "src/main/resources/"
local test_resource_dir = "src/test/resources/"
local package_roots = { main_dir, test_dir, main_resource_dir, test_resource_dir }
--local test_path = "."
local test_path = "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem"

M.fix_java_proj_after_change = function(src, destination)
    local is_dir = util.is_dir(destination)
    local is_file = util.is_file(destination)
    for _, root in ipairs(package_roots) do
        if string_util.contains(src, root) and string_util.contains(destination, root) then
            -- com/example/EmployeeManagementSystem/service/ServiceEmployee
            local package_src_path = vim.split(src, root)[2]:gsub("%.java", "")
            local package_destination_path = vim.split(destination, root)[2]:gsub("%.java", "")

            -- com.example.EmployeeManagementSystem.service.ServiceEmployee
            local package_src_classpath = package_src_path:gsub("/", ".")
            local package_destination_classpath = package_destination_path:gsub("/", ".")

            -- com\.example\.EmployeeManagementSystem\.service\.ServiceEmployee
            local package_src_classpath_escaped = package_src_classpath:gsub("%.", "\\.")

            -- com\/example\/EmployeeManagementSystem\/service\/ServiceEmployee
            local package_src_path_escaped = package_src_path:gsub("/", "\\/")
            local package_destination_path_escaped = package_destination_path:gsub("/", "\\/")

            -- ServiceEmployee
            local old_type_name = package_src_path:match("([^/]+)$")
            local new_type_name = package_destination_path:match("([^/]+)$")

            if is_file then
                -- com.example.EmployeeManagementSystem.service
                local package_declaration_src = package_src_classpath:match("(.+)%.%w+$")
                local package_declaration_destination = package_destination_classpath:match("(.+)%.%w+$")

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
                    destination
                )
                run_cmd(fix_type_declaration_cmd)

                -- ==========================================================================
                -- ==========================================================================
                -- 2. fix type symbols (simple java name) where type is imported.
                local fix_type_symbols_where_imported = string.format(
                    "rg --color=never -l 'import\\s+%s' "
                        .. test_path
                        .. " | xargs sed -i -E 's/([[:space:],;(}<])%s([[:space:],;(}\\.>])/\\1%s\\2/g'",
                    package_src_classpath_escaped,
                    old_type_name,
                    new_type_name
                )
                -- vim.notify(fix_type_symbols_where_imported)
                run_cmd(fix_type_symbols_where_imported)

                -- ==========================================================================
                -- ==========================================================================
                -- 3. fix type full qualified names (acroll all files - java,yaml,properties etc)
                local fix_type_full_qualified_names = string.format(
                    "rg --color=never -l '%s' " .. test_path .. " | xargs sed -i -E 's/%s([;.$\"]|$)/%s\\1/g'",
                    -- sed -i -E 's/ServiceEmployee([^[:alnum:]_]|$)/ServiceEmployeeUser\1/g'
                    package_src_classpath_escaped,
                    package_src_classpath_escaped,
                    package_destination_classpath
                )
                -- vim.notify(fix_type_symbols_where_imported)
                run_cmd(fix_type_full_qualified_names)

                -- ==========================================================================
                -- ==========================================================================
                -- 4. fix packaged decration in changed file.
                local fix_package_declaration = string.format(
                    "sed -i -E 's/package\\s+%s;/package %s;/g' %s",
                    package_declaration_src_escaped,
                    package_declaration_destination,
                    destination
                )
                -- vim.notify(fix_package_declaration)
                run_cmd(fix_package_declaration)

                -- TODO:
                -- ==========================================================================
                -- ==========================================================================
                -- add import declarations of the new class name to the classes of the old folder
                -- 5. add package import of old package
                -- local all_package_import_of_old_package = string.format(
                --     'sed -i "${3}i new line text" %s',
                --     package_declaration_src_escaped,
                --     package_declaration_src,
                --     destination
                -- )
                -- vim.notify(fix_package_declaration)
                -- run_cmd(fix_package_declaration)
                --
                -- sed -i "${N}i new line text" filepath
                --
                --
                -- ==========================================================================
                -- ==========================================================================
                -- 6. fix file path/resources path

                local fix_file_paht_declaration = string.format(
                    "rg --color=never -l '%s' " .. test_path .. " | xargs sed -i -E 's/%s([;.\"]|$)/%s\\1/g'",
                    package_src_path_escaped,
                    package_src_path_escaped,
                    package_destination_path_escaped
                )
                vim.notify(fix_file_paht_declaration)
                run_cmd(fix_file_paht_declaration)
            elseif is_dir then
            end
        end
    end

    -- local src_parts = vim.split(src, "/")
    -- local destination_parts = vim.split(destination, "/")
    -- local is_leaf_changed = src_parts[#src_parts] ~= destination_parts[#destination_parts]
    --
    -- print(util.is_dir(src))
    -- print(util.is_file(src))
    -- local is_leaf_dir_changed = (util.is_dir(src) or util.is_dir(destination)) and is_leaf_changed
    -- local is_leaf_file_changed = (util.is_file(src) or util.is_file(destination)) and is_leaf_changed
    --
    -- if is_leaf_dir_changed then
    --     M.fix_java_proj_after_path_rename(src, destination)
    --     return
    -- end
    --
    -- if is_leaf_file_changed then
    --     M.fix_java_proj_after_path_rename(src, destination)
    --     return
    -- end
    --
    -- vim.notify(string.format("Skipped leaf change: %s -> %s", src, destination))
end

M.fix_java_proj_after_change(
    "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/ServiceEmployee.java",
    "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"
)
-- M.fix_java_proj_after_change(
--     "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service",
--     "/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl"
-- )

return M

--[[ local function run(cmd)
    vim.fn.jobstart(cmd, {
        stdout_buffered = true,
        stderr_buffered = true,
        on_stdout = function(_, data)
            if data then
                print(table.concat(data, "\n"))
            end
        end,
        on_stderr = function(_, data)
            if data then
                print(table.concat(data, "\n"))
            end
        end,
    })
end ]]
