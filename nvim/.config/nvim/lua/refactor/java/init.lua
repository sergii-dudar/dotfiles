local M = {}

local util = require("utils.common-util")

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




packages|dirs changes fixes:
-- TODO: maybe will be need separare to (import path, package path)
rg --color=never -l 'com.example.EmployeeManagementSystem' . | xargs sed -i -E 's/com.example.EmployeeManagementSystem/com.serhii.dudar.application/g'
rg --color=never -l 'com.serhii.dudar.application' . | xargs sed -i -E 's/com.serhii.dudar.application/com.example.EmployeeManagementSystem/g'

rg --color=never -l 'com\/example\/EmployeeManagementSystem' . | xargs sed -i -E 's/com\/example\/EmployeeManagementSystem/com\/serhii\/dudar\/application/g'
rg --color=never -l 'com\/serhii\/dudar\/application' . | xargs sed -i -E 's/com\/serhii\/dudar\/application/com\/example\/EmployeeManagementSystem/g'


java file rename fixes:
-- class declaration
sed -i -E 's/(class|interface|enum|record) ServiceEmployee\s/\1 ServiceUser /g' /home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/ServiceEmployee.java
sed -i -E 's/(class|interface|enum|record) ServiceUser\s/\1 ServiceEmployee /g'  /home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/ServiceUser.java

-- global symbols usabe (imported)
rg --color=never -l 'import com\.example\.EmployeeManagementSystem\.dto\.DtoEmployee' . | xargs sed -i -E 's/DtoEmployee[^\(]/DtoEmployeeUser/g'
rg --color=never -l 'import com\.example\.EmployeeManagementSystem\.dto\.DtoEmployeeUser' . | xargs sed -i -E 's/DtoEmployeeUser[^\(]/DtoEmployee/g'

-- global symbols usabe (with full qualified name), also that can be used in properties/yaml
rg --color=never -l '' . | xargs sed -i -E 's///g'
rg --color=never -l '' . | xargs sed -i -E 's///g'

non java file rename fixes:

--]]

M.fix_java_proj_after_file_rename = function(src_file, destination_file)
    vim.notify(string.format("Apply java class change: %s -> %s", src_file, destination_file))
end

M.fix_java_proj_after_path_rename = function(src_path, destination_path)
    vim.notify(string.format("Apply package change: %s -> %s", src_path, destination_path))
end

M.flyer_fix_java_proj_after_change = function(src, destination)
    local src_parts = vim.split(src, "/")
    local destination_parts = vim.split(destination, "/")
    local is_leaf_changed = src_parts[#src_parts] ~= destination_parts[#destination_parts]

    -- print(is_leaf_changed)
    -- print(destination_parts)
    local is_leaf_dir_changed = (util.is_dir(src) or util.is_dir(destination)) and is_leaf_changed
    local is_leaf_file_changed = (util.is_file(src) or util.is_file(destination)) and is_leaf_changed

    if is_leaf_dir_changed then
        M.fix_java_proj_after_path_rename(src, destination)
        return
    end

    if is_leaf_file_changed then
        M.fix_java_proj_after_path_rename(src, destination)
        return
    end

    vim.notify(string.format("Skipped leaf change: %s -> %s", src, destination))
end

--M.flyer_fix_java_proj_after_change("src/main/java/com/example", "src/main/java/com/serhii")

-- TODO:

return M
