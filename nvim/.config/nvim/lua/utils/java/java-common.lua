local M = {}

local util = require("utils.common-util")

-- local root = vim.fn.getcwd()
local root = vim.fs.root(0, { ".git", "pom.xml", "mvnw", "gradlew", "build.gradle", "build.gradle.kts" })
local src_dir = root .. "/src/main/java/"
local test_dir = root .. "/src/test/java/"

M.java_class_to_path = function(classname)
    local relative_path = classname:gsub("%.", "/") .. ".java"
    local full_path_src = src_dir .. relative_path
    if util.is_file_exists(full_path_src) then
        return full_path_src
    end
    return test_dir .. relative_path
end

return M