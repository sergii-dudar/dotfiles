local M = {}

local uv = vim.loop
local maven_util = require("utils.java.maven-util")
local util = require("utils.common-util")
-- local util = require("utils.common-util")
local home = os.getenv("HOME")
local cwd = vim.fn.getcwd()

-- sdk list java
-- sdk install java 25xxx-amzn
-- sdk list maven
-- sdk install maven 3.9.xxx
local java_dir = vim.fn.glob(home .. "/.sdkman/candidates/java/current")
local java_bin = java_dir .. "/bin/java"
--local java_google_style_file = home .. "/dotfiles/work/formatter/intellij-java-google-style.xml"
--local java_google_style_file = home .. "/dotfiles/work/formatter/default_intellij_eclipse.xml"

-- https://github.com/google/styleguide/blob/gh-pages/eclipse-java-google-style.xml
local java_google_style_file = home .. "/dotfiles/work/formatter/eclipse-java-google-style.xml"

M.java_dir = java_dir
M.java_bin = java_bin
M.java_google_style_file = java_google_style_file

-- local trace_class_pattern = "(.-)([^/]-)%.([^%.]+)%((.-):(%d+)%)"
-- cp_path, method, file, line in string.gmatch(trace, "([%w%.%/%_-]*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
local java_mvn_class_pattern = "at%s+([^%s]-)([^/^%s]-)%.([^%.]+)%((.-)%.java:(%d+)%)"

-- local java_class_pattern = "%[([A-Z]+)%]%s+([^:]+)%:%[(%d+),(%d+)%]%s*([^\n]*)" -- for gmatch
local java_compile_java_pattern = "%[([A-Z]+)%]%s+([^:]+):%[(%d+),(%d+)%]%s*(.*)"

---@return { class_path: string,
---class_path_root: string,
---class_line_number: integer,
---line_start_position: integer,
---line_end_position: integer,
---method: string} | nil
M.parse_java_mvn_run_class_line = function(line)
    -- line = util.strip_ansi(line)
    local prefix, class_path, method, file_name, line_num = line:match(java_mvn_class_pattern)
    if class_path then
        return {
            class_path = class_path,
            class_path_root = vim.split(class_path, "%$")[1],
            class_line_number = (tonumber(line_num) or 1),
            line_start_position = string.find(line, prefix .. class_path) - 1,
            line_end_position = #line,
            method = method,
        }
    end
    return nil
end

---@return { file: string,
---lnum: integer,
---col: integer,
---end_col: integer,
---message: string,
---severity: string} | nil
M.parse_mvn_compile_java_line = function(line)
    -- line = util.strip_ansi(line)
    local level, file, lnum, col, msg = line:match(java_compile_java_pattern)
    if file then
        col = (tonumber(col) or 1) - 1
        return {
            file = file,
            lnum = (tonumber(lnum) or 1) - 1,
            col = col,
            end_col = col + 20,
            message = msg,
            -- severity = vim.diagnostic.severity.ERROR,
            severity = maven_util.to_severity(level),
        }
    end
    return nil
end

---@return [{ class_path: string,
---class_path_root: string,
---class_line_number: integer,
---line_start_position: integer,
---line_end_position: integer,
---method: string}]
M.parse_java_mvn_run_class_text = function(text)
    local items = {}
    for line in text:gmatch("[^\n]+") do
        local p = M.parse_java_mvn_run_class_line(line)
        if p then
            items[#items + 1] = p
        end
    end
    return items
end

---@return [{ file: string,
---lnum: integer,
---col: integer,
---end_col: integer,
---message: string,
---severity: string}]
M.parse_mvn_compile_java_text = function(text)
    local items = {}
    -- for line in trace:gmatch("[^\r\n]+") do
    for line in text:gmatch("[^\n]+") do
        local p = M.parse_mvn_compile_java_line(line)
        if p then
            items[#items + 1] = p
        end
    end
    return items
end

-- local root = vim.fn.getcwd()
-- local root = vim.fs.root(0, { ".git", "pom.xml", "mvnw", "gradlew", "build.gradle", "build.gradle.kts" })
-- local src_dir = root .. "/src/main/java/"
-- local test_dir = root .. "/src/test/java/"

M.java_class_to_proj_path = function(classname)
    local relative_path = classname:gsub("%.", "/") .. ".java"
    local file_path = vim.fn.glob("*/**/" .. relative_path)

    -- local file_path = vim.fn.findfile(file)
    -- resolve file full path from root

    if file_path ~= nil and #file_path ~= 0 then
        return file_path
    end

    return nil

    --[[ local full_path_src = src_dir .. relative_path
    local full_path_test = test_dir .. relative_path
    if util.is_file_exists(full_path_src) then
        return full_path_src
    elseif util.is_file_exists(full_path_test) then
        return full_path_test
    end
    -- then it's dependency lib
    return nil ]]
end

--[[ M.edit_java_resourse_file = function(resource_package_url)
    resource_package_url = resource_package_url or util.get_token_under_cursor('%"')
    local main_resource = string.format("%s/src/main/resources/%s", cwd, resource_package_url)
    local test_resource = string.format("%s/src/test/resources/%s", cwd, resource_package_url)
    local valid_url
    if util.is_file_exists(main_resource) then
        valid_url = main_resource
    elseif util.is_file_exists(test_resource) then
        valid_url = test_resource
    end
    if valid_url then
        util.edit_file("file:" .. valid_url)
    else
        vim.notify(string.format("⚠️ No such file %s exists", resource_package_url), vim.log.levels.INFO)
    end
end ]]

local java_root_files = {
    "pom.xml",
    "build.gradle",
    "build.gradle.kts",
    "settings.gradle",
    "settings.gradle.kts",
}

M.is_java_project = function()
    local root = vim.fn.getcwd()
    for _, f in ipairs(java_root_files) do
        if util.is_file_exists(root .. "/" .. f) then
            return true
        end
    end
    return false
end

local get_root_src_package_inner = function(src_dir)
    -- run your fd command
    local cmd = "fd -e java --type f . " .. src_dir .. " --exec dirname {} \\; | sort -u | head -n 1"
    local handle = io.popen(cmd)
    if not handle then
        return nil
    end

    local folder = handle:read("*a"):gsub("%s+$", "") -- trim newline

    handle:close()

    if folder == "" then
        return nil
    end

    -- find index of src/main/java inside path
    local idx = folder:find(src_dir)
    if not idx then
        return nil
    end

    -- extract everything after src/main/java/
    local pkg = folder:sub(idx + #src_dir + 1)

    -- convert path to package: "/" -> "."
    pkg = pkg:gsub("/", ".")

    return pkg
end

local root_src_package_cache = {}

M.get_root_src_package = function(src_dir)
    src_dir = src_dir or "src/main/java"
    local result = root_src_package_cache[src_dir]
    if result then
        return result
    end

    result = get_root_src_package_inner(src_dir)
    root_src_package_cache[src_dir] = result
    return result
end

M.get_root_src_main_package = function()
    return M.get_root_src_package("src/main/java")
end

M.get_root_src_test_package = function()
    return M.get_root_src_package("src/test/java")
end

return M
