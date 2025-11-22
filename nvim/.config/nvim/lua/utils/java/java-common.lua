local M = {}

local maven_util = require("utils.java.maven-util")
local util = require("utils.common-util")
-- local trace_class_pattern = "(.-)([^/]-)%.([^%.]+)%((.-):(%d+)%)"
-- cp_path, method, file, line in string.gmatch(trace, "([%w%.%/%_-]*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
local java_mvn_class_pattern = "at%s+([^%s]-)([^/^%s]-)%.([^%.]+)%((.-)%.java:(%d+)%)"

-- local java_class_pattern = "%[([A-Z]+)%]%s+([^:]+)%:%[(%d+),(%d+)%]%s*([^\n]*)" -- for gmatch
local java_compile_java_pattern = "%[([A-Z]+)%]%s+([^:]+):%[(%d+),(%d+)%]%s*(.*)"

---@return { class_path: string,
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

return M
