local M = {}

--local util = require("utils.common-util")
-- local trace_class_pattern = "(.-)([^/]-)%.([^%.]+)%((.-):(%d+)%)"
-- cp_path, method, file, line in string.gmatch(trace, "([%w%.%/%_-]*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
local trace_class_pattern = "([^%s]-)([^/^%s]-)%.([^%.]+)%((.-):(%d+)%)"

-- local root = vim.fn.getcwd()
-- local root = vim.fs.root(0, { ".git", "pom.xml", "mvnw", "gradlew", "build.gradle", "build.gradle.kts" })
-- local src_dir = root .. "/src/main/java/"
-- local test_dir = root .. "/src/test/java/"

M.java_class_to_proj_path = function(classname)
    local relative_path = classname:gsub("%.", "/") .. ".java"
    local file_path = vim.fn.glob("*/**/" .. relative_path)

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

----------------------------------------
---@return { class_path: string,
---class_line_number: integer,
---line_start_position: integer,
---line_end_position: integer } | nil
M.parse_java_trace_error_line = function(line)
    local prefix, class_path, method, file_name, line_num = line:match(trace_class_pattern)
    if class_path then
        return {
            class_path = class_path,
            class_line_number = line_num,
            line_start_position = string.find(line, prefix .. class_path) - 1,
            line_end_position = #line,
        }
    end
    return nil
end

--[[ local line1 = "at java.base/java.util.Objects.requireNonNull(Objects.java:246)"
local line2 = "at org.apache.commons.lang3.ObjectUtils.requireNonEmpty(ObjectUtils.java:1211)"
local line3 = "at ua.serhii.application.test.utils.testutil.assertthat(testutil.java:10)"
local line4 = "at ua.serhii.application.tests2.testassdrt(tests2.java:23)"
local line5 = "at ua.serhii.application.tests2.testsomething3(tests2.java:18)"

dd({
    M.parse_java_trace_error_line(line1),
    M.parse_java_trace_error_line(line2),
    M.parse_java_trace_error_line(line3),
    M.parse_java_trace_error_line(line4),
    M.parse_java_trace_error_line(line5),
}) ]]

-- local term_stacktrace_ns = vim.api.nvim_create_namespace("TermStacktrace")

-- 2. Target buffer and line index
-- local buf = vim.fn.bufnr("/home/serhii/dotfiles/test.txt")
-- M.highlight_java_test_trace(buf, term_stacktrace_ns)

----------------------------------------

return M