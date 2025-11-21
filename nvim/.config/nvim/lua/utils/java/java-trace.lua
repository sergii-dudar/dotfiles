local M = {}

local common = require("utils.common-util")
local java_common = require("utils.java.java-common")

local project_link_color = "#1E90FF"
local ex_link_color = "#8A2BE2"
vim.api.nvim_set_hl(0, "TraceProjectClassHl", {
    fg = project_link_color,
    underline = true,
    bold = true,
})
vim.api.nvim_set_hl(0, "TraceNonProjectClassHl", {
    fg = ex_link_color,
    underline = true,
    bold = true,
})

M.highlight_java_test_trace = function(buffer, namespace)
    if vim.api.nvim_buf_is_valid(buffer) then
        -- vim.notify("highlight", vim.log.levels.INFO)
        local lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)

        for i, line in ipairs(lines) do
            local trace = java_common.parse_java_trace_error_line(line)
            if trace then
                local line_ix = i - 1
                local file_path = java_common.java_class_to_proj_path(trace.class_path)
                local hl_group = file_path and "TraceProjectClassHl" or "TraceNonProjectClassHl"
                vim.api.nvim_buf_set_extmark(buffer, namespace, line_ix, trace.line_start_position, {
                    end_line = line_ix, -- only current line
                    end_col = trace.line_end_position,
                    hl_group = hl_group,
                })
            end

            --[[ if line:find("^%s*at ([%w%._$]+)%.([%w%._$]+)%(([%w%._$]+):(%d+)%)$") then
                local start_ix = string.find(line, "at") + 2
                -- vim.notify(line, vim.log.levels.INFO)
                local line_ix = i - 1
                vim.api.nvim_buf_set_extmark(buffer, namespace, line_ix, start_ix, {
                    end_line = line_ix, -- only current line
                    end_col = #line,
                    hl_group = "TraceProjectClassHl",
                })
            end ]]
        end
    end
end

M.highlight_java_compile_trace = function() end

M.parse_java_stack_trace = function(trace)
    local items = {}

    -- Match patterns like "Class.method(FileName.java:LineNumber)"
    --for cp_path, method, file, line in string.gmatch(trace, "at (.*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
    for cp_path, method, file, line in string.gmatch(trace, "([%w%.%/%_-]*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
        local file_no_ex = string.match(file, "([^.]+)")
        cp_path = string.gsub(cp_path, "." .. file_no_ex, "")
        local path = string.gsub(cp_path, "%.", "/")
        --print(path .. ", " .. method .. ", " .. file .. ", " .. line)

        -- local file_path = vim.fn.findfile(file)
        -- resolve file full path from root
        local file_path = vim.fn.glob("*/**/" .. path .. "/" .. file)

        --LazyVim.info("path: " .. file_path .. " file: " .. file)
        if file_path ~= nil and #file_path ~= 0 then
            -- LazyVim.info(file .. ": '" .. file_path .. "'")

            table.insert(items, {
                filename = file_path,
                lnum = tonumber(line),
                col = 1, -- Default to column 1
                --text = file .. " error location from stack trace"
                text = method,
            })
        end
    end

    dd(items)
    return items
end

local text = [[
warning: a terminally deprecated method in sun.misc.unsafe has been called
warning: sun.misc.unsafe::staticfieldbase has been called by com.google.inject.internal.aop.hiddenclassdefiner (file:/home/serhii/.sdkman/candidates/maven/current/lib/guice-5.1.0-classes.jar)
warning: please consider reporting this to the maintainers of class com.google.inject.internal.aop.hiddenclassdefiner
warning: sun.misc.unsafe::staticfieldbase will be removed in a future release
[error] tests run: 1, failures: 1, errors: 0, skipped: 0, time elapsed: 0.023 s <<< failure! -- in ua.serhii.application.tests2
[error] ua.serhii.application.tests2.testsomething3 -- time elapsed: 0.013 s <<< failure!
java.lang.assertionerror: some error to note

        at java.base/java.util.Objects.requireNonNull(Objects.java:246)
        at org.apache.commons.lang3.ObjectUtils.requireNonEmpty(ObjectUtils.java:1211)
        at ua.serhii.application.test.utils.TestUtil.assertThat(TestUtil.java:20)
        at ua.serhii.application.Tests2.testAssdrt(Tests2.java:23)
        at ua.serhii.application.Tests2.testSomething3(Tests2.java:18)
        at ua.serhii.application.test.utils.TestUtil.main(TestUtil.java:24)


[error] failures: 
[error]   tests2.testsomething3:18->testassdrt:23 some error to note
[error] tests run: 1, failures: 1, errors: 0, skipped: 0
[error] failed to execute goal org.apache.maven.plugins:maven-surefire-plugin:3.2.5:test (default-test) on project serhii-application: there are test failures.
[error] 
[error] please refer to /home/serhii/serhii.home/git/tests/serhii-application/target/surefire-reports for the individual test results.
[error] please refer to dump files (if any exist) [date].dump, [date]-jvmrun[n].dump and [date].dumpstream.
[error] -> [help 1]
[error] 
[error] to see the full stack trace of the errors, re-run maven with the -e switch.
[error] re-run maven using the -x switch to enable full debug logging.
[error] 
[error] for more information about the errors and possible solutions, please read the following articles:
[error] [help 1] http://cwiki.apache.org/confluence/display/maven/mojofailureexception

[process exited 1]
]]

M.parse_java_stack_trace2 = function(trace)
    local items = {}

    -- Match patterns like "Class.method(FileName.java:LineNumber)"
    --for cp_path, method, file, line in string.gmatch(trace, "at (.*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
    for cp_path, method, file, line in string.gmatch(trace, "([%w%.%/%_-]*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
        local file_no_ex = string.match(file, "([^.]+)")
        print(cp_path)

        cp_path = string.gsub(cp_path, "." .. file_no_ex, "")

        -- print(cp_path)

        local path = string.gsub(cp_path, "%.", "/")
        --print(path .. ", " .. method .. ", " .. file .. ", " .. line)

        -- print(cp_path)

        -- local file_path = vim.fn.findfile(file)
        -- resolve file full path from root
        local file_path = vim.fn.glob("*/**/" .. path .. "/" .. file)

        -- print(file_path)

        --LazyVim.info("path: " .. file_path .. " file: " .. file)
        if file_path ~= nil and #file_path ~= 0 then
            -- LazyVim.info(file .. ": '" .. file_path .. "'")

            table.insert(items, {
                filename = file_path,
                lnum = tonumber(line),
                col = 1, -- Default to column 1
                --text = file .. " error location from stack trace"
                text = method,
            })
        end
    end

    dd(items)
    return items
end

-- M.parse_java_stack_trace2(text)

M.show_stack_trace_qflist = function(stack_trace)
    local trace_items = M.parse_java_stack_trace(stack_trace)
    -- dd(trace_items)
    vim.fn.setqflist({}, "r", { title = "Trace Quickfix List", items = trace_items })
    vim.cmd("Trouble qflist toggle")
end

M.parse_selected_trace_to_qflist = function()
    local stack_trace = common.get_visual_selection()
    M.show_stack_trace_qflist(stack_trace)
end

M.parse_buffer_trace_to_qflist = function()
    local stack_trace = common.get_current_buffer_text()
    M.show_stack_trace_qflist(stack_trace)
end

return M
