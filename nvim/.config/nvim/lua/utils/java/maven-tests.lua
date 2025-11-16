local M = {}

local java_ts_util = require("utils.java.java-ts-util")

-- M.test = function()
--     -- run_maven({ "-q", "-DskipTests=false", "test" })
-- end
--
-- M.verify = function()
--     -- run_maven({ "-q", "verify" })
-- end

local function get_test_runner(test_name, debug)
    if debug then
        return 'mvn -q test -Dmaven.surefire.debug -Dtest="' .. test_name .. '"'
    end
    return 'mvn -q test -Dtest="' .. test_name .. '"'
end

local function get_verify_runner(debug)
    local cmd = "mvn -q verify -DskipAssembly -DskipInstall -DskipTests=false"
    if debug then
        return cmd .. " -Dmaven.surefire.debug"
    end
    return cmd
end

M.run_java_test_method = function(debug)
    -- local method_name = utils.get_current_full_method_name("\\#")
    local method_name = java_ts_util.get_full_method("\\#")
    vim.cmd("term " .. get_test_runner(method_name, debug))
end

M.run_java_test_method_debug = function()
    M.run_java_test_method(true)
end

M.run_java_test_class = function(debug)
    -- local class_name = utils.get_current_full_class_name()
    local class_name = java_ts_util.get_class_name()
    vim.cmd("term " .. get_test_runner(class_name, debug))
end

M.run_java_test_class_debug = function()
    M.run_java_test_class(true)
end

M.run_java_test_all = function(debug)
    vim.cmd("term " .. get_verify_runner(debug))
end

M.run_java_test_all_debug = function()
    M.run_java_test_all(true)
end

local function get_spring_boot_runner(profile, debug)
    local debug_param = ""
    if debug then
        debug_param =
            ' -Dspring-boot.run.jvmArguments="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=8000" '
    end

    local profile_param = ""
    if profile then
        profile_param = " -Dspring-boot.run.profiles=" .. profile .. " "
    end

    return "mvn spring-boot:run " .. profile_param .. debug_param
end
-- 15sp|term mvn spring-boot:run
-- 15sp|term mvn test -Dtest="testSomething"

M.run_spring_boot = function(debug)
    vim.cmd("15sp|term " .. get_spring_boot_runner(nil, debug))
end

M.run_spring_boot_debug = function()
    M.run_spring_boot(true)
end

return M
