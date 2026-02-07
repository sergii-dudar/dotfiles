local jdtls_classpath_util = require("utils.java.jdtls-classpath-util")
local java_ts_util = require("utils.java.java-ts-util")

local home = os.getenv("HOME")
local java_dir = vim.fn.glob(home .. "/.sdkman/candidates/java/current")
local java_bin = java_dir .. "/bin/java"

local M = {}

---@return table
function M.build_cmd()
    return build_java_cmd()
end

---@return table
function M.build_debug_cmd()
    return build_java_cmd(true)
end

---@param is_debug boolean|nil
function build_java_cmd(is_debug)
    local classpath = jdtls_classpath_util.get_classpath_for_main_method()
    local class_name = java_ts_util.get_class_name()
    if not class_name then
        vim.notify("❌ Could not determine current class name for debug", vim.log.levels.WARN)
        return {}
    end

    if not is_debug then
        return { java_bin, "-cp", classpath, class_name }
    end

    return {
        java_bin,
        "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005",
        "-cp",
        classpath,
        class_name,
    }
end

---@return table DAP configuration
function M.build_dap_launch_config()
    local class_name = java_ts_util.get_class_name()
    if not class_name then
        vim.notify("❌ Could not determine current class name for debug", vim.log.levels.WARN)
    end

    return {
        type = "java",
        request = "launch",
        name = "Debug Current Main Class (Overseer)",
        javaExec = java_bin,
        mainClass = class_name,
        modulePaths = {},
    }
end

return M
