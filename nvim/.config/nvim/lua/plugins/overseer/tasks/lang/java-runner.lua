local java_bin = vim.fn.glob("~/.sdkman/candidates/java/current/bin/java")

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
    local classpath = require("utils.java.jdtls-classpath-util").get_classpath_for_main_method()
    local class_name = require("utils.java.java-ts-util").get_class_name()
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

--- Attach to existing jvm dap session.
---@param port integer|nil 5005 if not specified.
function M.dap_attach_to_remote(port)
    require("utils.java.jdtls-config-dap-util").attach_to_remote(port)
end
-- vim.bo.filetype

---@return table DAP configuration
function M.build_dap_launch_config()
    local class_name = require("utils.java.java-ts-util").get_class_name()
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
