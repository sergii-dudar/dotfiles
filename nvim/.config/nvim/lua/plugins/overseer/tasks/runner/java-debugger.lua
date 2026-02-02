local jdtls_classpath_util = require("utils.java.jdtls-classpath-util")
local java_ts_util = require("utils.java.java-ts-util")

local M = {}

---@return table DAP configuration
function M.build_dap_config()
    local home = os.getenv("HOME")
    local java_dir = vim.fn.glob(home .. "/.sdkman/candidates/java/current")
    local java_bin = java_dir .. "/bin/java"

    local class_name = java_ts_util.get_class_name()
    if not class_name then
        vim.notify("❌ Could not determine current class name for debug", vim.log.levels.WARN)
        return nil
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