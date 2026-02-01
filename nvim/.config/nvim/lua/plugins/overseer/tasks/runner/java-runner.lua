local jdtls_classpath_util = require("utils.java.jdtls-classpath-util")
local java_ts_util = require("utils.java.java-ts-util")

local M = {}

---@return table
function M.build_cmd()
    local classpath = jdtls_classpath_util.get_classpath_for_main_method()
    local class_name = java_ts_util.get_class_name()
    return { "java", "-cp", classpath, class_name }
end

return M
