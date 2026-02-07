local jdtls_classpath_util = require("utils.java.jdtls-classpath-util")
local java_ts_util = require("utils.java.java-ts-util")

return {
    name = "DEBUG_CURRENT",
    builder = function()
        local classpath = jdtls_classpath_util.get_classpath_for_main_method()
        local class_name = java_ts_util.get_class_name()
        local result_cmd = {
            "java",
            "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005",
            "-cp",
            classpath,
            class_name,
        }
        return {
            cmd = result_cmd,
            -- add some components that will pipe the output to quickfix,
            -- parse it using errorformat, and display any matching lines as diagnostics.
            components = {
                { "on_output_quickfix", set_diagnostics = true },
                "on_result_diagnostics",
                "on_exit_set_status",
                "on_complete_dispose",
                -- "on_complete_notify",
                "debug.dap_ui_output", -- mirror output to DAP REPL/console
            },
        }
    end,
    condition = {
        filetype = { "java" },
    },
}
