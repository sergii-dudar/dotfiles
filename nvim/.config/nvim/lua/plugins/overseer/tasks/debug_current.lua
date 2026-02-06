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
            },
        }
    end,
    condition = {
        filetype = { "java" },
    },
}

-- local build_resolvers = function()
--     local type_to_resolver = {}
--
--     type_to_resolver["java"] = function()
--         local java_debugger = require("plugins.overseer.tasks.runner.java-debugger")
--         return java_debugger.build_dap_config()
--     end
--
--     return type_to_resolver
-- end
--
-- return {
--     name = "DEBUG_CURRENT",
--     builder = function()
--         local resolvers = build_resolvers()
--         local type_resolver = resolvers[vim.bo.filetype]
--
--         if not type_resolver then
--             vim.notify("Debug not supported for filetype: " .. vim.bo.filetype, vim.log.levels.WARN)
--             return nil
--         end
--
--         local dap_config = type_resolver()
--         if not dap_config then
--             return nil
--         end
--
--         -- Store the config for rerun capability
--         vim.g.last_debug_config = dap_config
--
--         -- Start DAP session directly
--         vim.schedule(function()
--             require("dap").run(dap_config)
--         end)
--
--         -- Return a dummy task that completes immediately
--         -- This allows overseer to track it and enable <leader>rl to work
--         return {
--             cmd = { "echo", "Debug session started" },
--             components = { "default" },
--         }
--     end,
--     condition = {
--         filetype = { "java" },
--     },
-- }