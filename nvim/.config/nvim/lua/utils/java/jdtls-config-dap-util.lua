local home = os.getenv("HOME")
local java_dir = vim.fn.glob(home .. "/.sdkman/candidates/java/current")
local java_bin = java_dir .. "/bin/java"
local java_ts_util = require("utils.java.java-ts-util")

local M = {}

M.default_dap_port = 5005
local default_attach_config = {
    type = "java",
    request = "attach",
    name = "Debug (Attach) - Remote",
    -- hostName = "127.0.0.1",
    host = "localhost",
    port = M.default_dap_port,
    -- projectName = project_name,
}
M.default_attach_config = default_attach_config
local last_runned_dap_config = nil

local run_main_class_config = function(dap_config)
    require("dap").run(dap_config)
end

M.rerun_last = function()
    --[[ if not last_runned_dap_config then
        vim.notify("❌ There no any previous DAP config to run", vim.log.levels.WARN)
        return
    end
    run_main_class_config(last_runned_dap_config) ]]
    require("dap").run_last()
end

M.run_current_main_class = function()
    local class_name = java_ts_util.get_class_name()
    if not class_name then
        vim.notify("❌ Could not determine current class name for DAP run", vim.log.levels.WARN)
        return
    end

    local classpath = require("utils.java.jdtls-classpath-util").get_classpath_for_main_method_table()
    last_runned_dap_config = {
        -- You need to extend the classPath to list your dependencies.
        -- `nvim-jdtls` would automatically add the `classPaths` property if it is missing
        classPaths = classpath,

        -- If using multi-module projects, remove otherwise.
        projectName = "payment-norkom-adapter-service", -- TODO: make dynamic
        javaExec = java_bin,
        mainClass = class_name,

        -- If using the JDK9+ module system, this needs to be extended
        -- `nvim-jdtls` would automatically populate this property
        modulePaths = {},
        name = "Launch Current Main Class",
        request = "launch",
        type = "java",
    }
    run_main_class_config(last_runned_dap_config)
end

--- Attach to existing jvm dap session.
---@param port integer|nil 5005 if not specified.
function M.attach_to_remote(port)
    port = port or M.default_dap_port
    vim.defer_fn(function()
        require("dap").run(default_attach_config)
    end, 200) -- ms; increase if your JVM is slow to start
end

return M
