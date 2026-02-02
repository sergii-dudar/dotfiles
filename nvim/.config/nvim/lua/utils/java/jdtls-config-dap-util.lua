local home = os.getenv("HOME")
local java_dir = vim.fn.glob(home .. "/.sdkman/candidates/java/current")
local java_bin = java_dir .. "/bin/java"
local java_ts_util = require("utils.java.java-ts-util")

local M = {}

local default_attach = {
    type = "java",
    request = "attach",
    name = "Debug (Attach) - Remote",
    hostName = "127.0.0.1",
    port = 5005,
}
local last_runned_dap_config = nil

local run_main_class_config = function(dap_config)
    require("dap").run(dap_config)
    vim.cmd("Neotree close")
end

M.rerun_last = function()
    --[[ if not last_runned_dap_config then
        vim.notify("❌ There no any previous DAP config to run", vim.log.levels.WARN)
        return
    end
    run_main_class_config(last_runned_dap_config) ]]
    require("dap").run_last()
    vim.cmd("Neotree close")
end

M.run_current_main_class = function()
    local class_name = java_ts_util.get_class_name()
    if not class_name then
        vim.notify("❌ Could not determine current class name for DAP run", vim.log.levels.WARN)
        return
    end

    last_runned_dap_config = {
        -- You need to extend the classPath to list your dependencies.
        -- `nvim-jdtls` would automatically add the `classPaths` property if it is missing
        -- classPaths = {},

        -- If using multi-module projects, remove otherwise.
        -- projectName = "yourProjectName",
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

function M.attach_to_remote(port)
    port = port or 5005
    vim.defer_fn(function()
        require("dap").run({
            type = "java",
            request = "attach",
            name = "Attach to Overseer (port 5005)",
            hostName = "127.0.0.1",
            port = 5005,
        })
        vim.cmd("Neotree close")
    end, 200) -- 1.5s; increase if your JVM is slow to start
end

return M
