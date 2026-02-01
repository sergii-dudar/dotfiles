local build_resolvers = function()
    local type_to_resolver = {}

    type_to_resolver["java"] = function()
        local java_debugger = require("plugins.overseer.tasks.debugger.java-debugger")
        return java_debugger.build_dap_config()
    end

    return type_to_resolver
end

return {
    name = "DEBUG_CURRENT",
    builder = function()
        local resolvers = build_resolvers()
        local type_resolver = resolvers[vim.bo.filetype]

        if not type_resolver then
            vim.notify("Debug not supported for filetype: " .. vim.bo.filetype, vim.log.levels.WARN)
            return nil
        end

        local dap_config = type_resolver()
        if not dap_config then
            return nil
        end

        -- Store the config for rerun capability
        vim.g.last_debug_config = dap_config

        -- Start DAP session directly
        vim.schedule(function()
            require("dap").run(dap_config)
        end)

        -- Return a dummy task that completes immediately
        -- This allows overseer to track it and enable <leader>rl to work
        return {
            cmd = { "echo", "Debug session started" },
            components = { "default" },
        }
    end,
    condition = {
        filetype = { "java" },
    },
}