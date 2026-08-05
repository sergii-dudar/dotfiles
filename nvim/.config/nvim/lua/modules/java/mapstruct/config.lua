-- Shared configuration defaults for the MapStruct Neovim integration.

local M = {}

M.defaults = {
    -- Required by the Java IPC server.
    jar_path = "~/tools/java-extensions/mapstruct/mapstruct-path-explorer-repo/target/mapstruct-path-explorer.jar",

    -- Project classpath discovery.
    use_jdtls_classpath = true,
    classpath = nil,

    -- Java server process.
    java_cmd = "java",
    log_level = vim.log.levels.WARN,
    log_file = "~/.local/state/nvim/mapstruct-source-server.log",

    -- IPC lifecycle.
    socket_dir = "/tmp",
    start_connect_delay_ms = 1000,
    connect_timeout_ms = 2000,
    connect_poll_interval_ms = 100,
    heartbeat_interval_ms = 10000,
    request_timeout_ms = 5000,

    -- Completion retry policy for temporary classpath/compiler states.
    -- Delays back off exponentially: 250ms, 500ms -> ~750ms total. Kept short so a
    -- completion request never outlives the user's typing; longer compile windows are
    -- handled by simply re-triggering completion.
    path_retry_max_attempts = 3,
    path_retry_initial_delay_ms = 250,

    -- Verbose IPC payload dumps (vim.inspect of every request/response). Opt-in: very
    -- noisy (hundreds of log lines per completion response).
    log_ipc_payloads = false,
}

--- Return a copy of the default MapStruct options.
---@return table
function M.get_defaults()
    return vim.deepcopy(M.defaults)
end

--- Merge caller options over MapStruct defaults without mutating the defaults table.
---@param opts? table
---@return table
function M.merge(opts)
    return vim.tbl_deep_extend("force", M.get_defaults(), opts or {})
end

return M