-- Server Lifecycle Manager for MapStruct IPC
-- Handles starting, stopping, and monitoring the Java server

local ipc_client = require("utils.blink.mapstruct-source.ipc_client")

local M = {}

-- State
local state = {
    server_job_id = nil,
    socket_path = nil,
    jar_path = nil,
    is_starting = false,
}

-- Generate unique socket path for this Neovim instance
local function generate_socket_path()
    -- local tmpdir = vim.fn.getenv("TMPDIR") or "/tmp"
    local tmpdir = "/tmp"
    print(tmpdir)
    local nvim_pid = vim.fn.getpid()
    return string.format("%s/mapstruct-ipc-%d.sock", tmpdir, nvim_pid)
end

-- Find project root directory
local function find_project_root(bufnr)
    local file_path = vim.api.nvim_buf_get_name(bufnr)
    local dir = vim.fn.fnamemodify(file_path, ":p:h")

    -- Look for pom.xml or build.gradle
    while dir ~= "/" and dir ~= "." do
        if
            vim.fn.filereadable(dir .. "/pom.xml") == 1
            or vim.fn.filereadable(dir .. "/build.gradle") == 1
            or vim.fn.filereadable(dir .. "/build.gradle.kts") == 1
        then
            return dir
        end
        dir = vim.fn.fnamemodify(dir, ":h")
    end

    return nil
end

-- Get classpath from project structure (fallback)
local function get_classpath_from_project(bufnr)
    local project_root = find_project_root(bufnr)
    if not project_root then
        vim.notify("[MapStruct Server] Could not find project root", vim.log.levels.DEBUG)
        return nil
    end

    vim.notify("[MapStruct Server] Project root: " .. project_root, vim.log.levels.DEBUG)

    -- Build classpath from standard Maven/Gradle locations
    local classpaths = {}

    -- Maven structure
    local maven_paths = {
        project_root .. "/target/classes",
        project_root .. "/target/test-classes",
    }

    -- Gradle structure
    local gradle_paths = {
        project_root .. "/build/classes/java/main",
        project_root .. "/build/classes/java/test",
    }

    -- Check which build tool
    local use_maven = vim.fn.filereadable(project_root .. "/pom.xml") == 1
    local paths_to_check = use_maven and maven_paths or gradle_paths

    for _, path in ipairs(paths_to_check) do
        if vim.fn.isdirectory(path) == 1 then
            table.insert(classpaths, path)
            vim.notify("[MapStruct Server] Found classpath: " .. path, vim.log.levels.DEBUG)
        end
    end

    if #classpaths > 0 then
        return table.concat(classpaths, ":")
    end

    return nil
end

-- Get classpath from nvim-jdtls with fallback
local function get_jdtls_classpath()
    local bufnr = vim.api.nvim_get_current_buf()

    -- Try jdtls first
    local ok, jdtls = pcall(require, "jdtls")
    if ok then
        -- Find jdtls client
        local clients = vim.lsp.get_clients({ name = "jdtls" })
        if clients and #clients > 0 then
            local client = clients[1]
            local uri = vim.uri_from_bufnr(bufnr)

            vim.notify("[MapStruct Server] Querying jdtls for classpath...", vim.log.levels.DEBUG)

            -- Try to get both runtime and test classpaths
            local all_classpaths = {}

            -- Query with 'test' scope to get test dependencies
            local result_test, err = client:request_sync("workspace/executeCommand", {
                command = "java.project.getClasspaths",
                arguments = { uri, vim.json.encode({ scope = "test" }) },
            }, 10000, bufnr)

            if result_test and result_test.result then
                local classpaths = result_test.result.classpaths or result_test.result
                if type(classpaths) == "table" and #classpaths > 0 then
                    vim.notify(
                        "[MapStruct Server] Got " .. #classpaths .. " test classpath entries from jdtls",
                        vim.log.levels.INFO
                    )
                    for _, cp in ipairs(classpaths) do
                        table.insert(all_classpaths, cp)
                    end
                end
            else
                -- Try runtime scope as fallback
                local result_runtime, err = client:request_sync("workspace/executeCommand", {
                    command = "java.project.getClasspaths",
                    arguments = { uri, vim.json.encode({ scope = "runtime" }) },
                }, 10000, bufnr)

                if result_runtime and result_runtime.result then
                    local classpaths = result_runtime.result.classpaths or result_runtime.result
                    if type(classpaths) == "table" and #classpaths > 0 then
                        vim.notify(
                            "[MapStruct Server] Got " .. #classpaths .. " runtime classpath entries from jdtls",
                            vim.log.levels.INFO
                        )
                        for _, cp in ipairs(classpaths) do
                            table.insert(all_classpaths, cp)
                        end
                    end
                end
            end

            if #all_classpaths > 0 then
                vim.notify("[MapStruct Server] Total jdtls classpath entries: " .. #all_classpaths, vim.log.levels.INFO)
                return table.concat(all_classpaths, ":")
            end

            if err then
                vim.notify("[MapStruct Server] jdtls request error: " .. vim.inspect(err), vim.log.levels.DEBUG)
            end
        else
            vim.notify("[MapStruct Server] No jdtls client found", vim.log.levels.DEBUG)
        end
    end

    -- Fallback to project structure
    vim.notify("[MapStruct Server] Falling back to project structure classpath", vim.log.levels.INFO)
    return get_classpath_from_project(bufnr)
end

-- Start the Java IPC server
function M.start(jar_path, opts, callback)
    opts = opts or {}

    if state.server_job_id then
        vim.notify("[MapStruct] Server already running", vim.log.levels.WARN)
        if callback then
            callback(true, state.socket_path)
        end
        return
    end

    if state.is_starting then
        vim.notify("[MapStruct] Server is already starting", vim.log.levels.WARN)
        return
    end

    state.is_starting = true
    state.jar_path = jar_path
    state.socket_path = generate_socket_path()

    -- Build classpath
    local classpath = jar_path

    -- Try to get classpath from jdtls if enabled (default: true)
    if opts.use_jdtls_classpath ~= false then
        local jdtls_cp = get_jdtls_classpath()
        if jdtls_cp then
            classpath = classpath .. ":" .. jdtls_cp
            vim.notify("[MapStruct] Using jdtls classpath", vim.log.levels.INFO)
        elseif opts.classpath then
            classpath = classpath .. ":" .. opts.classpath
        end
    elseif opts.classpath then
        classpath = classpath .. ":" .. opts.classpath
    end

    -- Build command
    local cmd = {
        opts.java_cmd or "java",
        "-cp",
        classpath,
        "com.dsm.mapstruct.IpcServer",
        state.socket_path,
    }

    vim.notify("[MapStruct] Starting server on " .. state.socket_path, vim.log.levels.INFO)

    -- Start server as background job
    state.server_job_id = vim.fn.jobstart(cmd, {
        on_stdout = function(_, data, _)
            if data and #data > 0 then
                for _, line in ipairs(data) do
                    if line ~= "" then
                        vim.notify("[MapStruct Server] " .. line, vim.log.levels.DEBUG)
                    end
                end
            end
        end,
        on_stderr = function(_, data, _)
            if data and #data > 0 then
                for _, line in ipairs(data) do
                    if line ~= "" then
                        vim.notify("[MapStruct Server Error] " .. line, vim.log.levels.ERROR)
                    end
                end
            end
        end,
        on_exit = function(_, exit_code, _)
            vim.notify("[MapStruct] Server exited with code " .. exit_code, vim.log.levels.WARN)
            M.cleanup()
        end,
    })

    if state.server_job_id <= 0 then
        vim.notify("[MapStruct] Failed to start server", vim.log.levels.ERROR)
        state.server_job_id = nil
        state.is_starting = false
        if callback then
            callback(false, "Failed to start server")
        end
        return
    end

    -- Give server time to start, then connect
    vim.defer_fn(function()
        state.is_starting = false
        ipc_client.connect(state.socket_path, function(success, err)
            if callback then
                if success then
                    callback(true, state.socket_path)
                else
                    callback(false, err)
                end
            end
        end)
    end, 1000)
end

-- Stop the server
function M.stop(callback)
    if not state.server_job_id then
        if callback then
            callback(true)
        end
        return
    end

    -- Send shutdown request
    if ipc_client.is_connected() then
        ipc_client.request("shutdown", {}, function()
            -- Server will shut down
        end)
    end

    -- Give server time to shut down gracefully
    vim.defer_fn(function()
        if state.server_job_id then
            vim.fn.jobstop(state.server_job_id)
        end

        M.cleanup()

        if callback then
            callback(true)
        end
    end, 200)
end

-- Restart the server
function M.restart(callback)
    vim.notify("[MapStruct] Restarting server...", vim.log.levels.INFO)
    M.stop(function()
        vim.defer_fn(function()
            if state.jar_path then
                M.start(state.jar_path, {}, callback)
            end
        end, 500)
    end)
end

-- Cleanup server resources
function M.cleanup()
    ipc_client.disconnect()

    if state.server_job_id then
        state.server_job_id = nil
    end

    -- Clean up socket file
    if state.socket_path and vim.fn.filereadable(state.socket_path) == 1 then
        vim.fn.delete(state.socket_path)
    end

    state.is_starting = false
end

-- Check if server is running
function M.is_running()
    return state.server_job_id ~= nil
end

-- Get server status
function M.get_status()
    return {
        running = state.server_job_id ~= nil,
        starting = state.is_starting,
        socket_path = state.socket_path,
        jar_path = state.jar_path,
        ipc_status = ipc_client.get_status(),
    }
end

-- Get socket path
function M.get_socket_path()
    return state.socket_path
end

return M
