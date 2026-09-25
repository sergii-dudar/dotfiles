local helper = require("tests.utils.spec_helper")

describe("modules.java.mapstruct server orchestration", function()
    local mapstruct
    local fake_server, ipc

    local function noop() end

    local stubbed_modules = {
        "utils.logging-util",
        "utils.ui.spinner",
        "modules.java.mapstruct.context",
        "utils.java.jdtls-classpath-util",
        "modules.java.mapstruct.server",
        "modules.java.mapstruct.ipc_client",
        "modules.java.mapstruct.config",
        "modules.java.mapstruct",
    }

    before_each(function()
        local vim_double = helper.reset_vim()

        local logger = { debug = noop, info = noop, warn = noop, error = noop, set_level = noop }
        helper.stub_module("utils.logging-util", {
            new = function()
                return logger
            end,
            level_to_number = function(level)
                return level
            end,
            level_to_string = function()
                return "WARN"
            end,
        })
        helper.stub_module("utils.ui.spinner", { start = noop, stop = noop })
        helper.stub_module("modules.java.mapstruct.context", { set_log_level = noop, stop_cleanup_timer = noop })
        helper.stub_module("utils.java.jdtls-classpath-util", {
            is_jdtls_ready = function()
                return true
            end,
            clear_cache = noop,
            set_log_level = noop,
        })

        ipc = { connected = false, requests = {} }
        helper.stub_module("modules.java.mapstruct.ipc_client", {
            set_log_level = noop,
            configure = noop,
            is_connected = function()
                return ipc.connected
            end,
            get_status = function()
                return { connected = ipc.connected, pending_requests = 0 }
            end,
            request = function(method, params, callback)
                table.insert(ipc.requests, { method = method, params = params, callback = callback })
            end,
        })

        fake_server = { job_id = nil, next_job = 0, start_calls = 0, terminate_calls = 0, pending_start = nil }
        helper.stub_module("modules.java.mapstruct.server", {
            set_log_level = noop,
            is_running = function()
                return fake_server.job_id ~= nil
            end,
            get_job_id = function()
                return fake_server.job_id
            end,
            get_status = function()
                return {
                    running = fake_server.job_id ~= nil,
                    starting = fake_server.pending_start ~= nil,
                    ipc_status = { connected = ipc.connected, pending_requests = 0 },
                }
            end,
            start = function(_, _, callback)
                fake_server.start_calls = fake_server.start_calls + 1
                fake_server.next_job = fake_server.next_job + 1
                fake_server.job_id = fake_server.next_job
                fake_server.pending_start = callback
            end,
            terminate = function()
                fake_server.terminate_calls = fake_server.terminate_calls + 1
                fake_server.job_id = nil
                ipc.connected = false
            end,
            stop = function(callback)
                fake_server.job_id = nil
                ipc.connected = false
                if callback then
                    callback(true)
                end
            end,
            get_socket_path = function()
                return "/tmp/test.sock"
            end,
        })

        vim_double.fn.filereadable = function()
            return 1
        end
        vim_double.fn.expand = function(path)
            return path
        end
        vim_double.api.nvim_create_augroup = function()
            return 1
        end
        vim_double.api.nvim_create_autocmd = noop
        vim_double.defer_fn = function(fn)
            fn()
        end

        helper.reload("modules.java.mapstruct.config")
        mapstruct = helper.reload("modules.java.mapstruct")
        assert.is_true(mapstruct.setup({ jar_path = "/jar" }))
    end)

    after_each(function()
        helper.clear_stub_modules(stubbed_modules)
    end)

    --- Complete the in-flight server start with the given outcome.
    ---@param success boolean
    local function finish_start(success)
        local callback = fake_server.pending_start
        fake_server.pending_start = nil
        ipc.connected = success
        callback(success, success and "/tmp/test.sock" or "boom")
    end

    --- Issue an explore_type_source request and collect its outcome.
    ---@param results table
    local function explore(results)
        mapstruct.explore_type_source({ typeName = "a.A" }, function(result, err)
            table.insert(results, { result = result, err = err })
        end)
    end

    it("starts the server once for concurrent callers and answers them all", function()
        -- given
        local results = {}

        -- when: two requests arrive while the server is down
        explore(results)
        explore(results)

        -- then: a single start, nothing sent yet
        assert.are.equal(1, fake_server.start_calls)
        assert.are.equal(0, #ipc.requests)

        -- when: the start completes
        finish_start(true)

        -- then: both requests go out and both callers are answered
        assert.are.equal(2, #ipc.requests)
        for _, request in ipairs(ipc.requests) do
            request.callback({ sourcePath = "/p" }, nil)
        end
        assert.are.equal(2, #results)
        assert.are.same({ sourcePath = "/p" }, results[1].result)
        assert.are.same({ sourcePath = "/p" }, results[2].result)
    end)

    it("reports a failed start to every queued caller", function()
        -- given
        local results = {}
        explore(results)
        explore(results)

        -- when
        finish_start(false)

        -- then
        assert.are.equal(1, fake_server.start_calls)
        assert.are.equal(2, #results)
        assert.are.equal("Server is not running", results[1].err)
        assert.are.equal("Server is not running", results[2].err)
    end)

    it("refuses to start when the module is not initialized", function()
        -- given: setup fails because the jar is missing
        helper.clear_stub_modules("modules.java.mapstruct")
        _G.vim.fn.filereadable = function()
            return 0
        end
        local uninitialized = require("modules.java.mapstruct")
        local outcome = {}

        -- when
        uninitialized.restart(function(success, err)
            outcome = { success = success, err = err }
        end)

        -- then: no process is spawned and the caller learns why
        assert.is_false(outcome.success)
        assert.matches("not initialized", outcome.err)
        assert.are.equal(0, fake_server.start_calls)
    end)

    it("terminates the failed process once and restarts once when several requests lose the connection", function()
        -- given: two requests in flight on job 1
        local results = {}
        explore(results)
        finish_start(true)
        explore(results)
        assert.are.equal(2, #ipc.requests)

        -- when: both fail together (a disconnect fails every pending request)
        ipc.requests[1].callback(nil, "timeout")
        ipc.requests[2].callback(nil, "timeout")

        -- then: the old process is killed once and exactly one replacement is started
        assert.are.equal(1, fake_server.terminate_calls)
        assert.are.equal(2, fake_server.start_calls)
        assert.are.equal(2, #ipc.requests)

        -- when: the replacement comes up
        finish_start(true)

        -- then: both requests are retried on it
        assert.are.equal(4, #ipc.requests)
        assert.are.equal(0, #results)
    end)
end)
