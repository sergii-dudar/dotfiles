local helper = require("tests.utils.spec_helper")

describe("modules.java.mapstruct.server", function()
    local server
    local jobs, deferred, ipc

    local function noop() end

    before_each(function()
        local vim_double = helper.reset_vim()

        local logger = { debug = noop, info = noop, warn = noop, error = noop, set_level = noop }
        helper.stub_module("utils.logging-util", {
            new = function()
                return logger
            end,
            level_to_string = function()
                return "WARN"
            end,
            level_to_number = function(level)
                return level
            end,
        })
        helper.stub_module("utils.java.jdtls-classpath-util", {
            get_classpath = function()
                return nil
            end,
            set_log_level = noop,
        })

        ipc = { connect_calls = 0, disconnect_calls = 0, connected = false }
        ipc.connect = function(_, callback)
            ipc.connect_calls = ipc.connect_calls + 1
            ipc.connected = true
            if callback then
                callback(true, nil)
            end
            return true
        end
        ipc.disconnect = function()
            ipc.disconnect_calls = ipc.disconnect_calls + 1
            ipc.connected = false
        end
        ipc.is_connected = function()
            return ipc.connected
        end
        ipc.get_status = function()
            return { connected = ipc.connected, pending_requests = 0 }
        end
        ipc.request = function(_, _, callback)
            if callback then
                callback({}, nil)
            end
        end
        ipc.configure = noop
        ipc.set_log_level = noop
        helper.stub_module("modules.java.mapstruct.ipc_client", ipc)

        jobs = { next_id = 0, started = {}, stopped = {} }
        vim_double.fn.jobstart = function(cmd, opts)
            jobs.next_id = jobs.next_id + 1
            jobs.started[jobs.next_id] = { cmd = cmd, opts = opts }
            return jobs.next_id
        end
        vim_double.fn.jobstop = function(job_id)
            -- Record what the module still tracked at kill time.
            table.insert(jobs.stopped, { job_id = job_id, tracked = server.get_job_id() })
            return 1
        end
        vim_double.fn.getpid = function()
            return 4242
        end
        vim_double.fn.expand = function(path)
            return path
        end
        deferred = {}
        vim_double.defer_fn = function(fn, ms)
            table.insert(deferred, { fn = fn, ms = ms })
        end

        helper.reload("modules.java.mapstruct.config")
        server = helper.reload("modules.java.mapstruct.server")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "utils.logging-util",
            "utils.java.jdtls-classpath-util",
            "modules.java.mapstruct.ipc_client",
            "modules.java.mapstruct.config",
            "modules.java.mapstruct.server",
        })
    end)

    --- Start a server and run its deferred connect step; returns the job id.
    ---@return integer
    local function start_server()
        server.start("/jar", {}, noop)
        local connect_step = table.remove(deferred, 1)
        connect_step.fn()
        return jobs.next_id
    end

    it("ignores on_exit of a superseded job", function()
        -- given: job 1 was terminated and replaced by job 2
        local first = start_server()
        server.terminate()
        local second = start_server()
        local disconnects_before = ipc.disconnect_calls

        -- when: the old process finally exits
        jobs.started[first].opts.on_exit(first, 143, "exit")

        -- then: the replacement is untouched
        assert.are.equal(second, server.get_job_id())
        assert.is_true(server.is_running())
        assert.are.equal(disconnects_before, ipc.disconnect_calls)
    end)

    it("cleans up when the current job exits", function()
        -- given
        local job = start_server()

        -- when
        jobs.started[job].opts.on_exit(job, 1, "exit")

        -- then
        assert.is_nil(server.get_job_id())
        assert.is_false(server.is_running())
        assert.is_true(ipc.disconnect_calls >= 1)
    end)

    it("terminate clears the tracked job before killing the process", function()
        -- given
        local job = start_server()

        -- when
        server.terminate()

        -- then: jobstop saw no tracked job, so the pending on_exit is a no-op
        assert.are.same({ { job_id = job, tracked = nil } }, jobs.stopped)
        assert.is_nil(server.get_job_id())
        assert.is_false(server.is_running())
    end)

    it("stop only force-stops the job it was asked to stop", function()
        -- given: stop was requested, then the process exited on its own and was replaced
        local first = start_server()
        server.stop(noop)
        local force_stop = table.remove(deferred, 1)
        jobs.started[first].opts.on_exit(first, 0, "exit")
        local second = start_server()

        -- when: the deferred force-stop fires
        force_stop.fn()

        -- then: the replacement survives
        assert.are.same({}, jobs.stopped)
        assert.are.equal(second, server.get_job_id())
    end)
end)
