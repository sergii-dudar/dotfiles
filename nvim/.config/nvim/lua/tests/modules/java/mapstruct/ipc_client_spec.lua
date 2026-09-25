local helper = require("tests.utils.spec_helper")

describe("modules.java.mapstruct.ipc_client", function()
    local ipc_client
    local pipe, timer, writes

    local function noop() end

    before_each(function()
        local vim_double = helper.reset_vim()

        local logger = { debug = noop, info = noop, warn = noop, error = noop, set_level = noop }
        helper.stub_module("utils.logging-util", {
            new = function()
                return logger
            end,
        })

        writes = {}
        pipe = {
            connect = function(self, _, callback)
                self.on_connect = callback
            end,
            read_start = function(self, callback)
                self.on_read = callback
            end,
            write = function(_, data, callback)
                table.insert(writes, data)
                if callback then
                    callback(nil)
                end
            end,
            is_closing = function()
                return false
            end,
            close = noop,
        }
        timer = {
            start = function(self, _, _, callback)
                self.callback = callback
            end,
            stop = noop,
            close = noop,
        }

        vim_double.uv.new_pipe = function()
            return pipe
        end
        vim_double.uv.new_timer = function()
            return timer
        end
        vim_double.uv.fs_stat = function()
            return { type = "socket" }
        end
        vim_double.uv.now = function()
            return 0
        end
        vim_double.schedule_wrap = function(fn)
            return function(...)
                return fn(...)
            end
        end
        vim_double.defer_fn = noop
        -- The double's decoder is deliberately unconfigured; this spec only ever reads back one
        -- response line, so decode it to the matching table.
        vim_double.json.decode = function()
            return { id = "1", result = { className = "x", completions = {} } }
        end
        vim_double.tbl_keys = function(tbl)
            local keys = {}
            for key in pairs(tbl) do
                table.insert(keys, tostring(key))
            end
            return keys
        end

        helper.reload("modules.java.mapstruct.config")
        ipc_client = helper.reload("modules.java.mapstruct.ipc_client")

        ipc_client.connect("/tmp/test.sock", noop)
        pipe.on_connect(nil)
    end)

    after_each(function()
        helper.clear_stub_modules({
            "utils.logging-util",
            "modules.java.mapstruct.config",
            "modules.java.mapstruct.ipc_client",
        })
    end)

    --- Count frames written to the socket that carry the given method.
    ---@param method string
    ---@return integer
    local function frames_for(method)
        local count = 0
        for _, frame in ipairs(writes) do
            if frame:find('"method"%s*:%s*"' .. method .. '"') then
                count = count + 1
            end
        end
        return count
    end

    it("connects and starts the heartbeat timer", function()
        assert.is_true(ipc_client.is_connected())
        assert.is_function(timer.callback)
    end)

    it("sends a heartbeat when idle", function()
        -- when
        timer.callback()

        -- then
        assert.are.equal(1, frames_for("heartbeat"))
    end)

    it("skips the heartbeat while a request is in flight and resumes once it is answered", function()
        -- given: an explore_path request waiting for its answer
        ipc_client.request("explore_path", { pathExpression = "" }, noop)
        assert.are.equal(1, frames_for("explore_path"))

        -- when: the heartbeat timer fires meanwhile
        timer.callback()

        -- then: no heartbeat is queued behind the slow request
        assert.are.equal(0, frames_for("heartbeat"))

        -- when: the server answers and the timer fires again
        pipe.on_read(nil, '{"id":"1","result":{"className":"x","completions":[]}}\n')
        timer.callback()

        -- then
        assert.are.equal(1, frames_for("heartbeat"))
    end)
end)
