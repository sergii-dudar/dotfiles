local helper = require("tests.utils.spec_helper")

describe("utils.java.jdtls-document-validation", function()
    local M
    local client, sent, autocmds, deleted_groups, timer

    --- Fire every autocmd callback registered for `event` on `bufnr`.
    local function fire(event, bufnr, data)
        for _, au in ipairs(autocmds) do
            if au.buffer == bufnr and vim.tbl_contains(au.events, event) then
                au.callback({ event = event, buf = bufnr, data = data })
            end
        end
    end

    local function validated_uris()
        local uris = {}
        for _, call in ipairs(sent) do
            if call.method == "java/validateDocument" then
                table.insert(uris, call.params.textDocument.uri)
            end
        end
        return uris
    end

    before_each(function()
        helper.reset_vim()
        sent = {}
        autocmds = {}
        deleted_groups = {}
        timer = { started = 0, stopped = 0, closed = false }
        function timer:start(delay, _, callback)
            self.started = self.started + 1
            self.delay = delay
            self.callback = callback
        end
        function timer:stop()
            self.stopped = self.stopped + 1
        end
        function timer:close()
            self.closed = true
        end

        client = {
            id = 7,
            name = "jdtls",
            stopped = false,
            notify = function(_, method, params)
                table.insert(sent, { method = method, params = params })
                return true
            end,
            is_stopped = function(self)
                return self.stopped
            end,
        }

        local next_group = 100
        vim.api.nvim_create_augroup = function()
            next_group = next_group + 1
            return next_group
        end
        vim.api.nvim_del_augroup_by_id = function(id)
            table.insert(deleted_groups, id)
        end
        vim.api.nvim_create_autocmd = function(events, opts)
            table.insert(autocmds, {
                events = type(events) == "table" and events or { events },
                buffer = opts.buffer,
                group = opts.group,
                callback = opts.callback,
            })
        end
        vim.api.nvim_buf_is_valid = function(bufnr)
            return bufnr ~= 99
        end
        vim.lsp.buf_is_attached = function(bufnr, client_id)
            return client_id == 7 and bufnr ~= 98
        end
        vim.uri_from_bufnr = function(bufnr)
            return "file:///repo/B" .. bufnr .. ".java"
        end
        vim.uv = {
            new_timer = function()
                return timer
            end,
        }
        vim.schedule_wrap = function(fn)
            return fn
        end

        M = helper.reload("utils.java.jdtls-document-validation")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.java.jdtls-document-validation" })
    end)

    it("tracks only jdtls clients and valid buffers", function()
        M.setup({ id = 1, name = "lua_ls" }, 1)
        M.setup(client, 99)
        assert.are.equal(0, #autocmds)
        assert.is_nil(M._state(1))
        assert.is_nil(M._state(7))
    end)

    it("does not re-validate a buffer nothing has invalidated", function()
        M.setup(client, 1)
        M.setup(client, 2)
        fire("BufEnter", 2)
        fire("BufEnter", 1)
        assert.are.same({}, validated_uris())
    end)

    it("re-validates a buffer once after another buffer changed", function()
        M.setup(client, 1)
        M.setup(client, 2)
        fire("TextChangedI", 1)
        fire("TextChangedI", 1)

        fire("BufEnter", 1) -- the edited buffer: jdtls validated it itself
        assert.are.same({}, validated_uris())

        fire("BufEnter", 2)
        fire("BufEnter", 2)
        assert.are.same({ "file:///repo/B2.java" }, validated_uris())

        fire("TextChanged", 2)
        fire("BufEnter", 1)
        assert.are.same({ "file:///repo/B2.java", "file:///repo/B1.java" }, validated_uris())
    end)

    it("setting up the same buffer twice registers it once", function()
        M.setup(client, 1)
        local n = #autocmds
        M.setup(client, 1)
        assert.are.equal(n, #autocmds)
    end)

    it("refreshes the other stale buffers a moment after a save, coalescing saves", function()
        M.setup(client, 1)
        M.setup(client, 2)
        M.setup(client, 3)
        fire("TextChanged", 1)
        fire("BufWritePost", 1)
        fire("BufWritePost", 1)
        assert.are.equal(2, timer.started)
        assert.are.equal(500, timer.delay)
        assert.are.same({}, validated_uris())

        timer.callback()
        local uris = validated_uris()
        table.sort(uris)
        assert.are.same({ "file:///repo/B2.java", "file:///repo/B3.java" }, uris)

        -- everything is current now
        fire("BufEnter", 2)
        fire("BufEnter", 3)
        timer.callback()
        assert.are.equal(2, #validated_uris())
    end)

    it("skips buffers that are no longer attached or valid, and stopped clients", function()
        M.setup(client, 1)
        M.setup(client, 98) -- attached check fails for 98
        fire("TextChanged", 1)
        fire("BufEnter", 98)
        assert.are.same({}, validated_uris())

        client.stopped = true
        M.setup(client, 2)
        fire("BufEnter", 2)
        assert.are.same({}, validated_uris())
    end)

    it("stops tracking on detach and wipeout and drops the client state with the last buffer", function()
        M.setup(client, 1)
        M.setup(client, 2)
        fire("TextChanged", 1)
        fire("BufWritePost", 1)

        fire("LspDetach", 1, { client_id = 8 }) -- another client detaching: ignored
        assert.is_not_nil(M._state(7).validated[1])

        fire("LspDetach", 1, { client_id = 7 })
        assert.is_nil(M._state(7).validated[1])
        assert.are.equal(1, #deleted_groups)

        fire("BufWipeout", 2)
        assert.is_nil(M._state(7))
        assert.is_true(timer.closed)
        assert.are.equal(2, #deleted_groups)
    end)
end)
