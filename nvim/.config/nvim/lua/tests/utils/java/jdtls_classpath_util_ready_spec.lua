local helper = require("tests.utils.spec_helper")

describe("utils.java.jdtls-classpath-util is_jdtls_ready", function()
    local util
    local client, rpc_calls, projects

    local function noop() end

    --- Build a fake jdtls client whose project query counts calls and returns `projects`.
    ---@param id integer
    ---@return table
    local function make_client(id)
        return {
            id = id,
            initialized = true,
            request_sync = function()
                rpc_calls = rpc_calls + 1
                return { result = projects }, nil
            end,
        }
    end

    before_each(function()
        helper.reset_vim()
        helper.stub_module("utils.logging-util", {
            new = function()
                return { debug = noop, info = noop, warn = noop, error = noop, set_level = noop }
            end,
        })
        rpc_calls = 0
        projects = { "file:///project" }
        client = make_client(7)
        helper.stub_module("utils.lsp-util", {
            get_client_by_name = function()
                return client
            end,
        })
        util = helper.reload("utils.java.jdtls-classpath-util")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.logging-util", "utils.lsp-util", "utils.java.jdtls-classpath-util" })
    end)

    it("asks jdtls once per client and keeps the positive answer", function()
        assert.is_true(util.is_jdtls_ready(1))
        assert.is_true(util.is_jdtls_ready(1))
        assert.is_true(util.is_jdtls_ready(1))

        assert.are.equal(1, rpc_calls)
    end)

    it("re-checks when a different jdtls client is attached", function()
        assert.is_true(util.is_jdtls_ready(1))

        client = make_client(8)

        assert.is_true(util.is_jdtls_ready(1))
        assert.are.equal(2, rpc_calls)
    end)

    it("re-checks after clear_cache", function()
        assert.is_true(util.is_jdtls_ready(1))

        util.clear_cache()

        assert.is_true(util.is_jdtls_ready(1))
        assert.are.equal(2, rpc_calls)
    end)

    it("does not cache a negative answer", function()
        projects = {}
        assert.is_false(util.is_jdtls_ready(1))

        projects = { "file:///project" }
        assert.is_true(util.is_jdtls_ready(1))
        assert.are.equal(2, rpc_calls)
    end)

    it("reports not ready when no client is attached", function()
        client = nil

        assert.is_false(util.is_jdtls_ready(1))
        assert.are.equal(0, rpc_calls)
    end)
end)
