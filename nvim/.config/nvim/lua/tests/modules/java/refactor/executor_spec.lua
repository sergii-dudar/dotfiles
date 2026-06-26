local helper = require("tests.utils.spec_helper")

describe("modules.java.refactor.executor", function()
    local executor
    local original_os_execute

    --- Return a no-op logger for refactor executor tests.
    local function logger()
        return {
            debug = function() end,
            error = function() end,
            info = function() end,
            warn = function() end,
        }
    end

    before_each(function()
        helper.reset_vim()
        original_os_execute = os.execute

        helper.stub_module("utils.logging-util", {
            new = logger,
        })
        helper.stub_module("utils.common-util", {
            close_window_if_exists = function() end,
        })
        helper.stub_module("utils.ui.spinner", {
            start = function() end,
            stop = function() end,
        })
        helper.reload("modules.java.refactor.constants")
        executor = helper.reload("modules.java.refactor.executor")
    end)

    after_each(function()
        os.execute = original_os_execute
        helper.clear_stub_modules({
            "modules.java.refactor.executor",
            "modules.java.refactor.constants",
            "utils.common-util",
            "utils.logging-util",
            "utils.ui.spinner",
        })
    end)

    it("builds a shell chain that preserves failure state across commands", function()
        -- given
        local shell_cmds = { "echo one", "echo two" }

        -- when
        local chain = executor.build_shell_chain(shell_cmds)

        -- then
        assert.are.equal("failed=0 ; ( echo one ) || failed=1 ; ( echo two ) || failed=1 ; exit $failed", chain)
    end)

    it("separates shell commands from Lua operations while preserving order within each group", function()
        -- given
        local lua_op = { type = "lua", description = "refresh buffers", fn = function() end }
        local operations = {
            { type = "shell", command = "sed -i one" },
            lua_op,
            { type = "shell", command = "sed -i two" },
        }

        -- when
        local shell_cmds, lua_operations = executor.separate_operations(operations)

        -- then
        assert.are.same({ "sed -i one", "sed -i two" }, shell_cmds)
        assert.are.same({ lua_op }, lua_operations)
    end)

    it("executes test-mode shell commands and Lua operations successfully", function()
        -- given
        local executed = {}
        local lua_called = false
        os.execute = function(cmd)
            table.insert(executed, cmd)
            return 0
        end
        local lua_operations = {
            {
                description = "refresh buffers",
                fn = function()
                    lua_called = true
                    return true
                end,
            },
        }

        -- when
        local success = executor.execute_test_mode({ "cmd-one", "cmd-two" }, lua_operations)

        -- then
        assert.is_true(success)
        assert.are.same({ "cmd-one", "cmd-two" }, executed)
        assert.is_true(lua_called)
    end)

    it("reports failed test-mode shell commands without running Lua operations", function()
        -- given
        local lua_called = false
        os.execute = function()
            return 1
        end
        local lua_operations = {
            {
                description = "refresh buffers",
                fn = function()
                    lua_called = true
                    return true
                end,
            },
        }

        -- when
        local success = executor.execute_test_mode({ "cmd-one" }, lua_operations)

        -- then
        assert.is_false(success)
        assert.is_false(lua_called)
    end)
end)
