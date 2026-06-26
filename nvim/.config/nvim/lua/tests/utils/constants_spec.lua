local helper = require("tests.utils.spec_helper")

describe("utils.constants", function()
    before_each(function()
        helper.reset_vim()
    end)

    it("exposes stable language diagnostic source names", function()
        -- given
        local constants = helper.reload("utils.constants")

        -- when
        local java_source = constants.java.maven_diagnostics_test_source
        local lua_source = constants.lua.busted_test_diagnostics_source
        local bash_source = constants.bash.bashunit_test_diagnostics_source

        -- then
        assert.are.equal("maven-test", java_source)
        assert.are.equal("busted_test_diagnostics", lua_source)
        assert.are.equal("bashunit_test_diagnostics", bash_source)
    end)

    it("derives platform-specific bashunit binary from vim.loop.os_uname", function()
        -- given
        vim.loop.os_uname = function()
            return { sysname = "Linux" }
        end

        -- when
        local constants = helper.reload("utils.constants")

        -- then
        assert.is_false(constants.is_macos)
        assert.are.equal("/home/serhii/.local/bin/bashunit", constants.bash.bashunit_bin)
    end)

    it("keeps shared output dimensions available for split helpers", function()
        -- given
        local constants = helper.reload("utils.constants")

        -- when
        local height = constants.output.height_rows

        -- then
        assert.are.equal(10, height)
    end)
end)
