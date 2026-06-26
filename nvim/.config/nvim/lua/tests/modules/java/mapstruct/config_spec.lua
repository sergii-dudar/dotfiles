local helper = require("tests.utils.spec_helper")

describe("modules.java.mapstruct.config", function()
    local config

    before_each(function()
        helper.reset_vim()
        config = helper.reload("modules.java.mapstruct.config")
    end)

    after_each(function()
        helper.clear_stub_modules("modules.java.mapstruct.config")
    end)

    it("returns a copy of defaults", function()
        -- given
        local defaults = config.get_defaults()

        -- when
        defaults.java_cmd = "jbr"

        -- then
        assert.are.equal("java", config.defaults.java_cmd)
    end)

    it("merges caller options over defaults without mutating defaults", function()
        -- given
        local opts = {
            java_cmd = "jbr",
            classpath = { "/classes" },
            request_timeout_ms = 42,
        }

        -- when
        local merged = config.merge(opts)

        -- then
        assert.are.equal("jbr", merged.java_cmd)
        assert.are.same({ "/classes" }, merged.classpath)
        assert.are.equal(42, merged.request_timeout_ms)
        assert.are.equal("java", config.defaults.java_cmd)
        assert.is_nil(config.defaults.classpath)
    end)
end)
