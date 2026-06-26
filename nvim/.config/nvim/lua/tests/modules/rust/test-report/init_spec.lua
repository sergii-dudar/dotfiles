local helper = require("tests.utils.spec_helper")

describe("modules.rust.test-report", function()
    before_each(function()
        helper.reset_vim()
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.rust.test-report",
            "modules.rust.test-report.lang.rust",
            "modules.common.test-report",
            "modules.common.test-report.registry",
        })
    end)

    it("registers the Rust adapter and returns the common test-report core", function()
        -- given
        local registrations = {}
        local rust_adapter = { diagnostic_source = "cargo-test" }
        local core = { process = function() end }

        helper.stub_module("modules.rust.test-report.lang.rust", rust_adapter)
        helper.stub_module("modules.common.test-report", core)
        helper.stub_module("modules.common.test-report.registry", {
            register = function(filetype, adapter)
                registrations[filetype] = adapter
            end,
        })

        -- when
        local returned = helper.reload("modules.rust.test-report")

        -- then
        assert.are.same(core, returned)
        assert.are.same(rust_adapter, registrations.rust)
    end)
end)
