local helper = require("tests.utils.spec_helper")

describe("modules.go.test-report", function()
    before_each(function()
        helper.reset_vim()
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.go.test-report",
            "modules.go.test-report.lang.go",
            "modules.common.test-report",
            "modules.common.test-report.registry",
        })
    end)

    it("registers the Go adapter and returns the common test-report core", function()
        -- given
        local registrations = {}
        local go_adapter = { diagnostic_source = "go-test" }
        local core = { process = function() end }

        helper.stub_module("modules.go.test-report.lang.go", go_adapter)
        helper.stub_module("modules.common.test-report", core)
        helper.stub_module("modules.common.test-report.registry", {
            register = function(filetype, adapter)
                registrations[filetype] = adapter
            end,
        })

        -- when
        local returned = helper.reload("modules.go.test-report")

        -- then
        assert.are.same(core, returned)
        assert.are.same(go_adapter, registrations.go)
    end)
end)
