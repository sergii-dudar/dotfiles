local helper = require("tests.utils.spec_helper")

describe("modules.common.test-report.registry", function()
    local registry

    before_each(function()
        helper.reset_vim()
        registry = helper.reload("modules.common.test-report.registry")
    end)

    after_each(function()
        helper.clear_stub_modules("modules.common.test-report.registry")
    end)

    it("registers and resolves an adapter by filetype", function()
        -- given
        local adapter = { diagnostic_source = "junit" }

        -- when
        registry.register("java", adapter)

        -- then
        assert.is_true(registry.has("java"))
        assert.are.same(adapter, registry.get("java"))
    end)

    it("returns nil and false for an unknown filetype", function()
        -- given
        local filetype = "kotlin"

        -- when
        local adapter = registry.get(filetype)

        -- then
        assert.is_false(registry.has(filetype))
        assert.is_nil(adapter)
    end)

    it("returns the registered adapter map", function()
        -- given
        local java_adapter = { group_separator = "." }
        local lua_adapter = { group_separator = "::" }

        -- when
        registry.register("java", java_adapter)
        registry.register("lua", lua_adapter)

        -- then
        assert.are.same({
            java = java_adapter,
            lua = lua_adapter,
        }, registry.all())
    end)
end)
