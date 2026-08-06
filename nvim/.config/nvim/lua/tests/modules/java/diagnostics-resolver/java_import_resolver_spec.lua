local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.java-import-resolver", function()
    local resolver
    local state

    before_each(function()
        _, state = helper.reset_vim()
        resolver = helper.reload("modules.java.diagnostics-resolver.java-import-resolver")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver.java-import-resolver",
        })
    end)

    it("normalizes and validates nested backend class names", function()
        -- given
        local resolved = {
            className = "example.ChargeCalculationRequest$ChargeAccount",
            packageName = "example",
        }

        -- when
        local descriptor = assert(resolver.describe(resolved))

        -- then
        assert.are.equal("example.ChargeCalculationRequest.ChargeAccount", descriptor.canonical_name)
        assert.are.equal("ChargeCalculationRequest.ChargeAccount", descriptor.class_reference)
        assert.are.equal("example.ChargeCalculationRequest", descriptor.import_name)
        assert.is_true(resolver.matches(resolved, "ChargeCalculationRequest.ChargeAccount"))
        assert.is_false(resolver.matches(resolved, "Account"))
    end)

    it("plans and inserts a java import without changing other import groups", function()
        -- given
        state.buffer_lines[1] = {
            "package example;",
            "",
            "import example.Source;",
            "import example.Target;",
            "",
            "public class Mapper {}",
        }

        -- when
        local references, imports = resolver.plan(1, {
            {
                key = "source",
                type = { className = "java.time.Duration", packageName = "java.time" },
            },
            {
                key = "target",
                type = { className = "long", packageName = "" },
            },
        })
        local inserted = resolver.apply(1, imports)

        -- then
        assert.are.same({ source = "Duration", target = "long" }, references)
        assert.are.same({ "java.time.Duration" }, imports)
        assert.are.equal(2, inserted)
        assert.are.same({
            "package example;",
            "",
            "import example.Source;",
            "import example.Target;",
            "",
            "import java.time.Duration;",
            "",
            "public class Mapper {}",
        }, state.buffer_lines[1])
    end)

    it("uses a qualified type when its simple import name is occupied", function()
        -- given
        state.buffer_lines[1] = {
            "package example;",
            "",
            "import other.Account;",
            "",
            "public class Mapper {}",
        }

        -- when
        local references, imports = resolver.plan(1, {
            {
                key = "target",
                type = { className = "api.Account", packageName = "api" },
            },
        })

        -- then
        assert.are.same({ target = "api.Account" }, references)
        assert.are.same({}, imports)
    end)
end)
