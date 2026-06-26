local helper = require("tests.utils.spec_helper")

describe("utils.java.javap-util", function()
    local javap_util
    local cache_util
    local state

    before_each(function()
        _, state = helper.reset_vim()
        cache_util = helper.reload("utils.cache-util")
        cache_util.java.javap_results_map = {}
        javap_util = helper.reload("utils.java.javap-util")
    end)

    it("keeps method names without parameter descriptors unchanged", function()
        -- given
        local qualified_name = "ua.example.FooTest#sample"

        -- when
        local resolved = javap_util.resolve_parametrized_method_signature(qualified_name, "/classes")

        -- then
        assert.are.equal(qualified_name, resolved)
        assert.are.same({}, state.system_calls)
    end)

    it("resolves a parametrized method descriptor from javap output", function()
        -- given
        local calls = {}
        vim.system = function(command, opts)
            table.insert(calls, { command = command, opts = opts })
            return {
                wait = function()
                    return {
                        stdout = table.concat({
                            'Compiled from "FooTest.java"',
                            "class ua.example.FooTest {",
                            "  void sample(java.lang.String, int);",
                            "  void noArgs();",
                            "}",
                        }, "\n"),
                    }
                end,
            }
        end

        -- when
        local resolved =
            javap_util.resolve_parametrized_method_signature("ua.example.FooTest#sample(java.lang.String)", "/classes")

        -- then
        assert.are.equal("ua.example.FooTest#sample(java.lang.String, int)", resolved)
        assert.are.same({ "javap", "-cp", "/classes", "ua.example.FooTest" }, calls[1].command)
    end)

    it("uses cached javap output for repeated resolutions", function()
        -- given
        local call_count = 0
        vim.system = function()
            call_count = call_count + 1
            return {
                wait = function()
                    return { stdout = "class ua.example.FooTest {\n  void sample(java.lang.String);\n}" }
                end,
            }
        end

        -- when
        local first =
            javap_util.resolve_parametrized_method_signature("ua.example.FooTest#sample(java.lang.String)", "/classes")
        local second =
            javap_util.resolve_parametrized_method_signature("ua.example.FooTest#sample(java.lang.String)", "/classes")

        -- then
        assert.are.equal("ua.example.FooTest#sample(java.lang.String)", first)
        assert.are.equal("ua.example.FooTest#sample(java.lang.String)", second)
        assert.are.equal(1, call_count)
    end)

    it("falls back to the original descriptor when javap has no matching method", function()
        -- given
        local qualified_name = "ua.example.FooTest#missing(java.lang.String)"
        vim.system = function()
            return {
                wait = function()
                    return { stdout = "class ua.example.FooTest {\n  void other(java.lang.String);\n}" }
                end,
            }
        end

        -- when
        local resolved = javap_util.resolve_parametrized_method_signature(qualified_name, "/classes")

        -- then
        assert.are.equal(qualified_name, resolved)
        assert.matches("Default qualitied name will be used", state.notifications[1].message)
    end)
end)
