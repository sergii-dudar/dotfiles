local helper = require("tests.utils.spec_helper")

describe("utils.java.jdtls-util", function()
    local jdtls_util

    before_each(function()
        helper.reset_vim()
        helper.stub_module(
            "nio",
            { lsp = {
                get_clients = function()
                    return {}
                end,
            } }
        )
        helper.stub_module("utils.java.java-common", {
            abbreviated_package_matches = function()
                return false
            end,
            build_abbreviated_query = function(class_name)
                return class_name
            end,
            has_abbreviated_segments = function()
                return false
            end,
        })
        jdtls_util = helper.reload("utils.java.jdtls-util")
    end)

    after_each(function()
        helper.clear_stub_modules({ "nio", "utils.java.java-common", "utils.java.jdtls-util" })
    end)

    it("decodes an encoded jdt class link into class name and line", function()
        -- given
        local encoded = "/%3Cjava.lang%28Object.class#268"

        -- when
        local parsed = jdtls_util.parse_jdt_link(encoded)

        -- then
        assert.are.same({ class_name = "java.lang.Object", line_number = 268 }, parsed)
    end)

    it("returns nil values when a jdt link is missing", function()
        -- given
        local encoded = nil

        -- when
        local class_name, line_number = jdtls_util.parse_jdt_link(encoded)

        -- then
        assert.is_nil(class_name)
        assert.is_nil(line_number)
    end)

    it("adds a missing closing parenthesis to hover jdt links", function()
        -- given
        local hover = "Open [Object](jdt://contents/java.base/java.lang/Object.class)"

        -- when
        local fixed = jdtls_util.fix_hover_links(hover)

        -- then
        assert.are.equal("Open [Object](jdt://contents/java.base/java.lang/Object.class))", fixed)
    end)

    it("fixes hover links inside markup content and marked-string arrays", function()
        -- given
        local markup = { kind = "markdown", value = "[A](jdt://a/A.class)" }
        local marked_strings = {
            "plain [B](jdt://b/B.class)",
            { language = "java", value = "[C](jdt://c/C.class)" },
        }

        -- when
        local fixed_markup = jdtls_util.fix_hover_contents(markup)
        local fixed_marked_strings = jdtls_util.fix_hover_contents(marked_strings)

        -- then
        assert.are.equal("[A](jdt://a/A.class))", fixed_markup.value)
        assert.are.equal("plain [B](jdt://b/B.class))", fixed_marked_strings[1])
        assert.are.equal("[C](jdt://c/C.class))", fixed_marked_strings[2].value)
    end)
end)
