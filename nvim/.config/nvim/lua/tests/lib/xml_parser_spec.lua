describe("lib.xml", function()
    local xml = require("lib.xml")

    it("keeps attribute values that contain a raw '>' (JUnit Jupiter failure messages)", function()
        -- given: the tag string is cut at the first '>' and must be extended until the quote closes
        local src = table.concat({
            '<testsuite name="JUnit Jupiter"><testcase name="t()" classname="a.B" time="0.1">',
            '<failure message="expected: &lt;2> but was: &lt;1>" type="org.opentest4j.AssertionFailedError">',
            "<![CDATA[org.opentest4j.AssertionFailedError: expected: <2> but was: <1>",
            "\tat a.B.t(B.java:12)",
            "]]></failure></testcase></testsuite>",
        }, "\n")

        -- when
        local parsed = xml.parse(src)

        -- then
        local failure = parsed.testsuite.testcase.failure
        assert.are.equal("expected: <2> but was: <1>", failure._attr.message)
        assert.are.equal("org.opentest4j.AssertionFailedError", failure._attr.type)
        assert.are.equal("string", type(failure[1]))
        assert.is_truthy(failure[1]:find("at a.B.t(B.java:12)", 1, true))
    end)

    it("parses ordinary attributes unchanged", function()
        -- when
        local parsed = xml.parse('<a x="1" y=\'it"s\'><b/></a>')

        -- then
        assert.are.equal("1", parsed.a._attr.x)
        assert.are.equal('it"s', parsed.a._attr.y)
    end)
end)
