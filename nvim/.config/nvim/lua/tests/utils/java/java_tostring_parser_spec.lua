local helper = require("tests.utils.spec_helper")

describe("utils.java.java-tostring-parser", function()
    local parser
    local state

    before_each(function()
        _, state = helper.reset_vim()
        parser = helper.reload("utils.java.java-tostring-parser")
    end)

    it("returns a clear error for empty input", function()
        -- given
        local input = ""

        -- when
        local result, err = parser.parse(input)

        -- then
        assert.is_nil(result)
        assert.are.equal("empty input", err)
    end)

    it("parses Lombok toString output with nested objects, arrays, booleans, nulls, and raw numbers", function()
        -- given
        local input = table.concat({
            "Payment(",
            "id=123, ",
            "amount=100.00, ",
            "active=true, ",
            "debtor=Person(name=Ada), ",
            "tags=[urgent, reviewed], ",
            "missing=null",
            ")",
        })

        -- when
        local result, err = parser.parse(input)

        -- then
        assert.is_nil(err)
        assert.are.equal("%%__RAWNUM__%%123", result.id)
        assert.are.equal("%%__RAWNUM__%%100.00", result.amount)
        assert.is_true(result.active)
        assert.are.same({ name = "Ada" }, result.debtor)
        assert.are.same({ "urgent", "reviewed" }, result.tags)
        assert.are.equal(vim.NIL, result.missing)
    end)

    it("parses bare Lombok object output without a class name", function()
        -- given
        local input = "(name=Ada, enabled=false)"

        -- when
        local result, err = parser.parse(input)

        -- then
        assert.is_nil(err)
        assert.are.same({
            name = "Ada",
            enabled = false,
        }, result)
    end)

    it("parses Swagger toString output with escaped newlines and nested class objects", function()
        -- given
        local input = table.concat({
            "class Payment {\\n",
            "    id: 42\\n",
            "    active: false\\n",
            "    debtor: class Person {\\n",
            "        name: Ada\\n",
            "    }\\n",
            "    flags: [true, false, null]\\n",
            "}",
        })

        -- when
        local result, err = parser.parse(input)

        -- then
        assert.is_nil(err)
        assert.are.equal("%%__RAWNUM__%%42", result.id)
        assert.is_false(result.active)
        assert.are.same({ name = "Ada" }, result.debtor)
        assert.are.same({ true, false, vim.NIL }, result.flags)
    end)

    it("parses auto-detected arrays with mixed Lombok, Swagger, and nested array values", function()
        -- given
        local input = table.concat({
            "[",
            "Payment(id=1), ",
            "class User {\n",
            "    name: Bob\n",
            "}, ",
            "[true, null]",
            "]",
        })

        -- when
        local result, err = parser.parse(input)

        -- then
        assert.is_nil(err)
        assert.are.equal("%%__RAWNUM__%%1", result[1].id)
        assert.are.same({ name = "Bob" }, result[2])
        assert.are.same({ true, vim.NIL }, result[3])
    end)

    it("converts parsed tables to JSON while preserving raw numeric precision", function()
        -- given
        local result = {
            amount = "%%__RAWNUM__%%100.00",
            count = "%%__RAWNUM__%%42",
            missing = vim.NIL,
            name = "Ada",
        }

        -- when
        local json = parser.to_json(result)

        -- then
        assert.are.equal('{"amount":100.00,"count":42,"missing":null,"name":"Ada"}', json)
    end)

    it("replaces the current visual selection with formatted JSON", function()
        -- given
        state.positions["'<"] = { 0, 1, 1, 0 }
        state.positions["'>"] = { 0, 1, 26, 0 }
        state.buffer_lines[0] = { "Payment(id=1, active=true)" }
        vim.fn.system = function(command, input)
            assert.matches("jq %.", command)
            assert.are.equal("", input)
            vim.v.shell_error = 0
            return table.concat({
                "{",
                '  "active": true,',
                '  "id": 1',
                "}",
            }, "\n")
        end

        -- when
        parser.convert_selection(false)

        -- then
        assert.are.same({
            bufnr = 0,
            start_row = 0,
            start_col = 0,
            end_row = 0,
            end_col = 26,
            lines = {
                "{",
                '  "active": true,',
                '  "id": 1',
                "}",
            },
        }, state.set_text)
    end)

    it("copies converted JSON to the clipboard when requested", function()
        -- given
        state.positions["'<"] = { 0, 1, 1, 0 }
        state.positions["'>"] = { 0, 1, 26, 0 }
        state.buffer_lines[0] = { "Payment(id=1, active=true)" }
        vim.fn.system = function()
            vim.v.shell_error = 1
            return ""
        end

        -- when
        parser.convert_selection(true)

        -- then
        assert.are.equal('{"active":true,"id":1}', state.registers["+"])
        assert.are.same({ message = "JSON copied to clipboard", level = vim.log.levels.INFO }, state.notifications[1])
        assert.is_nil(state.set_text)
    end)

    it("notifies a parse error instead of replacing text when the selected input is empty", function()
        -- given
        state.positions["'<"] = { 0, 1, 1, 0 }
        state.positions["'>"] = { 0, 1, 1, 0 }
        state.buffer_lines[0] = { "" }

        -- when
        parser.convert_selection(false)

        -- then
        assert.are.same(
            { message = "Failed to parse Java toString: empty input", level = vim.log.levels.ERROR },
            state.notifications[1]
        )
        assert.is_nil(state.set_text)
    end)
end)
