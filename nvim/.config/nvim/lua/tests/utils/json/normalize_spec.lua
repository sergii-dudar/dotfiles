local helper = require("tests.utils.spec_helper")

describe("utils.json.normalize", function()
    local normalize
    local state

    before_each(function()
        _, state = helper.reset_vim()
        normalize = helper.reload("utils.json.normalize")
    end)

    it("unwraps stringified JSON object content and formats the whole buffer with jq", function()
        -- given
        state.buffer_lines[0] = { '"{\\"name\\":\\"Ada\\",\\"age\\":42}"' }
        vim.json.decode = function(content)
            if content == '"{\\"name\\":\\"Ada\\",\\"age\\":42}"' then
                return '{"name":"Ada","age":42}'
            end
            if content == '{"name":"Ada","age":42}' then
                return { name = "Ada", age = 42 }
            end
            error("invalid json")
        end
        vim.fn.executable = function(command)
            return command == "jq" and 1 or 0
        end
        vim.fn.system = function(command, input)
            assert.are.same({ "jq", "." }, command)
            assert.are.equal('{"name":"Ada","age":42}', input)
            vim.v.shell_error = 0
            return table.concat({
                "{",
                '  "age": 42,',
                '  "name": "Ada"',
                "}",
            }, "\n")
        end

        -- when
        normalize.normalize_buffer()

        -- then
        assert.are.same({
            "{",
            '  "age": 42,',
            '  "name": "Ada"',
            "}",
        }, state.buffer_lines[0])
    end)

    it("trims and keeps plain JSON content when jq is unavailable", function()
        -- given
        state.buffer_lines[0] = { '  {"ready":true}  ' }
        vim.json.decode = function(content)
            if content == '{"ready":true}' then
                return { ready = true }
            end
            error("invalid json")
        end
        vim.fn.executable = function()
            return 0
        end

        -- when
        normalize.normalize_buffer()

        -- then
        assert.are.same({ '{"ready":true}' }, state.buffer_lines[0])
    end)

    it("does not unwrap a top-level JSON string unless it contains an object or array", function()
        -- given
        state.buffer_lines[0] = { '"plain text"' }
        vim.json.decode = function(content)
            if content == '"plain text"' then
                return "plain text"
            end
            error("invalid json")
        end
        vim.fn.executable = function()
            return 0
        end

        -- when
        normalize.normalize_buffer()

        -- then
        assert.are.same({ '"plain text"' }, state.buffer_lines[0])
    end)

    it("unwraps stringified JSON array content in a visual selection and replaces only the selection", function()
        -- given
        state.positions["'<"] = { 0, 1, 1, 0 }
        state.positions["'>"] = { 0, 1, 2147483647, 0 }
        state.buffer_lines[0] = { '"[1,2]"' }
        vim.json.decode = function(content)
            if content == '"[1,2]"' then
                return "[1,2]"
            end
            if content == "[1,2]" then
                return { 1, 2 }
            end
            error("invalid json")
        end
        vim.fn.executable = function(command)
            return command == "jq" and 1 or 0
        end
        vim.fn.system = function(command, input)
            assert.are.same({ "jq", "." }, command)
            assert.are.equal("[1,2]", input)
            vim.v.shell_error = 0
            return table.concat({
                "[",
                "  1,",
                "  2",
                "]",
            }, "\n")
        end

        -- when
        normalize.normalize_selection()

        -- then
        assert.are.same({
            bufnr = 0,
            start_row = 0,
            start_col = 0,
            end_row = 0,
            end_col = 7,
            lines = {
                "[",
                "  1,",
                "  2",
                "]",
            },
        }, state.set_text)
    end)

    it("keeps selected content unchanged when jq fails to format it", function()
        -- given
        state.positions["'<"] = { 0, 1, 1, 0 }
        state.positions["'>"] = { 0, 1, 7, 0 }
        state.buffer_lines[0] = { "{oops}" }
        vim.json.decode = function()
            error("invalid json")
        end
        vim.fn.executable = function(command)
            return command == "jq" and 1 or 0
        end
        vim.fn.system = function()
            vim.v.shell_error = 1
            return ""
        end

        -- when
        normalize.normalize_selection()

        -- then
        assert.are.same({
            bufnr = 0,
            start_row = 0,
            start_col = 0,
            end_row = 0,
            end_col = 7,
            lines = { "{oops}" },
        }, state.set_text)
    end)
end)
