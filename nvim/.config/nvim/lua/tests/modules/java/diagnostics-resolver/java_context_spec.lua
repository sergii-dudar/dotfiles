local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.java-context", function()
    local java_context
    local state

    before_each(function()
        _, state = helper.reset_vim()
        vim.api.nvim_buf_get_lines = function(bufnr, start_row, end_row)
            local lines = state.buffer_lines[bufnr] or {}
            if end_row == -1 then
                end_row = #lines
            end

            local result = {}
            for index = start_row + 1, end_row do
                result[#result + 1] = lines[index]
            end
            return result
        end
        java_context = helper.reload("modules.java.diagnostics-resolver.java-context")
    end)

    after_each(function()
        helper.clear_stub_modules({ "modules.java.diagnostics-resolver.java-context" })
    end)

    it("inserts a generated member between the current and following methods", function()
        -- given
        state.buffer_lines[1] = {
            "    void current();",
            "    void following();",
        }
        local method = {
            range = function()
                return 0, 4, 0, 19
            end,
        }

        -- when
        local insert_row = java_context.insert_after_method(1, method, { "    void generated();" })

        -- then
        assert.are.equal(1, insert_row)
        assert.are.same({
            "    void current();",
            "",
            "    void generated();",
            "",
            "    void following();",
        }, state.buffer_lines[1])
    end)

    it("does not add a trailing blank before the enclosing type closes", function()
        -- given
        state.buffer_lines[1] = {
            "    void current();",
            "}",
        }
        local method = {
            range = function()
                return 0, 4, 1, 0
            end,
        }

        -- when
        java_context.insert_after_method(1, method, { "    void generated();" })

        -- then
        assert.are.same({
            "    void current();",
            "",
            "    void generated();",
            "}",
        }, state.buffer_lines[1])
    end)
end)
