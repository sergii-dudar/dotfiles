local helper = require("tests.utils.spec_helper")

describe("utils.java.java-format-checker", function()
    local checker
    local state
    local extmarks

    --- Build the minimal Tree-sitter node interface used by the checker.
    local function fake_node(node_type, text, range, children)
        return {
            type = function()
                return node_type
            end,
            range = function()
                return unpack(range)
            end,
            iter_children = function()
                local index = 0
                return function()
                    index = index + 1
                    return children and children[index] or nil
                end
            end,
            text = text,
        }
    end

    --- Install a deterministic Tree-sitter parse containing one method invocation.
    local function stub_tree(invocation)
        vim.treesitter = {
            get_node_text = function(node)
                return node.text
            end,
            get_parser = function()
                return {
                    parse = function()
                        return {
                            {
                                root = function()
                                    return {}
                                end,
                            },
                        }
                    end,
                }
            end,
            query = {
                parse = function()
                    return {
                        captures = { "_name", "call" },
                        iter_captures = function()
                            local yielded = false
                            return function()
                                if yielded then
                                    return nil
                                end
                                yielded = true
                                return 2, invocation
                            end
                        end,
                    }
                end,
            },
        }
    end

    before_each(function()
        _, state = helper.reset_vim()
        state.current_buf = 7
        state.loaded_buffers[7] = true
        extmarks = {}

        local next_namespace = 0
        vim.api.nvim_create_namespace = function()
            next_namespace = next_namespace + 1
            return next_namespace
        end
        vim.api.nvim_buf_clear_namespace = function() end
        vim.api.nvim_buf_set_extmark = function(bufnr, namespace, row, col, opts)
            local line = state.buffer_lines[bufnr] and state.buffer_lines[bufnr][row + 1]
            if not line or col > #line then
                error("Invalid 'col': out of range")
            end
            extmarks[#extmarks + 1] = {
                bufnr = bufnr,
                namespace = namespace,
                row = row,
                col = col,
                opts = opts,
            }
        end
    end)

    after_each(function()
        helper.clear_stub_modules("utils.java.java-format-checker")
    end)

    it("maps placeholders in parser string_literal text blocks to their source line", function()
        state.buffer_lines[7] = {
            '        String responseWithUnknownFields = """',
            "                {",
            '                  "token": "%s",',
            "                }",
            '                """.formatted(CARD_ID);',
        }

        local format_string = fake_node(
            "string_literal",
            table.concat({
                '"""',
                "                {",
                '                  "token": "%s",',
                "                }",
                '                """',
            }, "\n"),
            { 0, 43, 4, 19 }
        )
        local arguments = fake_node("argument_list", "(CARD_ID)", { 4, 29, 4, 38 }, {
            fake_node("(", "(", { 4, 29, 4, 30 }),
            fake_node("identifier", "CARD_ID", { 4, 30, 4, 37 }),
            fake_node(")", ")", { 4, 37, 4, 38 }),
        })
        local invocation = fake_node("method_invocation", nil, { 0, 43, 4, 38 }, {
            format_string,
            fake_node(".", ".", { 4, 19, 4, 20 }),
            fake_node("identifier", "formatted", { 4, 20, 4, 29 }),
            arguments,
        })
        stub_tree(invocation)
        checker = helper.reload("utils.java.java-format-checker")

        assert.has_no.errors(function()
            checker.apply(7)
        end)
        assert.are.equal(1, #extmarks)
        assert.are.equal(2, extmarks[1].row)
        assert.are.equal(28, extmarks[1].col)
        assert.are.equal(30, extmarks[1].opts.end_col)
        assert.are.equal("JavaFormatOk", extmarks[1].opts.hl_group)
    end)
end)
