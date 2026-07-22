local helper = require("tests.utils.spec_helper")

describe("utils.java.jdtls-adaptive-formatter", function()
    local formatter
    local state
    local client
    local requests

    --- Build the minimal Tree-sitter node interface used by the formatter.
    local function fake_node(node_type, range, first_child, parent)
        return {
            type = function()
                return node_type
            end,
            range = function()
                return unpack(range)
            end,
            named_child = function(_, index)
                return index == 0 and first_child or nil
            end,
            parent = function()
                return parent
            end,
        }
    end

    --- Build a string-literal node for concatenation tests.
    local function fake_literal(range)
        return {
            type = function()
                return "string_literal"
            end,
            range = function()
                return unpack(range)
            end,
            named_child = function()
                return nil
            end,
            parent = function()
                return nil
            end,
            field = function()
                return {}
            end,
        }
    end

    --- Build a non-string operand node (e.g. an identifier) for concatenation tests.
    local function fake_operand(node_type, range)
        local node = fake_literal(range)
        node.type = function()
            return node_type
        end
        return node
    end

    --- Build a `+` binary-expression node with the fields the formatter reads.
    local function fake_binary(range, operator_text, operands, parent)
        return {
            type = function()
                return "binary_expression"
            end,
            range = function()
                return unpack(range)
            end,
            named_child = function()
                return nil
            end,
            parent = function()
                return parent
            end,
            field = function(_, name)
                if name == "operator" then
                    return { { __text = operator_text } }
                end
                if name == "left" then
                    return operands.left and { operands.left } or {}
                end
                if name == "right" then
                    return operands.right and { operands.right } or {}
                end
                return {}
            end,
        }
    end

    --- Install a deterministic Java parser and query result for a test.
    local function stub_tree(parameters)
        vim.treesitter = {
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
                        iter_captures = function()
                            local index = 0
                            return function()
                                index = index + 1
                                if parameters[index] then
                                    return 1, parameters[index]
                                end
                            end
                        end,
                    }
                end,
            },
            get_node_text = function(node)
                return node and node.__text
            end,
        }
    end

    before_each(function()
        _, state = helper.reset_vim()
        state.current_buf = 7
        requests = {}
        vim.bo[7].expandtab = true

        vim.api.nvim_get_mode = function()
            return { mode = "n" }
        end
        vim.fn.getpos = function()
            return { 0, 1, 1, 0 }
        end
        vim.lsp.util.character_offset = function(_, _, character)
            return character
        end
        vim.lsp.util.get_effective_tabstop = function()
            return 4
        end
        vim.lsp.util.make_formatting_params = function(options)
            return { options = options }
        end
        vim.lsp.util.make_text_document_params = function(bufnr)
            return { uri = "buffer://" .. bufnr }
        end
        vim.lsp.util.apply_text_edits = function(edits, bufnr, encoding)
            state.applied_edits = state.applied_edits or {}
            state.applied_edits[#state.applied_edits + 1] = {
                edits = edits,
                bufnr = bufnr,
                encoding = encoding,
            }
        end

        client = {
            name = "jdtls",
            offset_encoding = "utf-16",
            supports_method = function(_, method)
                return method == "textDocument/rangeFormatting"
            end,
            request_sync = function(_, method, params, timeout, bufnr)
                requests[#requests + 1] = {
                    method = method,
                    params = params,
                    timeout = timeout,
                    bufnr = bufnr,
                }
                if method == "textDocument/formatting" then
                    return {
                        result = {
                            {
                                range = { start = { line = 4, character = 0 }, ["end"] = { line = 4, character = 1 } },
                                newText = "base-kept",
                            },
                            {
                                range = {
                                    start = { line = 11, character = 0 },
                                    ["end"] = { line = 11, character = 1 },
                                },
                                newText = "base-method-overlap",
                            },
                            {
                                range = {
                                    start = { line = 21, character = 0 },
                                    ["end"] = { line = 21, character = 1 },
                                },
                                newText = "base-constructor-overlap",
                            },
                        },
                    }
                end
                return {
                    result = {
                        {
                            range = params.range,
                            newText = "adaptive-" .. params.range.start.line,
                        },
                    },
                }
            end,
        }
        vim.lsp.get_clients = function()
            return { client }
        end

        formatter = helper.reload("utils.java.jdtls-adaptive-formatter")
    end)

    after_each(function()
        helper.clear_stub_modules("utils.java.jdtls-adaptive-formatter")
        _G.LazyVim = nil
    end)

    it("registers ahead of the generic Conform formatter", function()
        local registered
        _G.LazyVim = {
            format = {
                register = function(candidate)
                    registered = candidate
                end,
            },
        }

        formatter.setup()

        assert.are.equal("JDTLS adaptive parameters", registered.name)
        assert.are.equal(110, registered.priority)
        assert.is_true(registered.primary)
        assert.are.same({ "jdtls" }, registered.sources(7))
    end)

    it("merges default edits with continuation-indented declaration ranges", function()
        local method_parent = fake_node("method_declaration", { 0, 0, 0, 0 })
        local constructor_parent = fake_node("constructor_declaration", { 0, 0, 0, 0 })
        local same_line_first = fake_node("formal_parameter", { 3, 20, 3, 30 })
        local next_line_first = fake_node("formal_parameter", { 11, 12, 11, 20 })
        local constructor_first = fake_node("formal_parameter", { 21, 12, 21, 20 })
        stub_tree({
            fake_node("formal_parameters", { 3, 19, 5, 40 }, same_line_first, method_parent),
            fake_node("formal_parameters", { 10, 80, 15, 45 }, next_line_first, method_parent),
            fake_node("formal_parameters", { 20, 24, 23, 30 }, constructor_first, constructor_parent),
        })

        formatter.format(0)

        assert.are.equal(3, #requests)
        assert.are.equal(7, requests[1].bufnr)
        assert.are.equal("textDocument/formatting", requests[1].method)
        assert.are.equal("textDocument/rangeFormatting", requests[2].method)
        assert.are.equal(10, requests[2].params.range.start.line)
        assert.are.equal(
            "16",
            requests[2].params.options["org.eclipse.jdt.core.formatter.alignment_for_parameters_in_method_declaration"]
        )
        assert.are.equal("textDocument/rangeFormatting", requests[3].method)
        assert.are.equal(20, requests[3].params.range.start.line)
        assert.are.equal(
            "16",
            requests[3].params.options["org.eclipse.jdt.core.formatter.alignment_for_parameters_in_constructor_declaration"]
        )
        assert.are.equal(1, #state.applied_edits)
        assert.are.equal(7, state.applied_edits[1].bufnr)
        assert.are.equal(3, #state.applied_edits[1].edits)
        assert.are.equal("base-kept", state.applied_edits[1].edits[1].newText)
        assert.are.equal("adaptive-10", state.applied_edits[1].edits[2].newText)
        assert.are.equal("adaptive-20", state.applied_edits[1].edits[3].newText)
    end)

    it("keeps adaptive range formatting inside a visual selection", function()
        vim.api.nvim_get_mode = function()
            return { mode = "V" }
        end
        vim.fn.getpos = function(mark)
            return mark == "v" and { 0, 9, 1, 0 } or { 0, 17, 1, 0 }
        end

        local method_parent = fake_node("method_declaration", { 0, 0, 0, 0 })
        stub_tree({
            fake_node(
                "formal_parameters",
                { 10, 40, 15, 20 },
                fake_node("formal_parameter", { 11, 12, 11, 20 }),
                method_parent
            ),
            fake_node(
                "formal_parameters",
                { 20, 40, 25, 20 },
                fake_node("formal_parameter", { 21, 12, 21, 20 }),
                method_parent
            ),
        })

        formatter.format(7)

        assert.are.equal(2, #requests)
        assert.are.equal("textDocument/rangeFormatting", requests[1].method)
        assert.are.equal(8, requests[1].params.range.start.line)
        assert.are.equal("textDocument/rangeFormatting", requests[2].method)
        assert.are.equal(10, requests[2].params.range.start.line)
    end)

    it("preserves a manually wrapped string concatenation", function()
        local declarator = fake_node("variable_declarator", { 3, 8, 6, 40 })
        local concatenation = fake_binary(
            { 3, 23, 6, 39 },
            "+",
            { left = fake_literal({ 3, 23, 3, 29 }), right = fake_literal({ 6, 26, 6, 39 }) },
            declarator
        )
        stub_tree({ concatenation })

        formatter.format(7)

        assert.are.equal(2, #requests)
        assert.are.equal("textDocument/formatting", requests[1].method)
        assert.are.equal("textDocument/rangeFormatting", requests[2].method)
        assert.are.equal(3, requests[2].params.range.start.line)
        assert.are.equal(6, requests[2].params.range["end"].line)
        assert.are.equal(
            "16",
            requests[2].params.options["org.eclipse.jdt.core.formatter.alignment_for_string_concatenation"]
        )
        assert.are.equal(
            "true",
            requests[2].params.options["org.eclipse.jdt.core.formatter.wrap_before_string_concatenation"]
        )
        assert.are.equal("false", requests[2].params.options["org.eclipse.jdt.core.formatter.join_wrapped_lines"])
    end)

    it("preserves a manually wrapped numeric additive expression", function()
        local declarator = fake_node("variable_declarator", { 3, 8, 5, 18 })
        local numeric = fake_binary({ 3, 16, 5, 17 }, "+", {
            left = fake_operand("decimal_integer_literal", { 3, 16, 3, 17 }),
            right = fake_operand("decimal_integer_literal", { 5, 16, 5, 17 }),
        }, declarator)
        stub_tree({ numeric })

        formatter.format(7)

        assert.are.equal(2, #requests)
        assert.are.equal("textDocument/formatting", requests[1].method)
        assert.are.equal("textDocument/rangeFormatting", requests[2].method)
        assert.are.equal(
            "16",
            requests[2].params.options["org.eclipse.jdt.core.formatter.alignment_for_additive_operator"]
        )
        assert.are.equal(
            "false",
            requests[2].params.options["org.eclipse.jdt.core.formatter.wrap_before_additive_operator"]
        )
        assert.are.equal("false", requests[2].params.options["org.eclipse.jdt.core.formatter.join_wrapped_lines"])
    end)

    it("preserves a manually wrapped boolean logical expression", function()
        local declarator = fake_node("variable_declarator", { 3, 8, 5, 25 })
        local logical = fake_binary({ 3, 20, 5, 24 }, "&&", {
            left = fake_operand("identifier", { 3, 20, 3, 21 }),
            right = fake_operand("identifier", { 5, 16, 5, 24 }),
        }, declarator)
        stub_tree({ logical })

        formatter.format(7)

        assert.are.equal(2, #requests)
        assert.are.equal("textDocument/rangeFormatting", requests[2].method)
        assert.are.equal(
            "16",
            requests[2].params.options["org.eclipse.jdt.core.formatter.alignment_for_logical_operator"]
        )
        assert.are.equal(
            "true",
            requests[2].params.options["org.eclipse.jdt.core.formatter.wrap_before_logical_operator"]
        )
        assert.are.equal("false", requests[2].params.options["org.eclipse.jdt.core.formatter.join_wrapped_lines"])
    end)

    it("ignores single-line and unsupported binary expressions", function()
        local inline = fake_binary(
            { 3, 23, 3, 39 },
            "+",
            { left = fake_literal({ 3, 23, 3, 29 }), right = fake_literal({ 3, 33, 3, 39 }) },
            fake_node("variable_declarator", { 3, 8, 3, 40 })
        )
        local bitwise = fake_binary({ 5, 16, 7, 18 }, "&", {
            left = fake_operand("identifier", { 5, 16, 5, 17 }),
            right = fake_operand("identifier", { 7, 16, 7, 18 }),
        }, fake_node("variable_declarator", { 5, 8, 7, 18 }))
        stub_tree({ inline, bitwise })

        formatter.format(7)

        assert.are.equal(1, #requests)
        assert.are.equal("textDocument/formatting", requests[1].method)
    end)

    it("skips nested inner concatenations, keeping only the outermost range", function()
        local declarator = fake_node("variable_declarator", { 3, 8, 5, 30 })
        local outer = fake_binary({ 3, 23, 5, 29 }, "+", {}, declarator)
        local inner = fake_binary({ 3, 23, 4, 29 }, "+", { left = fake_literal({ 3, 23, 3, 29 }) }, outer)
        outer.field = function(_, name)
            if name == "operator" then
                return { { __text = "+" } }
            end
            if name == "left" then
                return { inner }
            end
            if name == "right" then
                return { fake_literal({ 5, 23, 5, 29 }) }
            end
            return {}
        end
        stub_tree({ outer, inner })

        formatter.format(7)

        assert.are.equal(2, #requests)
        assert.are.equal("textDocument/rangeFormatting", requests[2].method)
        assert.are.equal(3, requests[2].params.range.start.line)
        assert.are.equal(5, requests[2].params.range["end"].line)
    end)
end)
