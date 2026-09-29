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

    describe("method lookup", function()
        --- Build a Tree-sitter node double that only carries source text.
        ---@param text string
        ---@return table
        local function text_node(text)
            return { text = text }
        end

        --- Build a method declaration node double.
        ---@param return_type string
        ---@param name string
        ---@param parameters { type: string, name: string }[]
        ---@return table
        local function method_node(return_type, name, parameters)
            local parameter_nodes = {}
            for _, parameter in ipairs(parameters) do
                parameter_nodes[#parameter_nodes + 1] = {
                    type = function()
                        return "formal_parameter"
                    end,
                    field = function(_, field)
                        if field == "type" then
                            return { text_node(parameter.type) }
                        end
                        return { text_node(parameter.name) }
                    end,
                }
            end

            return {
                field = function(_, field)
                    if field == "type" then
                        return { text_node(return_type) }
                    elseif field == "name" then
                        return { text_node(name) }
                    end
                    return {
                        {
                            iter_children = function()
                                local index = 0
                                return function()
                                    index = index + 1
                                    return parameter_nodes[index]
                                end
                            end,
                        },
                    }
                end,
            }
        end

        --- Expose method declarations through a Tree-sitter query double.
        ---@param methods table[]
        local function stub_declared_methods(methods)
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
                get_node_text = function(node)
                    return node.text
                end,
                query = {
                    parse = function()
                        return {
                            iter_captures = function()
                                local index = 0
                                return function()
                                    index = index + 1
                                    if methods[index] then
                                        return 1, methods[index]
                                    end
                                end
                            end,
                        }
                    end,
                },
            }
        end

        it("finds a signature written on one line", function()
            -- given
            state.buffer_lines[1] = { "    protected abstract long map(Duration value);" }

            -- then
            assert.is_true(java_context.method_exists(1, "long map(Duration value)"))
        end)

        it("finds a signature behind a qualified return type", function()
            -- given
            state.buffer_lines[1] = {
                "    protected abstract ua.target.TransferType toTransferType(TransferDirection direction);",
            }

            -- then
            assert.is_true(java_context.method_exists(1, "TransferType toTransferType(TransferDirection direction)"))
        end)

        it("ignores a signature that only appears in comments", function()
            -- given
            state.buffer_lines[1] = {
                "    // long map(Duration value)",
                "    /* long map(Duration value) */",
                "     * long map(Duration value)",
            }

            -- then
            assert.is_false(java_context.method_exists(1, "long map(Duration value)"))
        end)

        it("ignores the tail of a longer type name", function()
            -- given
            state.buffer_lines[1] = { "    default Xlong map(Duration value) { return null; }" }

            -- then
            assert.is_false(java_context.method_exists(1, "long map(Duration value)"))
        end)

        it("finds a declaration split across lines", function()
            -- given
            state.buffer_lines[1] = {
                "    default long map(",
                "        Duration value",
                "    ) {",
            }
            stub_declared_methods({ method_node("long", "map", { { type = "Duration", name = "value" } }) })

            -- then
            assert.is_true(java_context.method_exists(1, "long map(Duration value)"))
        end)

        it("treats a shared name and erased parameter types as a clash", function()
            -- given
            stub_declared_methods({
                method_node("long", "map", { { type = "java.time.Duration", name = "duration" } }),
                method_node("List<B>", "convert", { { type = "List<A>", name = "values" } }),
            })

            -- then
            assert.is_true(java_context.method_name_taken(1, "map", { "Duration" }))
            assert.is_true(java_context.method_name_taken(1, "convert", { "java.util.List<C>" }))
            assert.is_false(java_context.method_name_taken(1, "map", { "String" }))
            assert.is_false(java_context.method_name_taken(1, "other", { "Duration" }))
            assert.is_false(java_context.method_name_taken(1, "map", { "Duration", "String" }))
        end)

        it("reports no clash without a syntax tree", function()
            -- then
            assert.is_false(java_context.method_name_taken(1, "map", { "Duration" }))
        end)

        it("keeps the preferred name when it is free", function()
            -- given
            state.buffer_lines[1] = { "    Target map(Source source);" }
            stub_declared_methods({ method_node("Target", "map", { { type = "Source", name = "source" } }) })

            -- when
            local signature, existing = java_context.available_signature(
                1,
                { "map", "toLong" },
                { "Duration" },
                function(name)
                    return "long " .. name .. "(Duration value)"
                end
            )

            -- then
            assert.are.equal("long map(Duration value)", signature)
            assert.is_nil(existing)
        end)

        it("falls back to the next name when the preferred one clashes", function()
            -- given
            state.buffer_lines[1] = { "    default long map(Duration value) {" }
            stub_declared_methods({ method_node("long", "map", { { type = "Duration", name = "value" } }) })

            -- when
            local signature, existing = java_context.available_signature(
                1,
                { "map", "toInt" },
                { "Duration" },
                function(name)
                    return "int " .. name .. "(Duration value)"
                end
            )

            -- then
            assert.are.equal("int toInt(Duration value)", signature)
            assert.is_nil(existing)
        end)

        it("reports the declared signature instead of generating it again", function()
            -- given
            state.buffer_lines[1] = {
                "    default long map(Duration value) {",
                "    default int toInt(Duration value) {",
            }
            stub_declared_methods({
                method_node("long", "map", { { type = "Duration", name = "value" } }),
                method_node("int", "toInt", { { type = "Duration", name = "value" } }),
            })

            -- when
            local signature, existing = java_context.available_signature(
                1,
                { "map", "toInt" },
                { "Duration" },
                function(name)
                    return "int " .. name .. "(Duration value)"
                end
            )

            -- then
            assert.is_nil(signature)
            assert.are.equal("int toInt(Duration value)", existing)
        end)

        it("reports no method range without a syntax tree", function()
            -- when
            local start_row, end_row = java_context.method_rows_at(1, 3, 4)

            -- then
            assert.is_nil(start_row)
            assert.is_nil(end_row)
        end)
    end)
end)
