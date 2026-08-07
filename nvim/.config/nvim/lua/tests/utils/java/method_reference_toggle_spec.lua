local helper = require("tests.utils.spec_helper")

describe("utils.java.method-reference-toggle", function()
    local toggle
    local state
    local client
    local response

    --- Build the minimal Tree-sitter node interface used by the fallback.
    ---@param node_type string
    ---@param text string
    ---@param range? integer[]
    ---@param children? table[]
    ---@param fields? table<string, table[]>
    ---@return table
    local function fake_node(node_type, text, range, children, fields)
        local node = {
            _type = node_type,
            _text = text,
            _range = range or { 0, 0, 0, 0 },
            _children = children or {},
            _fields = fields or {},
        }

        for _, child in ipairs(node._children) do
            child._parent = node
        end

        function node:type()
            return self._type
        end

        function node:parent()
            return self._parent
        end

        function node:named()
            return true
        end

        function node:field(name)
            return self._fields[name] or {}
        end

        function node:range()
            return table.unpack(self._range)
        end

        function node:iter_children()
            local index = 0
            return function()
                index = index + 1
                return self._children[index]
            end
        end

        return node
    end

    --- Install a small class AST containing `this::buildContext` and its target method.
    ---@param receiver_type? string
    local function install_tree(receiver_type)
        local receiver = fake_node(receiver_type or "this", receiver_type == "identifier" and "service" or "this")
        local reference_name = fake_node("identifier", "buildContext")
        local method_reference = fake_node(
            "method_reference",
            "this::buildContext",
            { 4, 20, 4, 38 },
            { receiver, reference_name }
        )

        local outer_parameter_name = fake_node("identifier", "request")
        local execute_parameters = fake_node("formal_parameters", "(ForecastRequest request)", nil, {
            fake_node("formal_parameter", "ForecastRequest request", nil, { outer_parameter_name }, {
                name = { outer_parameter_name },
            }),
        })
        local execute_name = fake_node("identifier", "execute")
        local execute = fake_node(
            "method_declaration",
            "execute",
            nil,
            { execute_name, execute_parameters, method_reference },
            { name = { execute_name }, parameters = { execute_parameters } }
        )

        local target_parameter_name = fake_node("identifier", "request")
        local target_parameters = fake_node("formal_parameters", "(ForecastRequest request)", nil, {
            fake_node("formal_parameter", "ForecastRequest request", nil, { target_parameter_name }, {
                name = { target_parameter_name },
            }),
        })
        local target_name = fake_node("identifier", "buildContext")
        local target = fake_node(
            "method_declaration",
            "buildContext",
            nil,
            { target_name, target_parameters },
            { name = { target_name }, parameters = { target_parameters } }
        )

        local class_body = fake_node("class_body", "", nil, { execute, target })
        local class = fake_node("class_declaration", "ForecastUseCase", nil, { class_body }, {
            body = { class_body },
        })
        local root = fake_node("program", "", nil, { class })
        function root:named_descendant_for_range()
            return reference_name
        end

        vim.treesitter = {
            get_node_text = function(node)
                return node._text
            end,
            get_parser = function()
                return {
                    parse = function()
                        return {
                            {
                                root = function()
                                    return root
                                end,
                            },
                        }
                    end,
                }
            end,
        }
    end

    before_each(function()
        _, state = helper.reset_vim()
        state.current_buf = 7
        state.cursor = { 5, 30 }
        client = { id = 19, name = "jdtls", offset_encoding = "utf-16" }
        response = {
            err = {
                message = "Internal error.",
                data = "Caused by: java.lang.IllegalArgumentException: Invalid identifier : >Mono<? extends ForecastContext><\n",
            },
        }

        vim.lsp.get_clients = function(filter)
            assert.are.same({ bufnr = 7, name = "jdtls" }, filter)
            return { client }
        end
        vim.api.nvim_buf_get_changedtick = function()
            return 1
        end
        vim.lsp.get_client_by_id = function(client_id)
            return client_id == client.id and client or nil
        end
        client.request = function(_, method, _, callback, bufnr)
            assert.are.equal("textDocument/codeAction", method)
            assert.are.equal(7, bufnr)
            callback(response.err, response.result)
        end

        helper.stub_module("utils.lang.lsp-common", {
            apply_lsp_action = function(action, action_client)
                state.applied_action = { action = action, client = action_client }
            end,
        })
        install_tree()
        toggle = helper.reload("utils.java.method-reference-toggle")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.java.method-reference-toggle", "utils.lang.lsp-common" })
    end)

    it("falls back for a unique same-class this method reference when JDTLS errors", function()
        -- when
        toggle.toggle()

        -- then
        assert.are.same({
            bufnr = 7,
            start_row = 4,
            start_col = 20,
            end_row = 4,
            end_col = 38,
            lines = { "request1 -> this.buildContext(request1)" },
        }, state.set_text)
        assert.are.same({}, state.notifications)
    end)

    it("surfaces the JDTLS error when the safe local fallback does not apply", function()
        -- given
        install_tree("identifier")

        -- when
        toggle.toggle()

        -- then
        assert.is_nil(state.set_text)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
        assert.is_truthy(state.notifications[1].message:find("Invalid identifier: >Mono", 1, true))
    end)

    it("still applies an available compiler-backed conversion", function()
        -- given
        local action = { title = "Convert to lambda expression" }
        response = { result = { action } }

        -- when
        toggle.toggle()

        -- then
        assert.are.same({ action = action, client = client }, state.applied_action)
        assert.is_nil(state.set_text)
    end)

    it("keeps the ordinary no-conversion message when the server did not fail", function()
        -- given
        response = { result = {} }

        -- when
        toggle.toggle()

        -- then
        assert.are.equal("No lambda <-> method reference conversion available here", state.notifications[1].message)
        assert.are.equal(vim.log.levels.INFO, state.notifications[1].level)
    end)
end)
