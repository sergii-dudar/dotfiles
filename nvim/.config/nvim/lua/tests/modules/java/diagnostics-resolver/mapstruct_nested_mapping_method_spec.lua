local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-nested-mapping-method", function()
    local resolver
    local state
    local path_requests
    local select_calls
    local chosen_index

    --- Configure Java Tree-sitter test nodes for a mapper method and its owner.
    ---@param owner_kind string
    ---@param method_row integer
    ---@param owner_end_row integer
    local function stub_java_tree(owner_kind, method_row, owner_end_row)
        local owner = {
            type = function()
                return owner_kind
            end,
            start = function()
                return 0, 0
            end,
            range = function()
                return 0, 0, owner_end_row, 1
            end,
            parent = function()
                return nil
            end,
        }
        local method = {
            type = function()
                return "method_declaration"
            end,
            start = function()
                return method_row, 4
            end,
            range = function()
                return method_row, 4, method_row, 80
            end,
            parent = function()
                return owner
            end,
        }
        vim.treesitter = {
            get_parser = function()
                return {
                    parse = function()
                        return {
                            {
                                root = function()
                                    return {
                                        named_descendant_for_range = function()
                                            return method
                                        end,
                                    }
                                end,
                            },
                        }
                    end,
                }
            end,
        }
    end

    --- Build a fake Tree-sitter node spanning one row.
    ---@param kind string
    ---@param row integer
    ---@param start_col integer
    ---@param end_col integer
    ---@param fields? table<string, table>
    ---@return table
    local function fake_node(kind, row, start_col, end_col, fields)
        return {
            type = function()
                return kind
            end,
            range = function()
                return row, start_col, row, end_col
            end,
            named = function()
                return true
            end,
            field = function(_, name)
                return fields and fields[name] and { fields[name] } or {}
            end,
        }
    end

    --- Iterate a list the way Tree-sitter iterates children.
    ---@param children table[]
    ---@return fun(): table|nil
    local function iterate(children)
        local index = 0
        return function()
            index = index + 1
            return children[index]
        end
    end

    --- Build a fake `@Mapping(...)` annotation node from its buffer line.
    --- Argument columns are located in the line, so `get_node_text` reads them back.
    ---@param row integer
    ---@param line string
    ---@param arguments { key: string, value: string }[] values as written, quotes included
    ---@return table
    local function fake_annotation(row, line, arguments)
        local start_col = line:find("@", 1, true) - 1
        local name_node = fake_node("identifier", row, start_col + 1, start_col + 1 + #"Mapping")
        local pairs = {}
        for _, argument in ipairs(arguments) do
            local pair_start = assert(line:find(argument.key .. " = " .. argument.value, 1, true))
            local value_index = pair_start + #argument.key + 3
            local value_kind = argument.value:sub(1, 1) == '"' and "string_literal" or "true"
            pairs[#pairs + 1] =
                fake_node("element_value_pair", row, pair_start - 1, value_index - 1 + #argument.value, {
                    key = fake_node("identifier", row, pair_start - 1, pair_start - 1 + #argument.key),
                    value = fake_node(value_kind, row, value_index - 1, value_index - 1 + #argument.value),
                })
        end
        local arguments_node = fake_node("annotation_argument_list", row, start_col + 8, #line)
        arguments_node.iter_children = function()
            return iterate(pairs)
        end
        return fake_node("annotation", row, start_col, #line, { name = name_node, arguments = arguments_node })
    end

    --- Configure a Java Tree-sitter test tree whose mapper method carries annotations.
    ---@param annotations table[] fake annotation nodes
    ---@param method_start_row integer first annotation row
    ---@param method_end_row integer declaration row
    ---@param method_end_col integer
    local function stub_annotated_java_tree(annotations, method_start_row, method_end_row, method_end_col)
        stub_java_tree("interface_declaration", method_start_row, method_end_row + 1)
        local parser = vim.treesitter.get_parser()
        local method = parser:parse()[1]:root():named_descendant_for_range()
        method.range = function()
            return method_start_row, 4, method_end_row, method_end_col
        end
        local modifiers = fake_node("modifiers", method_start_row, 4, method_end_row - 1, 60)
        modifiers.iter_children = function()
            return iterate(annotations)
        end
        method.iter_children = function()
            return iterate({ modifiers })
        end
        vim.treesitter.get_node_text = function(node, bufnr)
            local start_row, start_col, _, end_col = node:range()
            local line = state.buffer_lines[bufnr][start_row + 1] or ""
            return line:sub(start_col + 1, end_col)
        end
    end

    before_each(function()
        _, state = helper.reset_vim()
        path_requests = {}
        select_calls = {}
        chosen_index = 1
        vim.ui = {
            select = function(items, opts, on_choice)
                select_calls[#select_calls + 1] = { items = items, opts = opts }
                on_choice(items[chosen_index])
            end,
        }
        vim.schedule = function(callback)
            callback()
        end
        helper.stub_module("modules.java.mapstruct", {
            get_method_types = function(_, callback)
                callback({
                    sources = { { name = "request", type = "example.ChargeCalculationRequest" } },
                    target_type = "api.PaymentChargesCalculationRequest",
                })
            end,
            resolve_path_type = function(params, callback)
                path_requests[#path_requests + 1] = vim.deepcopy(params)
                if params.sources[1].name == "$target" then
                    callback({ className = "api.Account", simpleName = "Account", packageName = "api" })
                else
                    callback({
                        className = "example.ChargeCalculationRequest$ChargeAccount",
                        simpleName = "ChargeAccount",
                        packageName = "example",
                    })
                end
            end,
        })

        state.buffer_options[1] = {
            expandtab = true,
            shiftwidth = 4,
            tabstop = 4,
        }
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

        resolver = helper.reload("modules.java.diagnostics-resolver.mapstruct-nested-mapping-method")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver.java-context",
            "modules.java.diagnostics-resolver.java-import-resolver",
            "modules.java.diagnostics-resolver.mapstruct-method-type-resolver",
            "modules.java.diagnostics-resolver.mapstruct-nested-mapping-method",
            "modules.java.diagnostics-resolver.mapstruct-nested-target-mappings",
            "modules.java.diagnostics-resolver.mapstruct-unmapped-target",
            "modules.java.mapstruct",
            "utils.nio-util",
        })
    end)

    it("parses nested source and target mapping types", function()
        -- given
        local message = 'Unmapped target property: "identification". Mapping from property '
            .. '"ChargeCalculationRequest.ChargeAccount debtorAccount" to "Account debtorAccount"'

        -- when
        local mapping = resolver.parse_mapping(message)

        -- then
        assert.are.same({
            unmapped_property = "identification",
            source_type = "ChargeCalculationRequest.ChargeAccount",
            source_property = "debtorAccount",
            target_type = "Account",
            target_property = "debtorAccount",
            method_name = "toAccount",
            signature = "Account toAccount(ChargeCalculationRequest.ChargeAccount debtorAccount)",
        }, mapping)
    end)

    it("inserts an abstract nested mapping method into a mapper class", function()
        -- given
        state.buffer_lines[1] = {
            "package example.mapper;",
            "",
            "import example.ChargeCalculationRequest;",
            "import example.Source;",
            "import example.Target;",
            "",
            "public abstract class ChargeCalculationAdapterMapper {",
            "    public abstract Target toRequest(Source request);",
            "",
            "    protected abstract String existing(String value);",
            "}",
        }
        stub_java_tree("class_declaration", 7, 10)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 7,
                col = 40,
                message = 'Unmapped target property: "identification". Mapping from property '
                    .. '"ChargeCalculationRequest.ChargeAccount debtorAccount" to "Account debtorAccount"',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.same({
            "package example.mapper;",
            "",
            "import api.Account;",
            "import example.ChargeCalculationRequest;",
            "import example.Source;",
            "import example.Target;",
            "",
            "public abstract class ChargeCalculationAdapterMapper {",
            "    public abstract Target toRequest(Source request);",
            "",
            "    protected abstract Account toAccount(ChargeCalculationRequest.ChargeAccount debtorAccount);",
            "",
            "    protected abstract String existing(String value);",
            "}",
        }, state.buffer_lines[1])
        assert.are.same({ 11, 31 }, state.cursor)
        assert.are.equal(2, #path_requests)
        assert.are.equal("debtorAccount.", path_requests[1].path_expression)
        assert.are.equal("request", path_requests[1].sources[1].name)
        assert.are.equal("debtorAccount.", path_requests[2].path_expression)
        assert.are.equal("$target", path_requests[2].sources[1].name)
    end)

    it("uses an implicit abstract declaration for a mapper interface", function()
        -- given
        state.buffer_lines[1] = {
            "package example.mapper;",
            "",
            "import example.ChargeCalculationRequest;",
            "import example.Source;",
            "import example.Target;",
            "",
            "public interface ChargeCalculationAdapterMapper {",
            "    Target toRequest(Source request);",
            "}",
        }
        stub_java_tree("interface_declaration", 7, 8)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 7,
                col = 25,
                message = 'Unmapped target property: "identification". Mapping from property '
                    .. '"ChargeCalculationRequest.ChargeAccount debtorAccount" to "Account debtorAccount"',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.equal(
            "    Account toAccount(ChargeCalculationRequest.ChargeAccount debtorAccount);",
            state.buffer_lines[1][11]
        )
    end)

    it("parses a forged mapping with a dotted path", function()
        -- given
        local message = 'Unmapped target property: "rgb". Mapping from property '
            .. '"Model.Color house.roof.color" to "Model.ColorDto house.roof.color".'

        -- when
        local mapping = resolver.parse_mapping(message)

        -- then
        assert.are.same({
            unmapped_property = "rgb",
            source_type = "Model.Color",
            source_property = "house.roof.color",
            target_type = "Model.ColorDto",
            target_property = "house.roof.color",
            method_name = "toColorDto",
            signature = "Model.ColorDto toColorDto(Model.Color color)",
        }, mapping)
    end)

    it("parses a forged collection element mapping", function()
        -- given
        local message = 'Unmapped target property: "color". Mapping from Collection element '
            .. '"Model.Wheel car.wheels" to "Model.WheelDto car.wheels".'

        -- when
        local mapping = resolver.parse_mapping(message)

        -- then
        assert.are.same({
            unmapped_property = "color",
            source_type = "Model.Wheel",
            source_property = "car.wheels",
            target_type = "Model.WheelDto",
            target_property = "car.wheels",
            method_name = "toWheelDto",
            signature = "Model.WheelDto toWheelDto(Model.Wheel wheel)",
            element_kind = "Collection element",
        }, mapping)
    end)

    it("generates the element mapping method for a forged collection element", function()
        -- given
        state.buffer_lines[1] = {
            "package example.mapper;",
            "",
            "import example.Source;",
            "import example.Target;",
            "",
            "public interface ChargeCalculationAdapterMapper {",
            "    Target toRequest(Source request);",
            "}",
        }
        stub_java_tree("interface_declaration", 6, 7)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 6,
                col = 25,
                message = 'Unmapped target property: "identification". Mapping from Collection element '
                    .. '"ChargeCalculationRequest.ChargeAccount accounts" to "Account accounts".',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.equal(0, #select_calls)
        assert.are.equal(2, #path_requests)
        assert.are.equal("accounts.first.", path_requests[1].path_expression)
        assert.are.equal("request", path_requests[1].sources[1].name)
        assert.are.equal("accounts.first.", path_requests[2].path_expression)
        assert.are.equal("$target", path_requests[2].sources[1].name)
        assert.are.equal(
            "    Account toAccount(ChargeCalculationRequest.ChargeAccount chargeAccount);",
            state.buffer_lines[1][11]
        )
    end)

    it("parses a forged map value mapping", function()
        -- given
        local message = 'Unmapped target property: "pronunciation". Mapping from Map value '
            .. '"Word words{:value}" to "WordDto words{:value}".'

        -- when
        local mapping = resolver.parse_mapping(message)

        -- then
        assert.are.same({
            unmapped_property = "pronunciation",
            source_type = "Word",
            source_property = "words{:value}",
            target_type = "WordDto",
            target_property = "words{:value}",
            method_name = "toWordDto",
            signature = "WordDto toWordDto(Word word)",
            element_kind = "Map value",
        }, mapping)
    end)

    it("generates the mapping method for a forged map value", function()
        -- given
        state.buffer_lines[1] = {
            "package example.mapper;",
            "",
            "import example.Source;",
            "import example.Target;",
            "",
            "public interface ChargeCalculationAdapterMapper {",
            "    Target toRequest(Source request);",
            "}",
        }
        stub_java_tree("interface_declaration", 6, 7)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 6,
                col = 25,
                message = 'Unmapped target property: "identification". Mapping from Map value '
                    .. '"ChargeCalculationRequest.ChargeAccount accounts{:value}" to "Account accounts{:value}".',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.equal(2, #path_requests)
        assert.are.equal("accounts.{:value}.", path_requests[1].path_expression)
        assert.are.equal("request", path_requests[1].sources[1].name)
        assert.are.equal("accounts.{:value}.", path_requests[2].path_expression)
        assert.are.equal("$target", path_requests[2].sources[1].name)
        assert.are.equal(
            "    Account toAccount(ChargeCalculationRequest.ChargeAccount chargeAccount);",
            state.buffer_lines[1][11]
        )
    end)

    it("offers the mapping method and the nested target annotations", function()
        -- given
        local mapping = resolver.parse_mapping(
            'Unmapped target property: "identification". Mapping from property '
                .. '"ChargeCalculationRequest.ChargeAccount debtorAccount" to "Account debtorAccount"'
        )

        -- when
        local choices = resolver.choices(mapping, nil)

        -- then
        assert.are.same({
            {
                kind = "generate",
                label = "Generate mapping method: Account toAccount(ChargeCalculationRequest.ChargeAccount debtorAccount)",
            },
            {
                kind = "properties",
                label = "Map or ignore nested target properties: debtorAccount.identification",
            },
        }, choices)
    end)

    it("mentions the nested annotations the mapping method takes over", function()
        -- given
        local mapping = resolver.parse_mapping(
            'Unmapped target property: "identification". Mapping from property '
                .. '"ChargeCalculationRequest.ChargeAccount debtorAccount" to "Account debtorAccount"'
        )

        -- when
        local choices = resolver.choices(mapping, { property = "debtorAccount", mappings = { {}, {} } })

        -- then
        assert.are.equal(
            "Generate mapping method: Account toAccount(ChargeCalculationRequest.ChargeAccount debtorAccount)"
                .. " (moves 2 nested @Mapping)",
            choices[1].label
        )
    end)

    it("asks before generating the method for a plain property", function()
        -- given
        state.buffer_lines[1] = {
            "package example.mapper;",
            "",
            "public interface ChargeCalculationAdapterMapper {",
            "    Target toRequest(Source request);",
            "}",
        }
        stub_java_tree("interface_declaration", 3, 4)

        -- when
        resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 3,
                col = 25,
                message = 'Unmapped target property: "identification". Mapping from property '
                    .. '"ChargeCalculationRequest.ChargeAccount debtorAccount" to "Account debtorAccount"',
            },
        })

        -- then
        assert.are.equal(1, #select_calls)
        assert.are.equal("MapStruct nested mapping: debtorAccount", select_calls[1].opts.prompt)
        assert.are.equal(2, #select_calls[1].items)
        assert.are.equal("generate", select_calls[1].items[1].kind)
        assert.are.equal("properties", select_calls[1].items[2].kind)
    end)

    it("annotates the nested target properties on the owning method when chosen", function()
        -- given
        chosen_index = 2
        local picker_items, picker_prompt
        helper.stub_module("utils.nio-util", {
            run = function(callback)
                callback()
            end,
            multi_select = function(items, prompt)
                picker_items = items
                picker_prompt = prompt
                for _, item in ipairs(items) do
                    if item.kind == "ignore_all" then
                        return { item }
                    end
                end
                return nil
            end,
        })
        state.buffer_lines[1] = {
            "package example.mapper;",
            "",
            "import org.mapstruct.Mapping;",
            "",
            "public interface ChargeCalculationAdapterMapper {",
            '    @Mapping(target = "debtorAccount", source = "request")',
            "    Target toRequest(Source request);",
            "}",
        }
        stub_java_tree("interface_declaration", 5, 7)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 6,
                col = 25,
                message = 'Unmapped target property: "identification". Mapping from property '
                    .. '"ChargeCalculationRequest.ChargeAccount debtorAccount" to "Account debtorAccount"',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.equal(0, #path_requests)
        assert.are.equal("MapStruct unmapped target properties of debtorAccount", picker_prompt)
        assert.are.equal("Add unmapped target property: debtorAccount.identification", picker_items[2].name)
        assert.are.same({
            "package example.mapper;",
            "",
            "import org.mapstruct.Mapping;",
            "",
            "public interface ChargeCalculationAdapterMapper {",
            '    @Mapping(target = "debtorAccount.identification", ignore = true)',
            '    @Mapping(target = "debtorAccount", source = "request")',
            "    Target toRequest(Source request);",
            "}",
        }, state.buffer_lines[1])
    end)

    it("does nothing when the choice is cancelled", function()
        -- given
        chosen_index = 3
        state.buffer_lines[1] = {
            "public interface ChargeCalculationAdapterMapper {",
            "    Target toRequest(Source request);",
            "}",
        }
        stub_java_tree("interface_declaration", 1, 2)

        -- when
        resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 1,
                col = 25,
                message = 'Unmapped target property: "identification". Mapping from property '
                    .. '"ChargeCalculationRequest.ChargeAccount debtorAccount" to "Account debtorAccount"',
            },
        })

        -- then
        assert.are.equal(1, #select_calls)
        assert.are.equal(0, #path_requests)
        assert.are.equal(3, #state.buffer_lines[1])
    end)

    it("moves nested target annotations into the generated mapping method", function()
        -- given
        require("modules.java.mapstruct").resolve_path_type = function(params, callback)
            path_requests[#path_requests + 1] = vim.deepcopy(params)
            local path = params.path_expression
            if params.sources[1].name == "$target" then
                callback({ className = "api.Account", simpleName = "Account", packageName = "api" })
            elseif path == "request.account." then
                callback({
                    className = "example.ChargeCalculationRequest$ChargeAccount",
                    simpleName = "ChargeAccount",
                    packageName = "example",
                })
            else
                callback({ className = "java.lang.String", simpleName = "String", packageName = "java.lang" })
            end
        end
        state.buffer_lines[1] = {
            "package example.mapper;",
            "",
            "import example.ChargeCalculationRequest;",
            "import example.Source;",
            "import example.Target;",
            "",
            "public interface ChargeCalculationAdapterMapper {",
            '    @Mapping(target = "debtorAccount", source = "request")',
            '    @Mapping(target = "debtorAccount.identification", source = "request.account.id")',
            '    @Mapping(target = "debtorAccount.name", ignore = true)',
            '    @Mapping(target = "creditor.name", source = "request.creditor")',
            "    Target toRequest(Source request);",
            "}",
        }
        local lines = state.buffer_lines[1]
        stub_annotated_java_tree({
            fake_annotation(
                7,
                lines[8],
                { { key = "target", value = '"debtorAccount"' }, { key = "source", value = '"request"' } }
            ),
            fake_annotation(8, lines[9], {
                { key = "target", value = '"debtorAccount.identification"' },
                { key = "source", value = '"request.account.id"' },
            }),
            fake_annotation(
                9,
                lines[10],
                { { key = "target", value = '"debtorAccount.name"' }, { key = "ignore", value = "true" } }
            ),
            fake_annotation(
                10,
                lines[11],
                { { key = "target", value = '"creditor.name"' }, { key = "source", value = '"request.creditor"' } }
            ),
        }, 7, 11, 37)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 11,
                col = 11,
                message = 'Unmapped target property: "currency". Mapping from property '
                    .. '"ChargeCalculationRequest.ChargeAccount debtorAccount" to "Account debtorAccount"',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.matches("moves 2 nested @Mapping", select_calls[1].items[1].label, nil, true)
        assert.are.same({
            "package example.mapper;",
            "",
            "import api.Account;",
            "import example.ChargeCalculationRequest;",
            "import example.Source;",
            "import example.Target;",
            "",
            "public interface ChargeCalculationAdapterMapper {",
            '    @Mapping(target = "debtorAccount", source = "request.account")',
            '    @Mapping(target = "creditor.name", source = "request.creditor")',
            "    Target toRequest(Source request);",
            "",
            '    @Mapping(target = "identification", source = "id")',
            '    @Mapping(target = "name", ignore = true)',
            "    Account toAccount(ChargeCalculationRequest.ChargeAccount debtorAccount);",
            "}",
        }, state.buffer_lines[1])
        assert.are.same({ 15, 12 }, state.cursor)
        assert.are.same(
            { "request.account.id.", "request.account.", "debtorAccount." },
            vim.tbl_map(function(request)
                return request.path_expression
            end, path_requests)
        )
        assert.are.equal("$target", path_requests[3].sources[1].name)
    end)

    it("resolves the owning method's own source when the diagnostic names its last segment", function()
        -- given
        require("modules.java.mapstruct").resolve_path_type = function(params, callback)
            path_requests[#path_requests + 1] = vim.deepcopy(params)
            if params.sources[1].name == "$target" then
                callback({ className = "api.Account", simpleName = "Account", packageName = "api" })
            elseif params.path_expression == "request.header.account." then
                callback({
                    className = "example.ChargeCalculationRequest$ChargeAccount",
                    simpleName = "ChargeAccount",
                    packageName = "example",
                })
            else
                callback(nil, "MapStruct did not resolve a type for path: " .. params.path_expression)
            end
        end
        state.buffer_lines[1] = {
            "package example.mapper;",
            "",
            "import api.Account;",
            "import example.ChargeCalculationRequest;",
            "",
            "public interface ChargeCalculationAdapterMapper {",
            '    @Mapping(target = "debtorAccount", source = "request.header.account")',
            "    Target toRequest(Source request);",
            "}",
        }
        local lines = state.buffer_lines[1]
        stub_annotated_java_tree({
            fake_annotation(6, lines[7], {
                { key = "target", value = '"debtorAccount"' },
                { key = "source", value = '"request.header.account"' },
            }),
        }, 6, 7, 37)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 7,
                col = 11,
                message = 'Unmapped target property: "currency". Mapping from property '
                    .. '"ChargeCalculationRequest.ChargeAccount account" to "Account debtorAccount"',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.same({
            "package example.mapper;",
            "",
            "import api.Account;",
            "import example.ChargeCalculationRequest;",
            "",
            "public interface ChargeCalculationAdapterMapper {",
            '    @Mapping(target = "debtorAccount", source = "request.header.account")',
            "    Target toRequest(Source request);",
            "",
            "    Account toAccount(ChargeCalculationRequest.ChargeAccount account);",
            "}",
        }, state.buffer_lines[1])
        assert.are.same(
            { "request.header.account.", "debtorAccount." },
            vim.tbl_map(function(request)
                return request.path_expression
            end, path_requests)
        )
    end)
end)
