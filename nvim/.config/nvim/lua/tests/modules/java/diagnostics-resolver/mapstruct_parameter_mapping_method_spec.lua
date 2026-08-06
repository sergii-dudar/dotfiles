local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-parameter-mapping-method", function()
    local resolver
    local state
    local path_requests

    --- Configure Java Tree-sitter test nodes for a mapper method and owner.
    ---@param method_row integer
    ---@param owner_end_row integer
    local function stub_java_tree(method_row, owner_end_row)
        local owner = {
            type = function()
                return "class_declaration"
            end,
            start = function()
                return 7, 0
            end,
            range = function()
                return 7, 0, owner_end_row, 1
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

    before_each(function()
        _, state = helper.reset_vim()
        path_requests = {}
        helper.stub_module("modules.java.mapstruct", {
            get_method_types = function(_, callback)
                callback({
                    sources = {
                        {
                            name = "initiation",
                            type = "ua.model.transfer.CardTransferInitiation",
                        },
                    },
                    target_type = "ua.model.initiation.CardTransferInitiationResponse",
                })
            end,
            resolve_path_type = function(params, callback)
                path_requests[#path_requests + 1] = vim.deepcopy(params)
                callback({
                    className = "java.util.Set",
                    simpleName = "Set",
                    packageName = "java.util",
                })
            end,
        })

        state.buffer_options[1] = {
            expandtab = true,
            shiftwidth = 4,
            tabstop = 4,
        }
        resolver = helper.reload("modules.java.diagnostics-resolver.mapstruct-parameter-mapping-method")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver.java-context",
            "modules.java.diagnostics-resolver.java-import-resolver",
            "modules.java.diagnostics-resolver.mapstruct-parameter-mapping-method",
            "modules.java.mapstruct",
        })
    end)

    it("parses a parameter mapping method with a nested collection element", function()
        -- given
        local message = 'Can\'t map parameter "CardTransferInitiation initiation" to '
            .. '"Set<CardTransferInitiationResponse.InitiatedTransfers> initiatedTransfers". '
            .. "Consider to declare/implement a mapping method: "
            .. '"Set<CardTransferInitiationResponse.InitiatedTransfers> map(CardTransferInitiation value)"'

        -- when
        local suggested = resolver.parse_suggested_method(message)

        -- then
        assert.are.same({
            signature = "Set<CardTransferInitiationResponse.InitiatedTransfers> map(CardTransferInitiation value)",
            source_type = "CardTransferInitiation",
            source_parameter = "initiation",
            target_type = "Set<CardTransferInitiationResponse.InitiatedTransfers>",
            target_property = "initiatedTransfers",
            container_type = "Set",
            element_type = "CardTransferInitiationResponse.InitiatedTransfers",
            method_name = "map",
            parameter_type = "CardTransferInitiation",
            parameter_name = "value",
        }, suggested)
    end)

    it("parses a non-generic parameter mapping method", function()
        -- given
        local message = 'Can\'t map parameter "Source source" to "Target target". '
            .. 'Consider to declare/implement a mapping method: "Target map(Source value)"'

        -- when
        local suggested = resolver.parse_suggested_method(message)

        -- then
        assert.are.same({
            signature = "Target map(Source value)",
            source_type = "Source",
            source_parameter = "source",
            target_type = "Target",
            target_property = "target",
            container_type = "Target",
            method_name = "map",
            parameter_type = "Source",
            parameter_name = "value",
        }, suggested)
    end)

    it("generates the suggested method and directly imports the collection type", function()
        -- given
        local message = 'Can\'t map parameter "CardTransferInitiation initiation" to '
            .. '"Set<CardTransferInitiationResponse.InitiatedTransfers> initiatedTransfers". '
            .. "Consider to declare/implement a mapping method: "
            .. '"Set<CardTransferInitiationResponse.InitiatedTransfers> map(CardTransferInitiation value)"'
        state.buffer_lines[1] = {
            "package ua.mapper;",
            "",
            "import org.mapstruct.Mapper;",
            "import ua.model.initiation.CardTransferInitiationResponse;",
            "import ua.model.initiation.CardTransferInitiationResponse.InitiatedTransfers;",
            "import ua.model.transfer.CardTransferInitiation;",
            "",
            "public abstract class CardTransferInitiationMapper {",
            "    public abstract CardTransferInitiationResponse toResponse(CardTransferInitiation initiation);",
            "}",
        }
        stub_java_tree(8, 9)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = { lnum = 8, col = 10, message = message },
        })

        -- then
        assert.is_true(resolved)
        assert.are.same({
            "package ua.mapper;",
            "",
            "import org.mapstruct.Mapper;",
            "import ua.model.initiation.CardTransferInitiationResponse;",
            "import ua.model.initiation.CardTransferInitiationResponse.InitiatedTransfers;",
            "import ua.model.transfer.CardTransferInitiation;",
            "",
            "import java.util.Set;",
            "",
            "public abstract class CardTransferInitiationMapper {",
            "    public abstract CardTransferInitiationResponse toResponse(CardTransferInitiation initiation);",
            "",
            "    protected Set<CardTransferInitiationResponse.InitiatedTransfers> map(CardTransferInitiation value) {",
            "        return ;",
            "    }",
            "}",
        }, state.buffer_lines[1])
        assert.are.same({ 14, 15 }, state.cursor)
        assert.are.equal("startinsert", state.commands[#state.commands])
        assert.are.equal(1, #path_requests)
        assert.are.equal("initiatedTransfers.", path_requests[1].path_expression)
        assert.are.same({
            { name = "$target", type = "ua.model.initiation.CardTransferInitiationResponse" },
        }, path_requests[1].sources)
    end)
end)
