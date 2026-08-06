local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-nested-properties-mapping-method", function()
    local resolver
    local state
    local path_requests

    --- Configure Java Tree-sitter test nodes for a mapper method and its owner.
    ---@param method_row integer
    ---@param owner_end_row integer
    local function stub_java_tree(method_row, owner_end_row)
        local owner = {
            type = function()
                return "class_declaration"
            end,
            start = function()
                return 5, 0
            end,
            range = function()
                return 5, 0, owner_end_row, 1
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
                    sources = { { name = "transfer", type = "example.CardTransferInitiation" } },
                    target_type = "api.Envelope",
                })
            end,
            resolve_path_type = function(params, callback)
                path_requests[#path_requests + 1] = vim.deepcopy(params)
                if params.sources[1].name == "$target" then
                    callback({
                        className = "api.CardTransferDetails",
                        simpleName = "CardTransferDetails",
                        packageName = "api",
                    })
                else
                    callback({
                        className = "example.CardTransferInitiation",
                        simpleName = "CardTransferInitiation",
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

        helper.reload("modules.java.diagnostics-resolver.mapstruct-nested-mapping-method")
        resolver = helper.reload("modules.java.diagnostics-resolver.mapstruct-nested-properties-mapping-method")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver.java-context",
            "modules.java.diagnostics-resolver.java-import-resolver",
            "modules.java.diagnostics-resolver.mapstruct-method-type-resolver",
            "modules.java.diagnostics-resolver.mapstruct-nested-mapping-method",
            "modules.java.diagnostics-resolver.mapstruct-nested-properties-mapping-method",
            "modules.java.mapstruct",
        })
    end)

    it("parses properties and nested source and target mapping types", function()
        -- given
        local message = 'Unmapped target properties: "merchantId, terminalId, debtorCardData, creditorCardData". '
            .. 'Mapping from property "CardTransferInitiation transfer" to '
            .. '"CardTransferDetails cardTransferDetails"'

        -- when
        local mapping = resolver.parse_mapping(message)

        -- then
        assert.are.same({
            unmapped_properties = { "merchantId", "terminalId", "debtorCardData", "creditorCardData" },
            source_type = "CardTransferInitiation",
            source_property = "transfer",
            target_type = "CardTransferDetails",
            target_property = "cardTransferDetails",
            method_name = "toCardTransferDetails",
            signature = "CardTransferDetails toCardTransferDetails(CardTransferInitiation transfer)",
        }, mapping)
    end)

    it("inserts the nested mapping method instead of opening the property picker", function()
        -- given
        state.buffer_lines[1] = {
            "package example.mapper;",
            "",
            "import api.Envelope;",
            "import example.CardTransferInitiation;",
            "",
            "public abstract class EnvelopeMapper {",
            "    public abstract Envelope toEnvelope(CardTransferInitiation transfer);",
            "}",
        }
        stub_java_tree(6, 7)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 6,
                col = 37,
                message = 'Unmapped target properties: "merchantId, terminalId, debtorCardData, creditorCardData". '
                    .. 'Mapping from property "CardTransferInitiation transfer" to '
                    .. '"CardTransferDetails cardTransferDetails"',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.same({
            "package example.mapper;",
            "",
            "import api.CardTransferDetails;",
            "import api.Envelope;",
            "import example.CardTransferInitiation;",
            "",
            "public abstract class EnvelopeMapper {",
            "    public abstract Envelope toEnvelope(CardTransferInitiation transfer);",
            "",
            "    protected abstract CardTransferDetails toCardTransferDetails(CardTransferInitiation transfer);",
            "}",
        }, state.buffer_lines[1])
        local declaration = state.buffer_lines[1][10]
        assert.are.same({ 10, declaration:find("toCardTransferDetails", 1, true) - 1 }, state.cursor)
        assert.are.equal(2, #path_requests)
        assert.are.equal("transfer.", path_requests[1].path_expression)
        assert.are.equal("transfer", path_requests[1].sources[1].name)
        assert.are.equal("cardTransferDetails.", path_requests[2].path_expression)
        assert.are.equal("$target", path_requests[2].sources[1].name)
    end)
end)
