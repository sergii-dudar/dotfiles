local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-enum-mapping-method", function()
    local resolver
    local state
    local path_requests

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
            range = function()
                return method_row, 4, method_row, 100
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
                    sources = { { name = "request", type = "ua.model.ChargeCalculationRequest" } },
                    target_type = "ua.target.PaymentRequest",
                })
            end,
            resolve_path_type = function(params, callback)
                path_requests[#path_requests + 1] = vim.deepcopy(params)
                if params.sources[1].name == "$target" then
                    callback({
                        className = "ua.target.TransferType",
                        simpleName = "TransferType",
                        packageName = "ua.target",
                    })
                else
                    callback({
                        className = "ua.model.TransferDirection",
                        simpleName = "TransferDirection",
                        packageName = "ua.model",
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
        resolver = helper.reload("modules.java.diagnostics-resolver.mapstruct-enum-mapping-method")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver.java-context",
            "modules.java.diagnostics-resolver.java-import-resolver",
            "modules.java.diagnostics-resolver.mapstruct-enum-mapping-method",
            "modules.java.diagnostics-resolver.mapstruct-method-type-resolver",
            "modules.java.mapstruct",
        })
    end)

    it("parses enum types, properties, and constants", function()
        -- given
        local message = 'The following constants from the property "TransferDirection direction" enum have no '
            .. 'corresponding constant in the "TransferType transferType" enum and must be be mapped via adding '
            .. "additional mappings: EXTERNAL, EXTERNAL_CROSS_BORDER, EXTERNAL_TO_INTERNAL, INTERNAL."

        -- when
        local mapping = resolver.parse_mapping(message)

        -- then
        assert.are.same({
            source_type = "TransferDirection",
            source_property = "direction",
            target_type = "TransferType",
            target_property = "transferType",
            constants = { "EXTERNAL", "EXTERNAL_CROSS_BORDER", "EXTERNAL_TO_INTERNAL", "INTERNAL" },
            method_name = "toTransferType",
            signature = "TransferType toTransferType(TransferDirection direction)",
        }, mapping)
    end)

    it("generates value mappings and imports resolved enum types", function()
        -- given
        local message = 'The following constants from the property "TransferDirection direction" enum have no '
            .. 'corresponding constant in the "TransferType transferType" enum and must be be mapped via adding '
            .. "additional mappings: EXTERNAL, EXTERNAL_CROSS_BORDER, EXTERNAL_TO_INTERNAL, INTERNAL."
        state.buffer_lines[1] = {
            "package ua.mapper;",
            "",
            "import org.mapstruct.Mapper;",
            "import ua.model.ChargeCalculationRequest;",
            "import ua.model.TransferDirection;",
            "import ua.target.PaymentRequest;",
            "",
            "public abstract class ChargeCalculationAdapterMapper {",
            "    public abstract PaymentRequest toRequest(ChargeCalculationRequest request);",
            "",
            '    // @ValueMapping(target = "OLD", source = "OLD")',
            "    // protected abstract ua.target.TransferType toTransferType(TransferDirection direction);",
            "}",
        }
        stub_java_tree(8, 12)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = { lnum = 8, col = 40, message = message },
        })

        -- then
        assert.is_true(resolved)
        assert.are.same({
            "package ua.mapper;",
            "",
            "import org.mapstruct.Mapper;",
            "import org.mapstruct.ValueMapping;",
            "import ua.model.ChargeCalculationRequest;",
            "import ua.model.TransferDirection;",
            "import ua.target.PaymentRequest;",
            "import ua.target.TransferType;",
            "",
            "public abstract class ChargeCalculationAdapterMapper {",
            "    public abstract PaymentRequest toRequest(ChargeCalculationRequest request);",
            "",
            '    @ValueMapping(target = "", source = "EXTERNAL")',
            '    @ValueMapping(target = "", source = "EXTERNAL_CROSS_BORDER")',
            '    @ValueMapping(target = "", source = "EXTERNAL_TO_INTERNAL")',
            '    @ValueMapping(target = "", source = "INTERNAL")',
            "    protected abstract TransferType toTransferType(TransferDirection direction);",
            "",
            '    // @ValueMapping(target = "OLD", source = "OLD")',
            "    // protected abstract ua.target.TransferType toTransferType(TransferDirection direction);",
            "}",
        }, state.buffer_lines[1])
        assert.are.same({ 13, 28 }, state.cursor)
        assert.are.equal("startinsert", state.commands[#state.commands])
        assert.are.equal(2, #path_requests)
        assert.are.equal("direction.", path_requests[1].path_expression)
        assert.are.equal("transferType.", path_requests[2].path_expression)
    end)

    it("parses a nested source path and names the parameter after its last property", function()
        -- given
        local message = 'The following constants from the property "Model.Dir source.config.direction" enum have no '
            .. 'corresponding constant in the "Model.Kind kind" enum and must be be mapped via adding '
            .. "additional mappings: EXTERNAL, INTERNAL."

        -- when
        local mapping = resolver.parse_mapping(message)

        -- then
        assert.are.same({
            source_type = "Model.Dir",
            source_property = "source.config.direction",
            target_type = "Model.Kind",
            target_property = "kind",
            constants = { "EXTERNAL", "INTERNAL" },
            method_name = "toKind",
            signature = "Model.Kind toKind(Model.Dir direction)",
        }, mapping)
    end)

    it("parses forged collection paths on both sides", function()
        -- given
        local message = 'The following constants from the property "Model.Dir box.parts[].dir" enum have no '
            .. 'corresponding constant in the "Model.Kind box.parts[].dir" enum and must be be mapped via adding '
            .. "additional mappings: EXTERNAL, INTERNAL."

        -- when
        local mapping = resolver.parse_mapping(message)

        -- then
        assert.are.equal("box.parts[].dir", mapping.source_property)
        assert.are.equal("box.parts[].dir", mapping.target_property)
        assert.are.equal("Model.Kind toKind(Model.Dir dir)", mapping.signature)
    end)

    it("does not claim the method-level enum diagnostic", function()
        -- given
        local message = "The following constants from the source enum have no corresponding constant in the "
            .. "target enum and must be be mapped via adding additional mappings: EXTERNAL, INTERNAL."

        -- when
        local mapping = resolver.parse_mapping(message)

        -- then
        assert.is_nil(mapping)
    end)

    it("resolves a nested source path and generates the parameter from its last property", function()
        -- given
        state.buffer_lines[1] = {
            "package ua.mapper;",
            "",
            "import org.mapstruct.Mapper;",
            "import ua.model.ChargeCalculationRequest;",
            "import ua.target.PaymentRequest;",
            "",
            "public abstract class ChargeCalculationAdapterMapper {",
            "    public abstract PaymentRequest toRequest(ChargeCalculationRequest request);",
            "}",
        }
        stub_java_tree(7, 8)

        -- when
        resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 7,
                col = 40,
                message = 'The following constants from the property "TransferDirection request.payment.direction" '
                    .. 'enum have no corresponding constant in the "TransferType transferType" enum and must be be '
                    .. "mapped via adding additional mappings: EXTERNAL.",
            },
        })

        -- then
        assert.are.equal("request.payment.direction.", path_requests[1].path_expression)
        assert.are.equal("transferType.", path_requests[2].path_expression)
        local generated = false
        for _, line in ipairs(state.buffer_lines[1]) do
            if line == "    protected abstract TransferType toTransferType(TransferDirection direction);" then
                generated = true
            end
        end
        assert.is_true(generated)
    end)
end)
