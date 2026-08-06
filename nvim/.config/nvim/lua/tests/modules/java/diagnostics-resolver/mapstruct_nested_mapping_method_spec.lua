local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-nested-mapping-method", function()
    local resolver
    local state
    local path_requests

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

    before_each(function()
        _, state = helper.reset_vim()
        path_requests = {}
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
            "modules.java.mapstruct",
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
end)
