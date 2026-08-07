local helper = require("tests.utils.spec_helper")

describe("utils.java.java-import-util", function()
    local import_util
    local state
    local declared_type_names
    local root_class

    before_each(function()
        _, state = helper.reset_vim()
        declared_type_names = {}
        root_class = nil
        helper.stub_module("utils.java.java-ts-util", {
            --- Return the Java type names configured for the current test.
            declared_type_names = function()
                return declared_type_names
            end,
            --- Return the top-level Java class configured for the current test.
            get_root_class_with_abstract = function()
                return root_class
            end,
        })
        import_util = helper.reload("utils.java.java-import-util")
    end)

    after_each(function()
        helper.clear_stub_modules("utils.java.java-ts-util")
    end)

    it("detects regular imports for a class name", function()
        -- given
        state.buffer_lines[3] = {
            "package ua.example;",
            "import java.util.List;",
            "import static java.util.Collections.emptyList;",
        }

        -- when
        local list_imported = import_util.import_exists("List", 3)
        local empty_list_imported_as_regular = import_util.import_exists("emptyList", 3)

        -- then
        assert.is_true(list_imported)
        assert.is_false(empty_list_imported_as_regular)
    end)

    it("detects explicit static imports for a member name", function()
        -- given
        state.buffer_lines[3] = {
            "import static org.assertj.core.api.Assertions.assertThat;",
            "import org.assertj.core.api.Assertions;",
        }

        -- when
        local assert_that_imported = import_util.static_import_exists("assertThat", 3)
        local assertions_imported_as_static = import_util.static_import_exists("Assertions", 3)

        -- then
        assert.is_true(assert_that_imported)
        assert.is_false(assertions_imported_as_static)
    end)

    it("preserves an existing import while replacing fully qualified usages", function()
        -- given
        local fqcn = "ua.raiffeisen.payments.cardtransferinitiation.core.model.enumeration.TransferDirection"
        state.buffer_lines[0] = {
            "package ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.mapper;",
            "",
            "import " .. fqcn .. ";",
            "",
            "abstract class CardTransferInitiationApiMapper {",
            "    abstract " .. fqcn .. " toTransferDirection();",
            "}",
        }
        vim.fn.expand = function(expression)
            if expression == "<cword>" then
                return "TransferDirection"
            end
            if expression == "<cWORD>" then
                return fqcn
            end
            return ""
        end

        -- when
        import_util.import_class_and_replace()

        -- then
        assert.are.same({
            "package ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.mapper;",
            "",
            "import " .. fqcn .. ";",
            "",
            "abstract class CardTransferInitiationApiMapper {",
            "    abstract TransferDirection toTransferDirection();",
            "}",
        }, state.buffer_lines[0])
    end)

    it("replaces a fully qualified enum constant with a static import", function()
        -- given
        local fqcn = "org.mapstruct.ReportingPolicy.ERROR"
        state.buffer_lines[0] = {
            "package ua.raiffeisen.payments.cardtransferinitiation.adapter.payment.kafka.mapper;",
            "",
            "import org.mapstruct.Mapper;",
            "",
            "@Mapper(unmappedTargetPolicy = " .. fqcn .. ")",
            "abstract class EnvelopeMapper {}",
        }
        vim.fn.expand = function(expression)
            if expression == "<cword>" then
                return "ERROR"
            end
            if expression == "<cWORD>" then
                return fqcn
            end
            return ""
        end

        -- when
        import_util.import_class_and_replace()

        -- then
        assert.are.same({
            "package ua.raiffeisen.payments.cardtransferinitiation.adapter.payment.kafka.mapper;",
            "",
            "import static " .. fqcn .. ";",
            "import org.mapstruct.Mapper;",
            "",
            "@Mapper(unmappedTargetPolicy = ERROR)",
            "abstract class EnvelopeMapper {}",
        }, state.buffer_lines[0])
    end)

    it("imports a same-file nested type used in the enclosing class header", function()
        -- given
        declared_type_names.CardDetailsResolver = true
        declared_type_names.CardDetailsResolverParams = true
        root_class = {
            fqn = "ua.raiffeisen.payments.cardtransfer.core.service.CardDetailsResolver",
            is_abstract = false,
        }
        state.buffer_lines[0] = {
            "package ua.raiffeisen.payments.cardtransfer.core.service;",
            "",
            "import java.util.function.Function;",
            "import reactor.core.publisher.Mono;",
            "",
            "public class CardDetailsResolver",
            "        implements Function<CardDetailsResolver.CardDetailsResolverParams, Mono<Void>> {",
            "    public static class CardDetailsResolverParams {}",
            "}",
        }
        vim.fn.expand = function(expression)
            if expression == "<cword>" then
                return "CardDetailsResolverParams"
            end
            if expression == "<cWORD>" then
                return "CardDetailsResolver.CardDetailsResolverParams"
            end
            return ""
        end

        -- when
        import_util.import_class_and_replace()

        -- then
        assert.are.same({
            "package ua.raiffeisen.payments.cardtransfer.core.service;",
            "",
            "import ua.raiffeisen.payments.cardtransfer.core.service.CardDetailsResolver.CardDetailsResolverParams;",
            "import java.util.function.Function;",
            "import reactor.core.publisher.Mono;",
            "",
            "public class CardDetailsResolver",
            "        implements Function<CardDetailsResolverParams, Mono<Void>> {",
            "    public static class CardDetailsResolverParams {}",
            "}",
        }, state.buffer_lines[0])
    end)
end)
