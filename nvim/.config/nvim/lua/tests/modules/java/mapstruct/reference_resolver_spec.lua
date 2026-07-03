local helper = require("tests.utils.spec_helper")

-- Unit coverage for the PURE (no LSP / no tree-sitter) matching helpers of the
-- MapStruct reference resolver. These functions encode the exact target-vs-sink
-- precision that keeps `gr` on `Person.name` from dragging in `Country.name` /
-- `Department.name`, so pinning their behaviour guards against that regression.
-- The tree-sitter-backed functions need a real parser + buffers and are exercised
-- via the live editor, not here.
describe("modules.java.mapstruct.reference_resolver", function()
    local resolver

    before_each(function()
        helper.reset_vim()
        resolver = helper.reload("modules.java.mapstruct.reference_resolver")
    end)

    after_each(function()
        helper.clear_stub_modules("modules.java.mapstruct.reference_resolver")
    end)

    describe("bean_property", function()
        it("decapitalizes a plain setter", function()
            assert.are.equal("name", resolver.bean_property("setName"))
            assert.are.equal("detailName", resolver.bean_property("setDetailName"))
        end)

        it("keeps acronym setters with two leading capitals intact", function()
            assert.are.equal("IBAN", resolver.bean_property("setIBAN"))
            assert.are.equal("SPerson", resolver.bean_property("setSPerson"))
        end)

        it("passes builder / field identifiers through unchanged", function()
            assert.are.equal("firstProductName", resolver.bean_property("firstProductName"))
            assert.are.equal("iban", resolver.bean_property("iban"))
        end)
    end)

    describe("extract_sink_property", function()
        it("reads the leftmost var.setter( sink", function()
            assert.are.equal("name", resolver.extract_sink_property("        dto.setName( person.getName() );"))
        end)

        it("reads a builder-style sink", function()
            assert.are.equal(
                "firstProductName",
                resolver.extract_sink_property("        target.firstProductName( helper( person ) );")
            )
        end)

        it("reads a direct field assignment sink", function()
            assert.are.equal("iban", resolver.extract_sink_property("        target.iban = source.getIban();"))
        end)

        it("ignores statement keywords as the sink var", function()
            assert.is_nil(resolver.extract_sink_property("        return person.getName();"))
            assert.is_nil(resolver.extract_sink_property("        this.value = x;"))
        end)

        it("does not treat an equality comparison as an assignment sink", function()
            assert.is_nil(resolver.extract_sink_property("        obj.field == other.field"))
        end)

        it("returns nil for lines that are not sinks", function()
            assert.is_nil(resolver.extract_sink_property("        int total = 0;"))
        end)
    end)

    describe("extract_value_constant", function()
        it("returns the enum constant at the reference column of a switch arm", function()
            local line = "        case NEW:"
            local col = line:find("NEW") - 1
            assert.are.equal("NEW", resolver.extract_value_constant(line, col))
        end)

        it("returns nil for non-case lines", function()
            assert.is_nil(resolver.extract_value_constant("        dto.setName( x );", 8))
        end)
    end)

    describe("target_matches", function()
        it("matches an exact single-segment target", function()
            assert.is_true(resolver.target_matches("name", "name"))
        end)

        it("matches on the first segment (top-level setter feeding a nested target)", function()
            assert.is_true(resolver.target_matches("person.address", "person"))
        end)

        it("matches on the last segment (nested-leaf setter)", function()
            assert.is_true(resolver.target_matches("item.details.detailName", "detailName"))
        end)

        it("does not match an unrelated target — the crux of the type-precision fix", function()
            -- `gr` on `name` must NOT surface `@Mapping(target = "fullName", source = "name")`.
            assert.is_false(resolver.target_matches("fullName", "name"))
            assert.is_false(resolver.target_matches("address.country.countryName", "name"))
        end)
    end)

    describe("property_sibling_names", function()
        it("derives the field + JavaBeans accessors from a bare property", function()
            local names = resolver.property_sibling_names("name")
            assert.is_true(names["name"])
            assert.is_true(names["getName"])
            assert.is_true(names["setName"])
            assert.is_true(names["isName"])
            assert.is_nil(names["fullName"])
        end)

        it("derives siblings when invoked on a getter", function()
            local names = resolver.property_sibling_names("getFullName")
            assert.is_true(names["getFullName"])
            assert.is_true(names["fullName"])
            assert.is_true(names["setFullName"])
            assert.is_true(names["isFullName"])
        end)

        it("always includes the symbol under the cursor verbatim", function()
            local names = resolver.property_sibling_names("isActive")
            assert.is_true(names["isActive"])
            assert.is_true(names["active"])
            assert.is_true(names["getActive"])
            assert.is_true(names["setActive"])
        end)
    end)
end)
