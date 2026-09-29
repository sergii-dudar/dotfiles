local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-diagnostic-reference", function()
    local reference

    before_each(function()
        helper.reset_vim()
        reference = helper.reload("modules.java.diagnostics-resolver.mapstruct-diagnostic-reference")
    end)

    after_each(function()
        helper.clear_stub_modules({ "modules.java.diagnostics-resolver.mapstruct-diagnostic-reference" })
    end)

    it("parses a plain typed property", function()
        -- when
        local type_name, path = reference.parse_typed_reference("Account debtorAccount")

        -- then
        assert.are.equal("Account", type_name)
        assert.are.equal("debtorAccount", path)
    end)

    it("parses a nested source path that starts with the parameter name", function()
        -- when
        local type_name, path = reference.parse_typed_reference("Duration source.config.ttl")

        -- then
        assert.are.equal("Duration", type_name)
        assert.are.equal("source.config.ttl", path)
    end)

    it("parses forged collection and map paths", function()
        -- when
        local collection_type, collection_path = reference.parse_typed_reference("Duration box.parts[].age")
        local map_type, map_path = reference.parse_typed_reference("Model.Word wordMap{:key}")

        -- then
        assert.are.equal("Duration", collection_type)
        assert.are.equal("box.parts[].age", collection_path)
        assert.are.equal("Model.Word", map_type)
        assert.are.equal("wordMap{:key}", map_path)
    end)

    it("keeps generic types with spaces intact", function()
        -- when
        local type_name, path = reference.parse_typed_reference("Map<String, Foo> attributes")

        -- then
        assert.are.equal("Map<String, Foo>", type_name)
        assert.are.equal("attributes", path)
    end)

    it("rejects a fragment without a type", function()
        -- when
        local type_name, path = reference.parse_typed_reference("debtorAccount")

        -- then
        assert.is_nil(type_name)
        assert.is_nil(path)
    end)

    it("names a path after its last property", function()
        -- then
        assert.are.equal("ttl", reference.path_name("ttl"))
        assert.are.equal("ttl", reference.path_name("source.config.ttl"))
        assert.are.equal("age", reference.path_name("box.parts[].age"))
        assert.are.equal("wheels", reference.path_name("car.wheels[]"))
        assert.are.equal("wordMap", reference.path_name("wordMap{:key}"))
        assert.are.equal("age", reference.path_name("box.byName{:value}.age"))
    end)

    it("names a collection element after its type", function()
        -- then
        assert.are.equal("wheel", reference.element_name("Model.Wheel"))
        assert.are.equal("transferDirection", reference.element_name("TransferDirection"))
        assert.are.equal("value", reference.element_name("Default"))
    end)

    it("builds the generated method name from the target type", function()
        -- then
        assert.are.equal("toAccount", reference.method_name_for("Account"))
        assert.are.equal("toWheelDto", reference.method_name_for("Model.WheelDto"))
        assert.are.equal("toLong", reference.method_name_for("long"))
    end)

    it("hides the default property kind", function()
        -- then
        assert.is_nil(reference.element_kind("property"))
        assert.is_nil(reference.element_kind(nil))
        assert.are.equal("Collection element", reference.element_kind("Collection element"))
        assert.is_true(reference.is_element_kind("Map value"))
        assert.is_false(reference.is_element_kind("property"))
        assert.is_false(reference.is_element_kind("parameter"))
    end)

    it("leaves plain and dotted paths untouched for the backend", function()
        -- then
        assert.are.equal("ttl", (reference.backend_path("ttl")))
        assert.are.equal("source.config.ttl", (reference.backend_path("source.config.ttl")))
    end)

    it("translates collection markers into the backend element accessor", function()
        -- then
        assert.are.equal("box.parts.first.age", (reference.backend_path("box.parts[].age")))
        assert.are.equal("car.wheels.first", (reference.backend_path("car.wheels", "Collection element")))
        assert.are.equal("items.first", (reference.backend_path("items", "Stream element")))
    end)

    it("explains an element type the backend lost", function()
        -- given
        local object = { className = "java.lang.Object" }

        -- then
        assert.are.equal(
            "MapStruct backend could not determine the element type of 'car.spare' "
                .. "(only List and array elements are resolved)",
            reference.unresolved_element(object, "car.spare.first.")
        )
        assert.is_nil(reference.unresolved_element(object, "car.payload."))
        assert.is_nil(reference.unresolved_element({ className = "fx.Wheel" }, "car.wheels.first."))
        assert.is_nil(reference.unresolved_element(nil, "car.wheels.first."))
    end)

    it("reports map key and value paths as unsupported", function()
        -- when
        local by_kind, kind_error = reference.backend_path("wordMap{:key}", "Map key")
        local by_path, path_error = reference.backend_path("box.byName{:value}.age")

        -- then
        assert.is_nil(by_kind)
        assert.matches("not supported yet", kind_error, nil, true)
        assert.is_nil(by_path)
        assert.matches("not supported yet", path_error, nil, true)
    end)
end)
