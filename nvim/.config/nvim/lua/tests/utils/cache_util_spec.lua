local helper = require("tests.utils.spec_helper")

describe("utils.cache-util", function()
    before_each(function()
        helper.reset_vim()
    end)

    it("exports independent Java cache maps", function()
        -- given
        local cache = helper.reload("utils.cache-util")

        -- when
        cache.java.jdt_load_workspace_symbol_map.Symbol = { "hit" }
        cache.java.javap_results_map.ClassName = "bytecode"

        -- then
        assert.are.same({ "hit" }, cache.java.jdt_load_workspace_symbol_map.Symbol)
        assert.are.equal("bytecode", cache.java.javap_results_map.ClassName)
        assert.is_not.equal(cache.java.jdt_load_workspace_symbol_map, cache.java.javap_results_map)
    end)
end)
