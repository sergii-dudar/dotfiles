local helper = require("tests.utils.spec_helper")

describe("modules.java.dependencies-search", function()
    local dep_search
    local cache_cleared

    before_each(function()
        helper.reset_vim()
        cache_cleared = false

        helper.stub_module("utils.java.jdtls-classpath-util", {})
        helper.stub_module("modules.java.dependencies-search.jarentry", {})
        helper.stub_module("utils.ui.spinner", {
            start = function() end,
            stop = function() end,
        })
        helper.stub_module("modules.java.static-import-explorer.util", {
            clear_preferred_cache = function()
                cache_cleared = true
            end,
        })

        dep_search = helper.reload("modules.java.dependencies-search")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.dependencies-search",
            "modules.java.dependencies-search.jarentry",
            "modules.java.static-import-explorer.util",
            "utils.java.jdtls-classpath-util",
            "utils.ui.spinner",
        })
    end)

    it("keeps Maven dependency paths unchanged for coordinate matching", function()
        -- given
        local path = "/Users/me/.m2/repository/org/assertj/assertj-core/3.25.1/assertj-core-3.25.1-sources"

        -- when
        local match_path = dep_search.coord_match_path(path)

        -- then
        assert.are.equal(path, match_path)
    end)

    it("normalizes Gradle dependency group segments for coordinate matching", function()
        -- given
        local path =
            "/Users/me/.gradle/caches/modules-2/files-2.1/org.assertj/assertj-core/3.25.1/hash/assertj-core-3.25.1-sources"

        -- when
        local match_path = dep_search.coord_match_path(path)

        -- then
        assert.are.equal("org/assertj/assertj-core/3.25.1/hash/assertj-core-3.25.1-sources", match_path)
    end)

    it("resets loaded state and clears dependent preferred dependency cache", function()
        -- given
        assert.is_false(dep_search.is_loaded())

        -- when
        dep_search.reset()

        -- then
        assert.is_false(dep_search.is_loaded())
        assert.are.same({}, dep_search.get_source_dirs("main"))
        assert.are.same({}, dep_search.get_source_dirs_all("test"))
        assert.are.same({}, dep_search.get_exclude())
        assert.is_true(cache_cleared)
    end)
end)
