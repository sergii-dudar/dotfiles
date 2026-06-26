local helper = require("tests.utils.spec_helper")

describe("utils.string-util", function()
    local string_util

    before_each(function()
        helper.reset_vim()
        string_util = helper.reload("utils.string-util")
    end)

    it("detects prefixes including exact matches", function()
        -- given
        local value = "org.apache.commons.lang3.ObjectUtils"

        -- when
        local has_prefix = string_util.starts_with(value, "org.apache.commons")
        local exact_match = string_util.starts_with(value, value)
        local missing_prefix = string_util.starts_with(value, "com.apache")

        -- then
        assert.is_true(has_prefix)
        assert.is_true(exact_match)
        assert.is_false(missing_prefix)
    end)

    it("splits a qualified value at the last dot", function()
        -- given
        local value = "org.apache.commons.StringUtils"

        -- when
        local prefix, suffix = string_util.split_by_last_dot(value)

        -- then
        assert.are.equal("org.apache.commons", prefix)
        assert.are.equal("StringUtils", suffix)
    end)

    it("returns nil parts when the value has no dotted suffix", function()
        -- given
        local value = "StringUtils"

        -- when
        local prefix, suffix = string_util.split_by_last_dot(value)

        -- then
        assert.is_nil(prefix)
        assert.is_nil(suffix)
    end)

    it("splits by a plain delimiter through vim.split", function()
        -- given
        local value = "alpha.beta.gamma"

        -- when
        local parts = string_util.split(value, ".")

        -- then
        assert.are.same({ "alpha", "beta", "gamma" }, parts)
    end)

    it("checks substring containment with plain matching", function()
        -- given
        local value = "a+b*c"

        -- when
        local found_index = string_util.contains(value, "+b")
        local missing_index = string_util.contains(value, "b+d")

        -- then
        assert.are.equal(2, found_index)
        assert.is_nil(missing_index)
    end)

    it("distinguishes nil and empty strings from non-empty strings", function()
        -- given
        local nil_value = nil
        local empty_value = ""
        local non_empty_value = " "

        -- when
        local nil_result = string_util.is_not_empty(nil_value)
        local empty_result = string_util.is_not_empty(empty_value)
        local non_empty_result = string_util.is_not_empty(non_empty_value)

        -- then
        assert.is_false(nil_result)
        assert.is_false(empty_result)
        assert.is_true(non_empty_result)
    end)

    it("checks whether a string equals any item in a list", function()
        -- given
        local candidates = { "java", "lua", "rust" }

        -- when
        local found = string_util.any_eq("lua", candidates)
        local missing = string_util.any_eq("go", candidates)

        -- then
        assert.is_true(found)
        assert.is_false(missing)
    end)
end)
