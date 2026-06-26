local helper = require("tests.utils.spec_helper")

describe("utils.list-util", function()
    local list_util

    before_each(function()
        helper.reset_vim()
        list_util = helper.reload("utils.list-util")
    end)

    it("finds the first item matching a predicate", function()
        -- given
        local values = {
            { name = "first", enabled = false },
            { name = "second", enabled = true },
            { name = "third", enabled = true },
        }

        -- when
        local found = list_util.findFirst(values, function(item)
            return item.enabled
        end)

        -- then
        assert.are.same({ name = "second", enabled = true }, found)
    end)

    it("returns nil when no item matches a predicate", function()
        -- given
        local values = { 1, 3, 5 }

        -- when
        local found = list_util.findFirst(values, function(item)
            return item % 2 == 0
        end)

        -- then
        assert.is_nil(found)
    end)

    it("collects all items matching a predicate in original order", function()
        -- given
        local values = { 1, 2, 3, 4, 5 }

        -- when
        local found = list_util.findAll(values, function(item)
            return item > 2
        end)

        -- then
        assert.are.same({ 3, 4, 5 }, found)
    end)

    it("checks whether a target matches any Lua pattern from a list", function()
        -- given
        local patterns = { "%.java$", "%.lua$" }

        -- when
        local found = list_util.any_match("init.lua", patterns)
        local missing = list_util.any_match("README.md", patterns)

        -- then
        assert.is_true(found)
        assert.is_false(missing)
    end)

    it("finds an item by key-value equality", function()
        -- given
        local values = {
            { id = 1, name = "one" },
            { id = 2, name = "two" },
        }

        -- when
        local found = list_util.find_by(values, "id", 2)
        local missing = list_util.find_by(values, "id", 3)

        -- then
        assert.are.same({ id = 2, name = "two" }, found)
        assert.is_nil(missing)
    end)

    it("deduplicates a list while preserving first-seen order", function()
        -- given
        local values = { "java", "lua", "java", "rust", "lua" }

        -- when
        local unique = list_util.to_unique_list(values)

        -- then
        assert.are.same({ "java", "lua", "rust" }, unique)
    end)
end)
