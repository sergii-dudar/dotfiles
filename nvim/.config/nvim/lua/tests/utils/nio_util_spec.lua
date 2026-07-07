local helper = require("tests.utils.spec_helper")

describe("utils.nio-util", function()
    local nio_util
    local state
    local nio

    before_each(function()
        helper.reset_vim()
        state = {}
        nio = {
            wrap = function(fn)
                return function(...)
                    local args = { ... }
                    local result
                    table.insert(args, function(value)
                        result = value
                    end)
                    fn(unpack(args))
                    return result
                end
            end,
            run = function(callback)
                state.run_callback = callback
                return "running"
            end,
        }
        helper.stub_module("nio", nio)
        _G.Snacks = {
            picker = {},
        }
        nio_util = helper.reload("utils.nio-util")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.nio-util", "nio" })
        _G.Snacks = nil
    end)

    it("wraps Snacks picker select into a coroutine-style function", function()
        -- given
        local captured
        Snacks.picker.select = function(items, opts, callback)
            captured = { items = items, opts = opts }
            callback("lua")
        end

        -- when
        local result = nio_util.select({ "lua", "java" }, "Language")

        -- then
        assert.are.equal("lua", result)
        assert.are.same({ items = { "lua", "java" }, opts = { prompt = "Language" } }, captured)
    end)

    it("wraps Snacks input into a coroutine-style function", function()
        -- given
        local captured
        Snacks.input = function(opts, callback)
            captured = opts
            callback("typed value")
        end

        -- when
        local result = nio_util.input("Expression")

        -- then
        assert.are.equal("typed value", result)
        assert.are.same({ prompt = "Expression" }, captured)
    end)

    it("returns selected multi-select items from the picker confirm callback", function()
        -- given
        local items = {
            { name = "one", id = 1 },
            { name = "two", id = 2 },
        }
        local closed = false
        Snacks.picker.pick = function(config)
            config.confirm({
                selected = function()
                    return {
                        { item = items[1] },
                        { item = items[2] },
                    }
                end,
                close = function()
                    closed = true
                end,
            })
        end

        -- when
        local result = nio_util.multi_select(items, "Pick values")

        -- then
        assert.are.same(items, result)
        assert.is_true(closed)
    end)

    it("returns nil when multi-select confirmation has no selected items", function()
        -- given
        local closed = false
        Snacks.picker.pick = function(config)
            config.confirm({
                selected = function()
                    return {}
                end,
                close = function()
                    closed = true
                end,
            })
        end

        -- when
        local result = nio_util.multi_select({ { name = "one" } }, "Pick values")

        -- then
        assert.is_nil(result)
        assert.is_true(closed)
    end)

    it("returns nil when the multi-select picker closes before confirmation", function()
        -- given
        Snacks.picker.pick = function(config)
            config.on_close()
        end

        -- when
        local result = nio_util.multi_select({ { name = "one" } }, "Pick values")

        -- then
        assert.is_nil(result)
    end)

    it("uses highlighted item chunks for multi-select display when present", function()
        -- given
        local chunks = {
            { "Add", "DiagnosticOk" },
            { " unmapped target property: " },
            { "hiddenDebtor", "Identifier" },
        }
        local rendered
        Snacks.picker.pick = function(config)
            rendered = config.format(config.items[1])
            config.on_close()
        end

        -- when
        local result = nio_util.multi_select({
            { name = "Add unmapped target property: hiddenDebtor", chunks = chunks },
        }, "Pick values")

        -- then
        assert.is_nil(result)
        assert.are.same(chunks, rendered)
    end)

    it("delegates run to nio.run", function()
        -- given
        local callback = function() end

        -- when
        local result = nio_util.run(callback)

        -- then
        assert.are.equal("running", result)
        assert.are.equal(callback, state.run_callback)
    end)
end)
