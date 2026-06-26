local helper = require("tests.utils.spec_helper")

describe("modules.java.refactor.canonical", function()
    local canonical

    --- Return a no-op logger for canonical refactor tests.
    local function logger()
        return {
            debug = function() end,
            error = function() end,
            info = function() end,
            warn = function() end,
        }
    end

    before_each(function()
        helper.reset_vim()
        helper.stub_module("utils.logging-util", {
            new = logger,
        })
        helper.reload("modules.java.refactor.constants")
        canonical = helper.reload("modules.java.refactor.canonical")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.refactor.canonical",
            "modules.java.refactor.constants",
            "utils.logging-util",
        })
    end)

    it("detects the changed package prefix from a Java file move", function()
        -- given
        local changes = {
            {
                src = "/repo/service/src/test/java/ua/old/payment/FooTest.java",
                dst = "/repo/service/src/test/java/ua/new/payment/FooTest.java",
            },
        }

        -- when
        local detected = canonical.detect(changes)

        -- then
        assert.are.same({
            old_prefix = "ua/old",
            new_prefix = "ua/new",
            root = "src/test/java/",
        }, detected)
    end)

    it("corrects directory move destinations from the canonical package transformation", function()
        -- given
        local changes = {
            {
                src = "/repo/service/src/test/java/ua/old/payment",
                dst = "/repo/service/src/test/java/payment",
            },
        }
        local detected = {
            old_prefix = "ua/old",
            new_prefix = "ua/new",
            root = "src/test/java/",
        }

        -- when
        canonical.correct_destinations(changes, detected)

        -- then
        assert.are.equal("/repo/service/src/test/java/ua/new/payment", changes[1].dst)
    end)

    it("detects directory changes that are parents of the canonical old prefix", function()
        -- given
        local detected = {
            old_prefix = "ua/old/payment",
            new_prefix = "ua/new/payment",
            root = "src/test/java/",
        }
        local parent_change = { src = "/repo/service/src/test/java/ua/old", dst = "/repo/service/src/test/java/ua/new" }
        local same_change = {
            src = "/repo/service/src/test/java/ua/old/payment",
            dst = "/repo/service/src/test/java/ua/new/payment",
        }
        local file_change = {
            src = "/repo/service/src/test/java/ua/old/payment/FooTest.java",
            dst = "/repo/service/src/test/java/ua/new/payment/FooTest.java",
        }

        -- when
        local parent = canonical.is_parent_of_canonical(parent_change, detected)
        local same = canonical.is_parent_of_canonical(same_change, detected)
        local file = canonical.is_parent_of_canonical(file_change, detected)

        -- then
        assert.is_true(parent)
        assert.is_false(same)
        assert.is_false(file)
    end)
end)
