local helper = require("tests.utils.spec_helper")

describe("utils.resource-cwd-resolver", function()
    local resolver

    before_each(function()
        helper.reset_vim()
        resolver = helper.reload("utils.resource-cwd-resolver")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.resource-cwd-resolver", "utils.java.java-common" })
    end)

    it("returns nil when the current filetype has no resolver", function()
        -- given
        vim.bo[3].filetype = "markdown"

        -- when
        local result = resolver.resolve(3)

        -- then
        assert.is_nil(result)
    end)

    it("resolves directories through a custom registered filetype resolver", function()
        -- given
        vim.bo[8].filetype = "custom"
        resolver.register("custom", function(bufnr)
            return {
                dirs = { "/repo/custom/resources" },
                title = "Custom " .. bufnr,
            }
        end)

        -- when
        local result = resolver.resolve(8)

        -- then
        assert.are.same({
            dirs = { "/repo/custom/resources" },
            title = "Custom 8",
        }, result)
    end)

    it("uses the current buffer when resolve is called without an explicit buffer", function()
        -- given
        local _, state = helper.reset_vim()
        resolver = helper.reload("utils.resource-cwd-resolver")
        state.current_buf = 6
        vim.bo[6].filetype = "custom"
        resolver.register("custom", function(bufnr)
            return { dirs = { "/repo/" .. bufnr }, title = "Current" }
        end)

        -- when
        local result = resolver.resolve()

        -- then
        assert.are.same({ dirs = { "/repo/6" }, title = "Current" }, result)
    end)

    it("returns test and main Java resource directories for test files", function()
        -- given
        helper.stub_module("utils.java.java-common", {
            get_buffer_project_path = function(bufnr)
                return bufnr == 4 and "/repo/payment-service" or nil
            end,
        })
        vim.bo[4].filetype = "java"
        vim.api.nvim_buf_get_name = function()
            return "/repo/payment-service/src/test/java/FooTest.java"
        end
        vim.uv.fs_stat = function(path)
            return ({
                ["/repo/payment-service/src/test/resources"] = true,
                ["/repo/payment-service/src/main/resources"] = true,
            })[path]
        end

        -- when
        local result = resolver.resolve(4)

        -- then
        assert.are.same({
            dirs = {
                "/repo/payment-service/src/test/resources",
                "/repo/payment-service/src/main/resources",
            },
            title = "Resources [payment-service] (test+main)",
        }, result)
    end)

    it("returns only main Java resources for production files", function()
        -- given
        helper.stub_module("utils.java.java-common", {
            get_buffer_project_path = function()
                return "/repo/payment-service"
            end,
        })
        vim.bo[4].filetype = "java"
        vim.api.nvim_buf_get_name = function()
            return "/repo/payment-service/src/main/java/Foo.java"
        end
        vim.uv.fs_stat = function(path)
            return path == "/repo/payment-service/src/main/resources" and true or nil
        end

        -- when
        local result = resolver.resolve(4)

        -- then
        assert.are.same({
            dirs = { "/repo/payment-service/src/main/resources" },
            title = "Resources [payment-service] (main)",
        }, result)
    end)

    it("returns nil for Java buffers without an existing resource directory", function()
        -- given
        helper.stub_module("utils.java.java-common", {
            get_buffer_project_path = function()
                return "/repo/payment-service"
            end,
        })
        vim.bo[4].filetype = "java"
        vim.api.nvim_buf_get_name = function()
            return "/repo/payment-service/src/main/java/Foo.java"
        end
        vim.uv.fs_stat = function()
            return nil
        end

        -- when
        local result = resolver.resolve(4)

        -- then
        assert.is_nil(result)
    end)
end)
