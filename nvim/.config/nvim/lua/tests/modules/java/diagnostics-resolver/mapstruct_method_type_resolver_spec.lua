local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-method-type-resolver", function()
    local resolver
    local path_requests
    local target_result

    before_each(function()
        helper.reset_vim()
        path_requests = {}
        target_result = { className = "long", simpleName = "long", packageName = "java.lang" }

        helper.stub_module("modules.java.mapstruct", {
            get_method_types = function(_, callback)
                callback({
                    sources = {
                        { name = "first", type = "example.First" },
                        { name = "second", type = "example.Second" },
                    },
                    target_type = "example.Target",
                })
            end,
            resolve_path_type = function(params, callback)
                path_requests[#path_requests + 1] = vim.deepcopy(params)
                local source_name = params.sources[1].name
                if source_name == "$target" then
                    callback(target_result)
                elseif source_name == "first" then
                    callback({ className = "java.lang.String", simpleName = "String", packageName = "java.lang" })
                else
                    callback({ className = "java.time.Duration", simpleName = "Duration", packageName = "java.time" })
                end
            end,
        })

        resolver = helper.reload("modules.java.diagnostics-resolver.mapstruct-method-type-resolver")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver.java-import-resolver",
            "modules.java.diagnostics-resolver.mapstruct-method-type-resolver",
            "modules.java.mapstruct",
        })
    end)

    it("finds a matching source parameter and resolves the target property", function()
        -- given
        local result, resolution_error = nil, nil

        -- when
        resolver.resolve({ bufnr = 1, diagnostic = { lnum = 4, col = 20 } }, {
            source_type = "Duration",
            source_property = "ttl",
            target_type = "long",
            target_property = "ttl",
        }, function(value, err)
            result = value
            resolution_error = err
        end)

        -- then
        assert.is_nil(resolution_error)
        assert.are.equal("java.time.Duration", result.source.className)
        assert.are.equal("long", result.target.className)
        assert.are.equal(3, #path_requests)
        assert.are.equal("first", path_requests[1].sources[1].name)
        assert.are.equal("second", path_requests[2].sources[1].name)
        assert.are.equal("$target", path_requests[3].sources[1].name)
    end)

    it("rejects a backend target type that disagrees with the diagnostic", function()
        -- given
        target_result = { className = "java.lang.String", simpleName = "String", packageName = "java.lang" }
        local result, resolution_error = nil, nil

        -- when
        resolver.resolve({ bufnr = 1, diagnostic = { lnum = 4, col = 20 } }, {
            source_type = "Duration",
            source_property = "ttl",
            target_type = "long",
            target_property = "ttl",
        }, function(value, err)
            result = value
            resolution_error = err
        end)

        -- then
        assert.is_nil(result)
        assert.matches("does not match diagnostic type 'long'", resolution_error, nil, true)
    end)
end)
