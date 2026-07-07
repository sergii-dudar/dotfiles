local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver", function()
    local resolver
    local state
    local dispatched

    before_each(function()
        _, state = helper.reset_vim()
        dispatched = nil

        helper.stub_module("modules.java.diagnostics-resolver.mapstruct-unmapped-target", {
            resolve = function(ctx)
                dispatched = ctx
            end,
        })

        state.current_buf = 7
        state.cursor = { 3, 4 }
        vim.diagnostic.get = function(bufnr, opts)
            return {
                {
                    bufnr = bufnr,
                    lnum = opts.lnum,
                    message = 'Unmapped target properties: "first, second"',
                },
            }
        end

        resolver = helper.reload("modules.java.diagnostics-resolver")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver",
            "modules.java.diagnostics-resolver.mapstruct-unmapped-target",
        })
    end)

    it("dispatches the first resolver matching the current-line diagnostic", function()
        -- when
        local resolved = resolver.resolve_current()

        -- then
        assert.is_true(resolved)
        assert.are.equal(7, dispatched.bufnr)
        assert.are.equal(2, dispatched.diagnostic.lnum)
        assert.are.equal("Unmapped target properties: .*", dispatched.pattern)
    end)

    it("notifies when no current-line diagnostic is supported", function()
        -- given
        vim.diagnostic.get = function()
            return { { message = "Some other diagnostic" } }
        end

        -- when
        local resolved = resolver.resolve_current()

        -- then
        assert.is_false(resolved)
        assert.are.equal("[Java Diagnostics] No supported diagnostic on current line", state.notifications[1].message)
    end)
end)
