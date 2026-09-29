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
        helper.stub_module("modules.java.diagnostics-resolver.mapstruct-mapping-method", {
            resolve = function(ctx)
                dispatched = ctx
            end,
        })
        helper.stub_module("modules.java.diagnostics-resolver.mapstruct-nested-mapping-method", {
            resolve = function(ctx)
                dispatched = ctx
            end,
        })
        helper.stub_module("modules.java.diagnostics-resolver.mapstruct-nested-properties-mapping-method", {
            resolve = function(ctx)
                dispatched = ctx
            end,
        })
        helper.stub_module("modules.java.diagnostics-resolver.mapstruct-parameter-mapping-method", {
            resolve = function(ctx)
                dispatched = ctx
            end,
        })
        helper.stub_module("modules.java.diagnostics-resolver.mapstruct-enum-mapping-method", {
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
                    lnum = 2,
                    message = 'Unmapped target properties: "first, second"',
                },
            }
        end

        resolver = helper.reload("modules.java.diagnostics-resolver")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver",
            "modules.java.diagnostics-resolver.mapstruct-enum-mapping-method",
            "modules.java.diagnostics-resolver.mapstruct-mapping-method",
            "modules.java.diagnostics-resolver.mapstruct-nested-mapping-method",
            "modules.java.diagnostics-resolver.mapstruct-nested-properties-mapping-method",
            "modules.java.diagnostics-resolver.mapstruct-parameter-mapping-method",
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

    it("prefers the nested mapping resolver over the generic plural-property resolver", function()
        -- given
        vim.diagnostic.get = function(bufnr, opts)
            return {
                {
                    bufnr = bufnr,
                    lnum = 2,
                    message = 'Unmapped target properties: "merchantId, terminalId". Mapping from property '
                        .. '"CardTransferInitiation transfer" to "CardTransferDetails cardTransferDetails"',
                },
            }
        end

        -- when
        local resolved = resolver.resolve_current()

        -- then
        assert.is_true(resolved)
        assert.are.equal('Unmapped target properties: ".*"%. Mapping from property ".*" to ".*"', dispatched.pattern)
    end)

    it("dispatches the resolver for a singular unmapped target property", function()
        -- given
        vim.diagnostic.get = function(bufnr, opts)
            return {
                {
                    bufnr = bufnr,
                    lnum = 2,
                    message = 'Unmapped target property: "instructedAmount"',
                },
            }
        end

        -- when
        local resolved = resolver.resolve_current()

        -- then
        assert.is_true(resolved)
        assert.are.equal('Unmapped target property: "instructedAmount"', dispatched.diagnostic.message)
        assert.are.equal("Unmapped target property: .*", dispatched.pattern)
    end)

    it("prefers the nested mapping resolver over the generic unmapped-property resolver", function()
        -- given
        vim.diagnostic.get = function(bufnr, opts)
            return {
                {
                    bufnr = bufnr,
                    lnum = 2,
                    message = 'Unmapped target property: "identification". Mapping from property '
                        .. '"ChargeCalculationRequest.ChargeAccount debtorAccount" to "Account debtorAccount"',
                },
            }
        end

        -- when
        local resolved = resolver.resolve_current()

        -- then
        assert.is_true(resolved)
        assert.are.equal('Unmapped target property: ".*"%. Mapping from property ".*" to ".*"', dispatched.pattern)
    end)

    it("dispatches the resolver for a suggested MapStruct mapping method", function()
        -- given
        vim.diagnostic.get = function(bufnr, opts)
            return {
                {
                    bufnr = bufnr,
                    lnum = 2,
                    message = 'Can\'t map property "Duration ttl" to "long ttl". '
                        .. 'Consider to declare/implement a mapping method: "long map(Duration value)"',
                },
            }
        end

        -- when
        local resolved = resolver.resolve_current()

        -- then
        assert.is_true(resolved)
        assert.are.equal("Can't map property .*Consider to declare/implement a mapping method: .*", dispatched.pattern)
    end)

    it("dispatches the resolver for a suggested whole-parameter mapping method", function()
        -- given
        vim.diagnostic.get = function(bufnr, opts)
            return {
                {
                    bufnr = bufnr,
                    lnum = 2,
                    message = 'Can\'t map parameter "CardTransferInitiation initiation" to '
                        .. '"Set<CardTransferInitiationResponse.InitiatedTransfers> initiatedTransfers". '
                        .. "Consider to declare/implement a mapping method: "
                        .. '"Set<CardTransferInitiationResponse.InitiatedTransfers> map(CardTransferInitiation value)"',
                },
            }
        end

        -- when
        local resolved = resolver.resolve_current()

        -- then
        assert.is_true(resolved)
        assert.are.equal("Can't map parameter .*Consider to declare/implement a mapping method: .*", dispatched.pattern)
    end)

    it("dispatches the resolver for missing enum constant mappings", function()
        -- given
        vim.diagnostic.get = function(bufnr, opts)
            return {
                {
                    bufnr = bufnr,
                    lnum = 2,
                    message = 'The following constants from the property "TransferDirection direction" enum have no '
                        .. 'corresponding constant in the "TransferType transferType" enum and must be be mapped via '
                        .. "adding additional mappings: EXTERNAL, INTERNAL.",
                },
            }
        end

        -- when
        local resolved = resolver.resolve_current()

        -- then
        assert.is_true(resolved)
        assert.are.equal(
            '^The following constants from the property ".*" enum have no corresponding constant in the ".*" enum and must .-mapped via adding additional mappings: .*',
            dispatched.pattern
        )
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

    describe("forged element mappings", function()
        --- Publish one diagnostic message on the cursor line.
        ---@param message string
        local function publish(message)
            vim.diagnostic.get = function(bufnr)
                return { { bufnr = bufnr, lnum = 2, message = message } }
            end
        end

        it("prefers the nested resolver for plural properties of a collection element", function()
            -- given
            publish(
                'Unmapped target properties: "color, size". Mapping from Collection element '
                    .. '"Model.Wheel wheels" to "Model.WheelDto wheels".'
            )

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_true(resolved)
            assert.are.equal(
                'Unmapped target properties: ".*"%. Mapping from %u%a+ %a+ ".*" to ".*"',
                dispatched.pattern
            )
        end)

        it("prefers the nested resolver for a singular property of a map value", function()
            -- given
            publish(
                'Unmapped target property: "pronunciation". Mapping from Map value '
                    .. '"Model.Word wordMap{:value}" to "Model.WordDto wordMap{:value}".'
            )

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_true(resolved)
            assert.are.equal('Unmapped target property: ".*"%. Mapping from %u%a+ %a+ ".*" to ".*"', dispatched.pattern)
        end)

        it("dispatches a suggested mapping method for a stream element", function()
            -- given
            publish(
                'Can\'t map Stream element "Duration ages" to "long ages". '
                    .. 'Consider to declare/implement a mapping method: "long map(Duration value)".'
            )

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_true(resolved)
            assert.are.equal(
                "Can't map %u%a+ %a+ \".*Consider to declare/implement a mapping method: .*",
                dispatched.pattern
            )
        end)
    end)

    describe("diagnostic positions", function()
        local extmark

        before_each(function()
            extmark = { 5, 4, { end_row = 5, end_col = 7 } }
            vim.diagnostic.get = function(bufnr)
                return {
                    {
                        bufnr = bufnr,
                        lnum = 2,
                        col = 4,
                        end_lnum = 2,
                        end_col = 7,
                        namespace = 11,
                        _extmark_id = 21,
                        message = 'Unmapped target properties: "first, second"',
                    },
                }
            end
            vim.diagnostic.get_namespace = function(namespace)
                assert.are.equal(11, namespace)
                return { user_data = { location_ns = 31 } }
            end
            vim.api.nvim_buf_get_extmark_by_id = function(bufnr, namespace, id, opts)
                assert.are.equal(7, bufnr)
                assert.are.equal(31, namespace)
                assert.are.equal(21, id)
                assert.is_true(opts.details)
                return extmark
            end
        end)

        it("follows a diagnostic to the line where it is rendered", function()
            -- given
            state.cursor = { 6, 4 }

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_true(resolved)
            assert.are.equal(5, dispatched.diagnostic.lnum)
            assert.are.equal(4, dispatched.diagnostic.col)
            assert.are.equal(5, dispatched.diagnostic.end_lnum)
        end)

        it("does not resolve a diagnostic on the line it was published on", function()
            -- given
            state.cursor = { 3, 4 }

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_false(resolved)
            assert.is_nil(dispatched)
        end)

        it("skips a diagnostic whose text was deleted", function()
            -- given
            state.cursor = { 6, 4 }
            extmark = { 5, 4, { end_row = 5, end_col = 7, invalid = true } }

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_false(resolved)
            assert.is_nil(dispatched)
        end)

        it("keeps the published position when the extmark is unavailable", function()
            -- given
            state.cursor = { 3, 4 }
            vim.api.nvim_buf_get_extmark_by_id = function()
                return {}
            end

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_true(resolved)
            assert.are.equal(2, dispatched.diagnostic.lnum)
        end)

        it("resolves the diagnostic of the method containing the cursor", function()
            -- given
            state.cursor = { 5, 4 }
            local method = {
                type = function()
                    return "method_declaration"
                end,
                range = function()
                    return 4, 4, 5, 30
                end,
                parent = function()
                    return nil
                end,
            }
            vim.treesitter = {
                get_parser = function()
                    return {
                        parse = function()
                            return {
                                {
                                    root = function()
                                        return {
                                            named_descendant_for_range = function()
                                                return method
                                            end,
                                        }
                                    end,
                                },
                            }
                        end,
                    }
                end,
            }

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_true(resolved)
            assert.are.equal(5, dispatched.diagnostic.lnum)
        end)

        it("does not borrow the diagnostic of another method", function()
            -- given
            state.cursor = { 3, 4 }
            local method = {
                type = function()
                    return "method_declaration"
                end,
                range = function()
                    return 1, 4, 3, 30
                end,
                parent = function()
                    return nil
                end,
            }
            vim.treesitter = {
                get_parser = function()
                    return {
                        parse = function()
                            return {
                                {
                                    root = function()
                                        return {
                                            named_descendant_for_range = function()
                                                return method
                                            end,
                                        }
                                    end,
                                },
                            }
                        end,
                    }
                end,
            }

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_false(resolved)
            assert.is_nil(dispatched)
        end)
    end)

    describe("several supported diagnostics", function()
        local offered
        local choice

        before_each(function()
            offered = nil
            choice = 2
            vim.ui = {
                select = function(items, opts, on_choice)
                    offered = { items = items, opts = opts }
                    on_choice(choice and items[choice] or nil)
                end,
            }
            vim.diagnostic.get = function(bufnr)
                return {
                    { bufnr = bufnr, lnum = 2, col = 4, message = 'Unmapped target properties: "first, second"' },
                    { bufnr = bufnr, lnum = 2, col = 4, message = "Some other diagnostic" },
                    {
                        bufnr = bufnr,
                        lnum = 2,
                        col = 4,
                        message = 'Can\'t map property "Duration ttl" to "long ttl". '
                            .. 'Consider to declare/implement a mapping method: "long map(Duration value)"',
                    },
                }
            end
        end)

        it("offers only the supported diagnostics and resolves the chosen one", function()
            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_true(resolved)
            assert.are.equal(2, #offered.items)
            assert.are.equal('Unmapped target properties: "first, second"', offered.opts.format_item(offered.items[1]))
            assert.are.equal(
                "Can't map property .*Consider to declare/implement a mapping method: .*",
                dispatched.pattern
            )
        end)

        it("resolves nothing when the picker is cancelled", function()
            -- given
            choice = nil

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_true(resolved)
            assert.is_nil(dispatched)
        end)

        it("dispatches a diagnostic published twice without a picker", function()
            -- given
            vim.diagnostic.get = function(bufnr)
                return {
                    { bufnr = bufnr, lnum = 2, col = 4, message = 'Unmapped target properties: "first, second"' },
                    { bufnr = bufnr, lnum = 2, col = 4, message = 'Unmapped target properties: "first, second"' },
                }
            end

            -- when
            local resolved = resolver.resolve_current()

            -- then
            assert.is_true(resolved)
            assert.is_nil(offered)
            assert.are.equal("Unmapped target properties: .*", dispatched.pattern)
        end)
    end)
end)
