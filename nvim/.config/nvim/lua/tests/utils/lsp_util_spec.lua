local helper = require("tests.utils.spec_helper")

describe("utils.lsp-util", function()
    local lsp_util
    local state

    before_each(function()
        _, state = helper.reset_vim()
        helper.stub_module("utils.lang.lsp-common", {
            apply_lsp_action = function(action, client)
                state.applied_action = { action = action, client = client }
            end,
        })
        lsp_util = helper.reload("utils.lsp-util")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.lsp-util", "utils.lang.lsp-common" })
    end)

    it("gets clients by name while preserving extra filters", function()
        -- given
        local captured_filter
        vim.lsp.get_clients = function(filter)
            captured_filter = filter
            return { { id = 1, name = filter.name } }
        end

        -- when
        local clients = lsp_util.get_clients_by_name("jdtls", { bufnr = 9 })

        -- then
        assert.are.same({ bufnr = 9, name = "jdtls" }, captured_filter)
        assert.are.same({ { id = 1, name = "jdtls" } }, clients)
    end)

    it("returns the first matching client and its id", function()
        -- given
        vim.lsp.get_clients = function()
            return {
                { id = 11, name = "lua_ls" },
                { id = 12, name = "lua_ls" },
            }
        end

        -- when
        local client = lsp_util.get_client_by_name("lua_ls")
        local client_id = lsp_util.get_client_id_by_name("lua_ls")

        -- then
        assert.are.same({ id = 11, name = "lua_ls" }, client)
        assert.are.equal(11, client_id)
    end)

    it("returns nil client id when no matching client exists", function()
        -- given
        vim.lsp.get_clients = function()
            return {}
        end

        -- when
        local client_id = lsp_util.get_client_id_by_name("missing")

        -- then
        assert.is_nil(client_id)
    end)

    it("builds an apply code action request with a matching filter", function()
        -- given
        local action = { title = "Organize imports" }

        -- when
        lsp_util.code_action.apply("Organize")

        -- then
        assert.is_true(state.code_action_opts.apply)
        assert.is_truthy(state.code_action_opts.filter(action))
        assert.is_nil(state.code_action_opts.filter({ title = "Rename symbol" }))
    end)

    it("builds a toggle code action request accepting either action pattern", function()
        -- given
        local enable_action = { title = "Enable feature" }
        local disable_action = { title = "Disable feature" }
        local other_action = { title = "Rename symbol" }

        -- when
        lsp_util.code_action.toggle("Enable", "Disable")

        -- then
        assert.is_true(state.code_action_opts.apply)
        assert.is_truthy(state.code_action_opts.filter(enable_action))
        assert.is_truthy(state.code_action_opts.filter(disable_action))
        assert.is_nil(state.code_action_opts.filter(other_action))
    end)

    it("notifies when resolving a contextual action without attached clients", function()
        -- given
        vim.lsp.get_clients = function()
            return {}
        end

        -- when
        lsp_util.code_action.resolve_context({ "Import" })

        -- then
        assert.are.equal("No LSP clients attached", state.notifications[1].message)
        assert.are.equal(vim.log.levels.INFO, state.notifications[1].level)
    end)

    it("applies the first code action matching the highest-priority pattern", function()
        -- given
        state.current_buf = 4
        state.cursor = { 8, 0 }
        local client = { id = 22, name = "jdtls" }
        vim.lsp.get_clients = function(filter)
            assert.are.same({ bufnr = 4 }, filter)
            return { { id = 22, offset_encoding = "utf-8" } }
        end
        vim.lsp.get_client_by_id = function(client_id)
            return client_id == 22 and client or nil
        end
        vim.lsp.buf_request_all = function(bufnr, method, params, callback)
            state.request = { bufnr = bufnr, method = method, params = params }
            callback({
                [22] = {
                    result = {
                        { title = "Fix import A" },
                        { title = "Fix import B" },
                    },
                },
            })
        end

        -- when
        lsp_util.code_action.resolve_context({ "B", "A" })

        -- then
        assert.are.equal(4, state.request.bufnr)
        assert.are.equal("textDocument/codeAction", state.request.method)
        assert.are.equal(vim.lsp.protocol.CodeActionTriggerKind.Invoked, state.request.params.context.triggerKind)
        assert.are.equal("Fix import B", state.applied_action.action.title)
        assert.are.equal(client, state.applied_action.client)
    end)
end)
