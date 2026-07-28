local helper = require("tests.utils.spec_helper")

describe("utils.java.lombok-builder-navigation", function()
    local navigation
    local state

    --- Position the test cursor on a token within the supplied source line.
    ---@param line string
    ---@param token string
    local function set_cursor_on(line, token)
        local start_idx = assert(line:find(token, 1, true))
        state.current_line = line
        state.cursor = { 1, start_idx - 1 }
    end

    --- Configure a synthetic destination buffer and the window APIs used to jump into it.
    ---@param lines string[]
    local function configure_target_buffer(lines)
        state.valid_windows = { [state.current_win] = true }
        state.buffer_lines[2] = lines

        vim.api.nvim_buf_get_lines = function(bufnr, start_row, end_row)
            local buffer_lines = state.buffer_lines[bufnr] or {}
            local last_row = end_row == -1 and #buffer_lines or end_row
            local selected = {}
            for idx = start_row + 1, last_row do
                table.insert(selected, buffer_lines[idx])
            end
            return selected
        end
        vim.api.nvim_win_call = function(_, callback)
            callback()
        end
        vim.fn.settagstack = function(win, stack, action)
            state.tagstack = { win = win, stack = stack, action = action }
        end
        vim.fn.win_getid = function(win)
            return win
        end
    end

    before_each(function()
        _, state = helper.reset_vim()
        local client = { offset_encoding = "utf-16" }

        vim.str_utfindex = function(_, _, byte_col)
            return byte_col
        end
        vim.lsp.get_clients = function(opts)
            if opts and opts.method == "textDocument/typeDefinition" then
                return { client }
            end
            return {}
        end
        vim.lsp.get_client_by_id = function(client_id)
            return client_id == 7 and client or nil
        end
        vim.lsp.buf.definition = function(opts)
            state.definition_opts = opts
        end
        vim.lsp.buf_request_all = function(bufnr, method, params, callback)
            state.type_definition_request = {
                bufnr = bufnr,
                method = method,
                params = params(client),
            }
            state.type_definition_callback = callback
        end
        vim.lsp.util.make_text_document_params = function(bufnr)
            return { uri = "file:///buffer/" .. bufnr }
        end
        vim.lsp.util.locations_to_items = function(locations)
            local items = {}
            for _, location in ipairs(locations) do
                table.insert(items, location.item)
            end
            return items
        end
        navigation = helper.reload("utils.java.lombok-builder-navigation")
    end)

    after_each(function()
        helper.unload("utils.java.lombok-builder-navigation")
    end)

    it("handles a Lombok builder setter used as a consumer method reference", function()
        -- given
        local line = "        .map(builder::balanceBooking)"
        set_cursor_on(line, "balanceBooking")

        -- when
        local handled = navigation.goto_definition({ filetype = "java", bufnr = 1 })

        -- then
        assert.is_true(handled)
        assert.is_nil(state.definition_opts)
        assert.are.equal("textDocument/typeDefinition", state.type_definition_request.method)
        assert.are.same({
            line = 0,
            character = assert(line:find("builder", 1, true)) - 1,
        }, state.type_definition_request.params.position)
    end)

    it("preserves navigation for a setter in a Lombok builder call chain", function()
        -- given
        local lines = {
            "var request = PacsRequestContext.builder()",
            '        .source("instant")',
            "        .build();",
        }
        state.buffer_lines[1] = lines
        set_cursor_on(lines[2], "source")
        state.cursor[1] = 2

        -- when
        local handled = navigation.goto_definition({ filetype = "java", bufnr = 1 })

        -- then
        assert.is_true(handled)
        assert.is_table(state.definition_opts)
    end)

    it("resolves a builder setter invoked through a lambda parameter alias", function()
        -- given
        local line = "return b.paymentDueDateTime(paymentDueDate.toString());"
        state.buffer_lines[1] = { line }
        set_cursor_on(line, "paymentDueDateTime")
        configure_target_buffer({
            "@Value",
            "@Builder(toBuilder = true)",
            "public class TransactionInitiation {",
            "    String paymentDueDateTime;",
            "}",
        })

        navigation.goto_definition({ filetype = "java", bufnr = 1 })

        -- when
        state.definition_opts.on_list({
            items = {
                {
                    bufnr = 2,
                    filename = "/workspace/TransactionInitiation.java",
                    lnum = 2,
                    col = 1,
                },
            },
        })

        -- then
        assert.are.same({ win = state.current_win, bufnr = 2 }, state.window_set_buf)
        assert.are.same({ 4, 11 }, state.cursor)
        assert.is_table(state.tagstack)
    end)

    it("preserves normal navigation for a non-Lombok member invocation", function()
        -- given
        local line = "return evaluator.evaluate(transaction);"
        set_cursor_on(line, "evaluate")
        configure_target_buffer({
            "public class BalanceBookingEvaluator {",
            "    public Result evaluate(Transaction transaction) {",
            "    }",
            "}",
        })

        navigation.goto_definition({ filetype = "java", bufnr = 1 })

        -- when
        state.definition_opts.on_list({
            items = {
                {
                    bufnr = 2,
                    filename = "/workspace/BalanceBookingEvaluator.java",
                    lnum = 2,
                    col = 19,
                },
            },
        })

        -- then
        assert.are.same({ win = state.current_win, bufnr = 2 }, state.window_set_buf)
        assert.are.same({ 2, 18 }, state.cursor)
    end)

    it("keeps unrelated method references on the standard LSP fallback path", function()
        -- given
        local line = "        .map(evaluator::balanceBooking)"
        set_cursor_on(line, "balanceBooking")

        -- when
        local handled = navigation.goto_definition({ filetype = "java", bufnr = 1 })

        -- then
        assert.is_false(handled)
        assert.is_nil(state.definition_opts)
        assert.is_nil(state.type_definition_request)
    end)

    it("redirects a builder consumer definition from the annotation to its source field", function()
        -- given
        local line = "        .map(builder::balanceBooking)"
        set_cursor_on(line, "balanceBooking")
        state.buffer_lines[1] = { line }
        configure_target_buffer({
            "@Value",
            "@Builder(toBuilder = true)",
            "public class TransactionInitiation {",
            "    Set<BalanceBookingType> balanceBooking;",
            "}",
        })

        navigation.goto_definition({ filetype = "java", bufnr = 1 })

        -- when
        state.type_definition_callback({
            [7] = {
                result = {
                    {
                        item = {
                            bufnr = 2,
                            filename = "/workspace/TransactionInitiation.java",
                            lnum = 2,
                            col = 1,
                        },
                    },
                },
            },
        })

        -- then
        assert.are.same({ win = state.current_win, bufnr = 2 }, state.window_set_buf)
        assert.are.same({ 4, 28 }, state.cursor)
        assert.is_table(state.tagstack)
    end)
end)
