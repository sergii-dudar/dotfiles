local helper = require("tests.utils.spec_helper")

describe("utils.dap-util", function()
    local dap_util
    local state

    before_each(function()
        _, state = helper.reset_vim()
        dap_util = helper.reload("utils.dap-util")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "utils.dap-util",
            "dapui",
            "dap",
            "utils.buffer-util",
            "utils.resource-cwd-resolver",
        })
        _G.Snacks = nil
    end)

    it("warns when the DAP console is not available", function()
        -- given
        helper.clear_stub_modules("dapui")

        -- when
        dap_util.show_logs()

        -- then
        assert.are.equal("DAP console is empty or not initialized", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("warns when the DAP console buffer is empty", function()
        -- given
        helper.stub_module("dapui", {
            elements = {
                console = {
                    buffer = function()
                        return 3
                    end,
                },
            },
        })
        state.buffer_lines[3] = { "" }

        -- when
        dap_util.show_logs()

        -- then
        assert.are.equal("DAP console is empty or not initialized", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("opens DAP console lines in a scratch split", function()
        -- given
        local opened
        helper.stub_module("dapui", {
            elements = {
                console = {
                    buffer = function()
                        return 3
                    end,
                },
            },
        })
        helper.stub_module("utils.buffer-util", {
            open_scratch_split = function(bufnr, opts)
                opened = { bufnr = bufnr, opts = opts }
            end,
        })
        state.buffer_lines[3] = { "first", "second" }

        -- when
        dap_util.show_logs()

        -- then
        assert.are.same({ "first", "second" }, state.buffer_lines[501])
        assert.are.same({ bufnr = 501, opts = { max_height = 10 } }, opened)
    end)

    it("resets the existing DAP log split and buffer", function()
        -- given
        helper.stub_module("dapui", {
            elements = {
                console = {
                    buffer = function()
                        return 3
                    end,
                },
            },
        })
        helper.stub_module("utils.buffer-util", {
            open_scratch_split = function() end,
        })
        state.buffer_lines[3] = { "line" }
        dap_util.show_logs()
        state.windows = { 10 }
        state.window_buffers = { [10] = 501 }

        -- when
        dap_util.reset()

        -- then
        assert.are.same({ win = 10, force = true }, state.closed_win)
        assert.are.same({ bufnr = 501, opts = { force = true } }, state.deleted_buffer)
    end)

    it("warns when evaluating an empty visual selection to a file", function()
        -- given
        state.registers.z = ""

        -- when
        dap_util.selection_eval_to_file()

        -- then
        assert.are.equal("No selection", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("warns when writing an empty visual selection directly to a file", function()
        -- given
        state.registers.z = ""

        -- when
        dap_util.selection_to_file()

        -- then
        assert.are.equal("No selection", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("warns when popup evaluation has no expression under cursor", function()
        -- given
        vim.fn.expand = function()
            return ""
        end

        -- when
        dap_util.eval_popup()

        -- then
        assert.are.equal("No expression under cursor", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("evaluates prompted input and writes the result through the Snacks explorer action", function()
        -- given
        vim.fn.expand = function(expr)
            return expr == "<cword>" and "defaultExpr" or ""
        end
        helper.stub_module("dap", {
            session = function()
                return {
                    config = { type = "java" },
                    current_frame = { id = 7 },
                    request = function(_, method, params, callback)
                        state.dap_request = { method = method, params = params }
                        callback(nil, { result = '"one\\ntwo"' })
                    end,
                }
            end,
        })
        helper.stub_module("utils.resource-cwd-resolver", {
            resolve = function()
                return { dirs = { "/repo/src/test/resources" }, title = "Resources" }
            end,
        })
        _G.Snacks = {
            input = {
                input = function(opts, callback)
                    state.input_opts = opts
                    callback("customExpr")
                end,
            },
            picker = {
                explorer = function(config)
                    state.explorer_config = config
                    config.actions.write_to_file({
                        close = function()
                            state.picker_closed = true
                        end,
                    }, { file = "/repo/src/test/resources/out.txt" })
                end,
            },
        }

        -- when
        dap_util.eval_to_file()

        -- then
        assert.are.same({ prompt = "Expression: ", default = "defaultExpr" }, state.input_opts)
        assert.are.equal("evaluate", state.dap_request.method)
        assert.are.same({
            expression = "customExpr",
            frameId = 7,
            context = "repl",
        }, state.dap_request.params)
        assert.are.equal("/repo/src/test/resources", state.explorer_config.cwd)
        assert.is_true(state.picker_closed)
        assert.are.same({
            {
                lines = { "one", "two" },
                path = "/repo/src/test/resources/out.txt",
            },
        }, state.writes)
        assert.are.equal("/repo/src/test/resources/out.txt", state.notifications[1].message)
        assert.are.equal("Written to /repo/src/test/resources/out.txt", state.notifications[2].message)
    end)

    it("evaluates the expression under cursor and opens the result in a wrapped float", function()
        -- given
        vim.fn.expand = function(expr)
            return expr == "<cexpr>" and "payload" or ""
        end
        helper.stub_module("dap", {
            session = function()
                return {
                    config = { type = "codelldb" },
                    current_frame = { id = 77 },
                    request = function(_, method, params, callback)
                        state.dap_request = { method = method, params = params }
                        callback(nil, { result = '"hello\\nworld"' })
                    end,
                }
            end,
        })

        -- when
        dap_util.eval_popup()

        -- then
        assert.are.equal("evaluate", state.dap_request.method)
        assert.are.same({
            expression = "payload",
            frameId = 77,
            context = "watch",
        }, state.dap_request.params)
        assert.are.same({ "hello", "world" }, state.buffer_lines[501])
        assert.are.equal("dap-eval", vim.bo[501].filetype)
        assert.is_true(vim.wo[900].wrap)
        assert.are.equal("q", state.keymaps[1].lhs)
        assert.are.equal("<Esc>", state.keymaps[2].lhs)
    end)
end)
