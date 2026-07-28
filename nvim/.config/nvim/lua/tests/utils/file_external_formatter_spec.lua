local helper = require("tests.utils.spec_helper")

describe("utils.file-external-formatter", function()
    local formatter
    local state
    local changedtick

    before_each(function()
        _, state = helper.reset_vim()
        state.current_buf = 7
        state.loaded_buffers[7] = true
        state.buffer_options[7] = {
            filetype = "json",
            modifiable = true,
        }
        state.buffer_lines[7] = { '{"name":"Ada"}' }
        changedtick = 10
        vim.api.nvim_buf_get_changedtick = function()
            return changedtick
        end
        vim.fn.executable = function()
            return 1
        end
        formatter = helper.reload("utils.file-external-formatter")
    end)

    it("formats JSON with jq", function()
        vim.system = function(command, opts, callback)
            assert.are.same({ "jq", "." }, command)
            assert.are.equal('{"name":"Ada"}', opts.stdin)
            assert.is_true(opts.text)
            callback({
                code = 0,
                stdout = '{\n  "name": "Ada"\n}\n',
                stderr = "",
            })
            return {}
        end

        formatter.format_current_buffer()

        assert.are.same({
            "{",
            '  "name": "Ada"',
            "}",
        }, state.buffer_lines[7])
        assert.matches("Formatted buffer with jq", state.notifications[#state.notifications].message)
    end)

    it("formats JSONC with Prettier", function()
        state.buffer_options[7].filetype = "jsonc"
        state.buffer_lines[7] = {
            "{",
            "// owner",
            '"name":"Ada",',
            "}",
        }
        vim.system = function(command, opts, callback)
            assert.are.same({ "prettier", "--parser", "jsonc" }, command)
            assert.are.equal('{\n// owner\n"name":"Ada",\n}', opts.stdin)
            callback({
                code = 0,
                stdout = '{\n  // owner\n  "name": "Ada",\n}\n',
                stderr = "",
            })
            return {}
        end

        formatter.format_current_buffer()

        assert.are.same({
            "{",
            "  // owner",
            '  "name": "Ada",',
            "}",
        }, state.buffer_lines[7])
        assert.matches("Formatted buffer with prettier", state.notifications[#state.notifications].message)
    end)

    it("formats XML with xmllint", function()
        state.buffer_options[7].filetype = "xml"
        state.buffer_lines[7] = { "<root><item>value</item></root>" }
        vim.system = function(command, opts, callback)
            assert.are.same({ "xmllint", "--format", "-" }, command)
            assert.are.equal("<root><item>value</item></root>", opts.stdin)
            assert.are.same({ XMLLINT_INDENT = string.rep(" ", 4) }, opts.env)
            callback({
                code = 0,
                stdout = '<?xml version="1.0"?>\n<root>\n    <item>value</item>\n</root>\n',
                stderr = "",
            })
            return {}
        end

        formatter.format_current_buffer()

        assert.are.same({
            '<?xml version="1.0"?>',
            "<root>",
            "    <item>value</item>",
            "</root>",
        }, state.buffer_lines[7])
    end)

    it("keeps malformed content when the formatter fails", function()
        vim.system = function(_, _, callback)
            callback({
                code = 4,
                stdout = "",
                stderr = "parse error: Invalid numeric literal at line 1",
            })
            return {}
        end

        formatter.format_current_buffer()

        assert.are.same({ '{"name":"Ada"}' }, state.buffer_lines[7])
        local notification = state.notifications[#state.notifications]
        assert.are.equal(vim.log.levels.ERROR, notification.level)
        assert.matches("jq failed", notification.message)
        assert.matches("parse error", notification.message)
    end)

    it("does not overwrite edits made while the formatter is running", function()
        local complete
        vim.system = function(_, _, callback)
            complete = callback
            return {}
        end

        formatter.format_current_buffer()
        changedtick = 11
        state.buffer_lines[7] = { '{"name":"Grace"}' }
        complete({
            code = 0,
            stdout = '{\n  "name": "Ada"\n}\n',
            stderr = "",
        })

        assert.are.same({ '{"name":"Grace"}' }, state.buffer_lines[7])
        local notification = state.notifications[#state.notifications]
        assert.are.equal(vim.log.levels.WARN, notification.level)
        assert.matches("buffer changed", notification.message)
    end)

    it("reports unsupported filetypes without starting a process", function()
        state.buffer_options[7].filetype = "yaml"
        local started = false
        vim.system = function()
            started = true
        end

        formatter.format_current_buffer()

        assert.is_false(started)
        local notification = state.notifications[#state.notifications]
        assert.are.equal(vim.log.levels.WARN, notification.level)
        assert.matches("filetype 'yaml'", notification.message)
        assert.matches("json, jsonc, xml", notification.message)
    end)

    it("reports a missing formatter executable", function()
        vim.fn.executable = function()
            return 0
        end

        formatter.format_current_buffer()

        local notification = state.notifications[#state.notifications]
        assert.are.equal(vim.log.levels.ERROR, notification.level)
        assert.matches("not executable: jq", notification.message)
    end)
end)
