local helper = require("tests.utils.spec_helper")

describe("utils.common-util", function()
    local common_util
    local state

    before_each(function()
        _, state = helper.reset_vim()
        common_util = helper.reload("utils.common-util")
    end)

    it("returns the current line under the cursor", function()
        -- given
        state.current_line = "local value = 42"

        -- when
        local line = common_util.get_line_under_cursor()

        -- then
        assert.are.equal("local value = 42", line)
    end)

    it("extracts the token under the cursor with default whitespace separators", function()
        -- given
        state.current_line = "alpha beta gamma"
        state.cursor = { 1, 8 }

        -- when
        local token = common_util.get_token_under_cursor()

        -- then
        assert.are.equal("beta", token)
    end)

    it("extracts the token under the cursor with custom left and right separators", function()
        -- given
        state.current_line = "call(foo.bar)"
        state.cursor = { 1, 6 }

        -- when
        local token = common_util.get_token_under_cursor_sides("[%(%s]", "[%)%s]")

        -- then
        assert.are.equal("foo.bar", token)
    end)

    it("returns and clears the visual selection register", function()
        -- given
        state.registers.v = "selected text"

        -- when
        local selection = common_util.get_visual_selection()

        -- then
        assert.are.equal("selected text", selection)
        assert.are.same({}, state.registers.v)
        assert.are.equal('noau normal! "vy"', state.commands[1])
    end)

    it("joins buffer lines into a single text block", function()
        -- given
        state.buffer_lines[12] = { "one", "two", "three" }

        -- when
        local text = common_util.get_buffer_text(12)

        -- then
        assert.are.equal("one\ntwo\nthree", text)
    end)

    it("reads text from the current buffer", function()
        -- given
        state.current_buf = 7
        state.buffer_lines[7] = { "current", "buffer" }

        -- when
        local text = common_util.get_current_buffer_text()

        -- then
        assert.are.equal("current\nbuffer", text)
    end)

    it("finds a filename with line number when the cursor word is a file", function()
        -- given
        local expansions = {
            ["<cfile>"] = "Example.java",
            ["<cWORD>"] = "Example.java:42",
        }
        vim.fn.expand = function(expr)
            return expansions[expr]
        end

        -- when
        local file = common_util.get_file_with_line()

        -- then
        assert.are.equal("Example.java:42", file)
    end)

    it("finds a filename with line number when the cursor word is the line number", function()
        -- given
        local expansions = {
            ["<cfile>"] = "42",
            ["<cWORD>"] = "Example.java:42",
        }
        vim.fn.expand = function(expr)
            return expansions[expr]
        end

        -- when
        local file = common_util.get_file_with_line()

        -- then
        assert.are.equal("Example.java:42", file)
    end)

    it("returns the current file token without extension", function()
        -- given
        local expansions = {
            ["<cfile>"] = "Example.java",
            ["<cWORD>"] = "Example.java:42",
        }
        vim.fn.expand = function(expr)
            return expansions[expr]
        end

        -- when
        local file = common_util.get_file_with_no_ext()

        -- then
        assert.are.equal("Example", file)
    end)

    it("strips ANSI escape sequences but preserves nil", function()
        -- given
        local colored = "\27[31merror\27[0m plain"

        -- when
        local stripped = common_util.strip_ansi(colored)
        local nil_result = common_util.strip_ansi(nil)

        -- then
        assert.are.equal("error plain", stripped)
        assert.is_nil(nil_result)
    end)

    it("checks readable files and directories through vim.fn", function()
        -- given
        vim.fn.filereadable = function(path)
            return path == "/tmp/file" and 1 or 0
        end
        vim.fn.isdirectory = function(path)
            return path == "/tmp/dir" and 1 or 0
        end

        -- when
        local file_exists = common_util.is_file_exists("/tmp/file")
        local is_file = common_util.is_file("/tmp/file")
        local is_dir = common_util.is_dir("/tmp/dir")
        local missing_file = common_util.is_file("/tmp/missing")

        -- then
        assert.is_true(file_exists)
        assert.is_true(is_file)
        assert.is_true(is_dir)
        assert.is_false(missing_file)
    end)

    it("closes only valid windows", function()
        -- given
        state.valid_windows = { [101] = true }

        -- when
        common_util.close_window_if_exists(101)
        common_util.close_window_if_exists(202)

        -- then
        assert.are.same({ win = 101, force = true }, state.closed_win)
    end)

    it("loads optional modules from a file path and returns an empty table on load failures", function()
        -- given
        local original_loadfile = _G.loadfile
        _G.loadfile = function(path)
            if path == "/tmp/valid.lua" then
                return function()
                    return { enabled = true }
                end
            end
            if path == "/tmp/runtime-error.lua" then
                return function()
                    error("boom")
                end
            end
            return nil, "missing"
        end

        -- when
        local loaded = common_util.load_optional_module("/tmp/valid.lua")
        local missing = common_util.load_optional_module("/tmp/missing.lua")
        local runtime_error = common_util.load_optional_module("/tmp/runtime-error.lua")
        _G.loadfile = original_loadfile

        -- then
        assert.are.same({ enabled = true }, loaded)
        assert.are.same({}, missing)
        assert.are.same({}, runtime_error)
    end)

    it("opens file URLs through vim.cmd.edit after URI conversion and escaping", function()
        -- given
        vim.fn.fnameescape = function(path)
            return path:gsub(" ", "\\ ")
        end

        -- when
        common_util.edit_file("file:///tmp/my file.txt")

        -- then
        assert.are.equal("edit /tmp/my\\ file.txt", state.commands[1])
    end)

    it("notifies elapsed time and returns the callback result", function()
        -- given
        vim.fn.reltimefloat = function()
            return 0.125
        end

        -- when
        local result = common_util.timewatch(function()
            return "done"
        end, "work")

        -- then
        assert.are.equal("done", result)
        assert.are.same({ message = "work Took 0.125000 s", level = nil }, state.notifications[1])
    end)

    it("returns a timestamp with millisecond precision", function()
        -- given
        local pattern = "^%d%d%d%d%-%d%d%-%d%d %d%d:%d%d:%d%d%.%d%d%d$"

        -- when
        local value = common_util.now_datetime()

        -- then
        assert.matches(pattern, value)
    end)
end)
