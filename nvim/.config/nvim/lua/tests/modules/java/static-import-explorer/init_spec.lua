local helper = require("tests.utils.spec_helper")

describe("modules.java.static-import-explorer", function()
    local explorer
    local state
    local dep_search
    local deps_loaded
    local picker_opens
    local rg_calls

    local match_line =
        '/repo/module/src/main/java/com/acme/Constants.java:3:    public static final String REQUEST_ID = "x";'

    --- Complete the captured `vim.system` call with a canned rg result.
    ---@param result { code: integer, stdout?: string, stderr?: string }
    local function finish_rg(result)
        assert.are.equal(1, #rg_calls)
        rg_calls[1].on_exit(result)
    end

    before_each(function()
        _, state = helper.reset_vim()
        deps_loaded = true
        picker_opens = {}
        rg_calls = {}

        -- Cursor on `REQUEST_ID` in a main-scope Java buffer.
        state.buffer_names[1] = "/repo/module/src/main/java/com/acme/app/Foo.java"
        state.buffer_lines[1] = { "package com.acme.app;", "", "class Foo { String id = REQUEST_ID; }" }
        state.current_line = state.buffer_lines[1][3]
        state.cursor = { 3, 24 }

        vim.fn.isdirectory = function(path)
            return path == "/repo/module/src/main/java" and 1 or 0
        end
        vim.schedule_wrap = function(callback)
            return callback
        end
        vim.system = function(command, opts, on_exit)
            table.insert(rg_calls, { command = command, opts = opts, on_exit = on_exit })
            return {}
        end
        vim.ui = {
            select = function() end,
            input = function() end,
        }

        helper.stub_module("utils.java.java-common", {
            get_buffer_project_path = function()
                return "/repo/module"
            end,
            is_test_file = function()
                return false
            end,
            file_to_fqcn = function(file)
                if file:match("Constants%.java$") then
                    return "com.acme.Constants"
                end
                return nil
            end,
        })
        dep_search = {
            coord_match_path = function(path)
                return path
            end,
            is_loaded = function()
                return deps_loaded
            end,
            load_sources = function(opts)
                -- Simulates jdtls returning no classpath: on_done never fires.
                if opts.on_fail then
                    opts.on_fail()
                end
            end,
            get_source_dirs = function()
                return {}
            end,
            get_source_dirs_all = function()
                return {}
            end,
        }
        helper.stub_module("modules.java.dependencies-search", dep_search)
        helper.stub_module("modules.java.static-import-explorer.picker", {
            open = function(_, picker_state)
                table.insert(picker_opens, picker_state)
            end,
        })
        helper.reload("modules.java.static-import-explorer.util")
        explorer = helper.reload("modules.java.static-import-explorer")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.static-import-explorer",
            "modules.java.static-import-explorer.util",
            "modules.java.static-import-explorer.picker",
            "modules.java.dependencies-search",
            "utils.java.java-common",
        })
    end)

    it("searches module sources when dependency sources cannot be loaded", function()
        -- given
        deps_loaded = false

        -- when
        explorer.quick_import()
        finish_rg({ code = 0, stdout = match_line .. "\n" })

        -- then
        assert.are.equal("/repo/module/src/main/java", rg_calls[1].command[#rg_calls[1].command])
        assert.are.same({
            "package com.acme.app;",
            "",
            "import static com.acme.Constants.REQUEST_ID;",
            "",
            "class Foo { String id = REQUEST_ID; }",
        }, state.buffer_lines[1])
        assert.are.equal(
            "[Static Import] Dependency sources unavailable, searching module sources only",
            state.notifications[1].message
        )
        assert.are.same({}, picker_opens)
    end)

    it("keeps ripgrep matches when rg reports a partial error", function()
        -- when
        explorer.quick_import()
        finish_rg({ code = 2, stdout = match_line .. "\n", stderr = "rg: /deps/missing: No such file or directory" })

        -- then
        assert.are.equal("import static com.acme.Constants.REQUEST_ID;", state.buffer_lines[1][3])
        assert.are.same({}, picker_opens)
    end)

    it("reports the rg error and falls back to the picker when there is no output", function()
        -- when
        explorer.quick_import()
        finish_rg({ code = 2, stdout = "", stderr = "rg: regex parse error\n" })

        -- then
        assert.are.equal("[Static Import] rg failed: rg: regex parse error", state.notifications[1].message)
        assert.are.equal("[Static Import] No matches found", state.notifications[2].message)
        assert.are.equal(1, #picker_opens)
        assert.is_true(picker_opens[1].include_all_deps)
        assert.are.equal("REQUEST_ID", picker_opens[1].current_word)
        assert.are.equal(3, #state.buffer_lines[1])
    end)
end)
