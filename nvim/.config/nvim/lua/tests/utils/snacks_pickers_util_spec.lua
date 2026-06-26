local helper = require("tests.utils.spec_helper")

describe("utils.snacks-pickers-util", function()
    local pickers_util
    local state
    local lazy_pick_calls

    local function install_lazyvim()
        local pick = setmetatable({}, {
            __call = function(_, source, opts)
                table.insert(lazy_pick_calls, { kind = "call", source = source, opts = opts })
                return function()
                    table.insert(lazy_pick_calls, { kind = "execute", source = source, opts = opts })
                end
            end,
        })
        pick.open = function(source, opts)
            table.insert(lazy_pick_calls, { kind = "open", source = source, opts = opts })
        end
        _G.LazyVim = { pick = pick }
    end

    local function install_recent_modules(has_results)
        helper.stub_module("snacks.picker.core.filter", {
            new = function(opts)
                state.created_filter = opts
                return { filter = true }
            end,
        })
        helper.stub_module("snacks.picker.source.recent", {
            files = function(opts)
                state.recent_finder_opts = opts
                return function(emit)
                    if has_results then
                        emit({ file = "/repo/recent.lua" })
                    end
                end
            end,
        })
    end

    local function picker(source, input_text)
        return {
            main = 20,
            opts = { source = source },
            input = {
                filter = {
                    current_buf = 1,
                    cwd = "/repo",
                },
                get = function()
                    return input_text or ""
                end,
            },
            cwd = function()
                return "/repo"
            end,
            norm = function(self, callback)
                self.norm_called = true
                callback()
            end,
            close = function(self)
                self.closed = true
            end,
        }
    end

    before_each(function()
        _, state = helper.reset_vim()
        lazy_pick_calls = {}
        install_lazyvim()
        _G.Snacks = {
            picker = {
                config = {
                    get = function(opts)
                        state.config_opts = opts
                        return opts
                    end,
                },
                buffers = function(opts)
                    state.buffers_picker_opts = opts or {}
                end,
                recent = function(opts)
                    state.recent_picker_opts = opts
                end,
            },
        }
        pickers_util = helper.reload("utils.snacks-pickers-util")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "utils.snacks-pickers-util",
            "snacks.picker.core.filter",
            "snacks.picker.source.recent",
        })
        _G.Snacks = nil
        _G.LazyVim = nil
    end)

    it("opens the buffers picker when another listed buffer is visible", function()
        -- given
        state.current_buf = 1
        state.buffers = { 1, 2 }
        vim.bo[1].buflisted = true
        vim.bo[1].buftype = ""
        vim.bo[2].buflisted = true
        vim.bo[2].buftype = ""

        -- when
        pickers_util.open_buffers_or_recent_or_files()

        -- then
        assert.are.same({}, state.buffers_picker_opts)
        assert.are.same({}, state.notifications)
    end)

    it("falls back to recent files when no buffer picker results exist", function()
        -- given
        install_recent_modules(true)
        state.current_buf = 1
        state.buffers = { 1 }
        vim.bo[1].buflisted = true
        vim.bo[1].buftype = ""
        vim.fn.getcwd = function()
            return "/repo"
        end

        -- when
        pickers_util.open_buffers_or_recent_or_files()

        -- then
        assert.matches("No buffers to show, opening recent files", state.notifications[1].message)
        assert.are.same({ cwd = "/repo", filter = { cwd = true } }, state.recent_picker_opts)
    end)

    it("falls back to cwd files when neither buffers nor recent files have results", function()
        -- given
        install_recent_modules(false)
        state.current_buf = 1
        state.buffers = { 1 }
        vim.bo[1].buflisted = true
        vim.bo[1].buftype = ""

        -- when
        pickers_util.open_buffers_or_recent_or_files()

        -- then
        assert.matches("No buffers or recent files to show, opening files", state.notifications[1].message)
        assert.are.same({
            { kind = "call", source = "files", opts = { root = false } },
            { kind = "execute", source = "files", opts = { root = false } },
        }, lazy_pick_calls)
    end)

    it("switches to files and seeds the fuzzy pattern from picker input", function()
        -- given
        local active_picker = picker("buffers", "needle")

        -- when
        pickers_util.switch_to_files(active_picker)

        -- then
        assert.is_true(active_picker.closed)
        assert.are.same({
            { kind = "open", source = "files", opts = { root = false, pattern = "needle" } },
        }, lazy_pick_calls)
    end)

    it("switches to grep and seeds the live search field from picker input", function()
        -- given
        local active_picker = picker("files", "needle")

        -- when
        pickers_util.switch_to_grep(active_picker)

        -- then
        assert.is_true(active_picker.closed)
        assert.are.same({
            { kind = "open", source = "live_grep", opts = { root = false, search = "needle" } },
        }, lazy_pick_calls)
    end)

    it("switches to recent files with cwd-scoped options", function()
        -- given
        local active_picker = picker("files", "needle")

        -- when
        pickers_util.switch_to_recent(active_picker)

        -- then
        assert.is_true(active_picker.closed)
        assert.are.same({ cwd = "/repo", filter = { cwd = true }, pattern = "needle" }, state.recent_picker_opts)
    end)

    it("rotates to the next available picker while skipping empty recent results", function()
        -- given
        install_recent_modules(false)
        local active_picker = picker("buffers", "")
        state.buffers = { 1 }
        vim.bo[1].buflisted = true
        vim.bo[1].buftype = ""

        -- when
        pickers_util.switch_to_next_picker(active_picker)

        -- then
        assert.is_true(active_picker.closed)
        assert.are.same({
            { kind = "open", source = "files", opts = { root = false } },
        }, lazy_pick_calls)
    end)

    it("rotates to the previous available picker when recent has results", function()
        -- given
        install_recent_modules(true)
        local active_picker = picker("files", "recent query")
        state.buffers = { 1 }
        vim.bo[1].buflisted = true
        vim.bo[1].buftype = ""

        -- when
        pickers_util.switch_to_prev_picker(active_picker)

        -- then
        assert.is_true(active_picker.closed)
        assert.are.same({ cwd = "/repo", filter = { cwd = true }, pattern = "recent query" }, state.recent_picker_opts)
    end)
end)
