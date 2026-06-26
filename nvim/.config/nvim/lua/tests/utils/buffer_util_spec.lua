local helper = require("tests.utils.spec_helper")

describe("utils.buffer-util", function()
    local buffer_util
    local state
    local window_util

    before_each(function()
        _, state = helper.reset_vim()
        window_util = {
            bot_split = function()
                state.current_win = 200
                state.bot_split_called = true
            end,
            restore_position = function()
                state.restore_position_called = true
            end,
        }
        helper.stub_module("utils.nvim.window-util", window_util)
        buffer_util = helper.reload("utils.buffer-util")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.buffer-util", "utils.nvim.window-util" })
    end)

    it("returns loaded listed buffers and loaded work-filetype buffers", function()
        -- given
        state.buffers = { 1, 2, 3, 4 }
        state.loaded_buffers = { [1] = true, [2] = true, [3] = true, [4] = false }
        state.buffer_options = {
            [1] = { filetype = "txt", buflisted = true },
            [2] = { filetype = "java", buflisted = false },
            [3] = { filetype = "help", buflisted = false },
            [4] = { filetype = "lua", buflisted = true },
        }

        -- when
        local active = buffer_util.get_active_ls_buffers()

        -- then
        assert.are.same({ 1, 2 }, active)
    end)

    it("finds a loaded buffer by normalized path", function()
        -- given
        state.buffers = { 1, 2 }
        state.loaded_buffers = { [1] = true, [2] = true }
        state.buffer_names = {
            [1] = "/repo/a.lua",
            [2] = "/repo/b.lua",
        }

        -- when
        local found = buffer_util.find_buf_by_path("/repo/b.lua")

        -- then
        assert.are.equal(2, found)
    end)

    it("returns nil when no loaded buffer matches the path", function()
        -- given
        state.buffers = { 1 }
        state.loaded_buffers = { [1] = false }
        state.buffer_names = { [1] = "/repo/a.lua" }

        -- when
        local found = buffer_util.find_buf_by_path("/repo/a.lua")

        -- then
        assert.is_nil(found)
    end)

    it("deletes the matching buffer by path", function()
        -- given
        state.buffers = { 9 }
        state.loaded_buffers = { [9] = true }
        state.buffer_names = { [9] = "/repo/delete.lua" }

        -- when
        local closed = buffer_util.close_buffer_by_path("/repo/delete.lua")

        -- then
        assert.is_true(closed)
        assert.are.same({ bufnr = 9, opts = { force = false } }, state.deleted_buffer)
    end)

    it("returns false when closing a missing buffer by path", function()
        -- given
        state.buffers = {}

        -- when
        local closed = buffer_util.close_buffer_by_path("/repo/missing.lua")

        -- then
        assert.is_false(closed)
        assert.is_nil(state.deleted_buffer)
    end)

    it("opens a path through vim.cmd.edit", function()
        -- given
        local path = "/repo/open.lua"

        -- when
        buffer_util.open_buffer_by_path(path)

        -- then
        assert.are.equal("edit /repo/open.lua", state.commands[1])
    end)

    it("moves focus right only from a neo-tree buffer", function()
        -- given
        vim.fn.expand = function()
            return "neo-tree filesystem [1]"
        end

        -- when
        buffer_util.focus_right_if_neotree()

        -- then
        assert.are.equal("wincmd l", state.commands[1])
    end)

    it("does not move focus from a regular buffer", function()
        -- given
        vim.fn.expand = function()
            return "/repo/init.lua"
        end

        -- when
        buffer_util.focus_right_if_neotree()

        -- then
        assert.are.same({}, state.commands)
    end)

    it("opens a scratch buffer in a bottom split with expected options", function()
        -- given
        state.buffer_lines[17] = { "one", "two", "three" }

        -- when
        buffer_util.open_scratch_split(17, { max_height = 2, filetype = "dap-log" })

        -- then
        assert.is_true(state.bot_split_called)
        assert.are.equal("nofile", vim.bo[17].buftype)
        assert.are.equal("wipe", vim.bo[17].bufhidden)
        assert.are.equal("dap-log", vim.bo[17].filetype)
        assert.is_false(vim.bo[17].modifiable)
        assert.are.same({ win = 200, bufnr = 17 }, state.window_set_buf)
        assert.are.same({ win = 200, height = 2 }, state.window_height)
        assert.is_true(vim.wo[200].wrap)
        assert.are.equal("q", state.keymaps[1].lhs)
    end)
end)
