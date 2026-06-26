local helper = require("tests.utils.spec_helper")

describe("utils.diff-util", function()
    local diff_util
    local state

    before_each(function()
        _, state = helper.reset_vim()
        diff_util = helper.reload("utils.diff-util")
    end)

    it("opens two files in a tab-local diff view", function()
        -- given
        vim.fn.fnameescape = function(path)
            return path:gsub(" ", "\\ ")
        end

        -- when
        diff_util.diff_files("/tmp/left file.txt", "/tmp/right file.txt")

        -- then
        assert.are.same({
            "tabnew /tmp/left\\ file.txt",
            "diffthis",
            "vsplit /tmp/right\\ file.txt",
            "diffthis",
        }, state.commands)
    end)

    it("warns when fewer than two items are selected", function()
        -- given
        local items = { { file = "/tmp/one.txt" } }

        -- when
        local ok = diff_util.diff_selected(items)

        -- then
        assert.is_false(ok)
        assert.are.equal("Select exactly 2 files to diff (use <Tab> to select)", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("warns with the selected count when more than two items are selected", function()
        -- given
        local items = { { file = "a" }, { file = "b" }, { file = "c" } }

        -- when
        local ok = diff_util.diff_selected(items)

        -- then
        assert.is_false(ok)
        assert.are.equal("Select exactly 2 files to diff (got 3)", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("notifies when selected items cannot be resolved to file paths", function()
        -- given
        local items = { { file = "/tmp/one.txt" }, { cwd = "/tmp" } }

        -- when
        local ok = diff_util.diff_selected(items)

        -- then
        assert.is_false(ok)
        assert.are.equal("Could not resolve file paths", state.notifications[1].message)
        assert.are.equal(vim.log.levels.ERROR, state.notifications[1].level)
    end)

    it("resolves cwd-relative picker items before diffing", function()
        -- given
        local diffed
        diff_util.diff_files = function(file1, file2)
            diffed = { file1, file2 }
        end
        local items = {
            { file = "left.txt", cwd = "/repo" },
            { file = "/abs/right.txt", cwd = "/repo" },
        }

        -- when
        local ok = diff_util.diff_selected(items)

        -- then
        assert.is_true(ok)
        assert.are.same({ "/repo/left.txt", "/abs/right.txt" }, diffed)
    end)
end)
