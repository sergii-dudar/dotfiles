local helper = require("tests.utils.spec_helper")

describe("utils.neotree-util", function()
    local neotree_util
    local state
    local clipboard_dir

    before_each(function()
        _, state = helper.reset_vim()
        vim.fn.stdpath = function()
            return "/tmp/nvim-data"
        end
        clipboard_dir = "/tmp/nvim-data/neo-tree-clipboard"
        neotree_util = helper.reload("utils.neotree-util")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.neotree-util", "neo-tree.sources.manager" })
    end)

    it("copies files and directories into the shared clipboard directory", function()
        -- given
        vim.fn.isdirectory = function(path)
            return (path == clipboard_dir or path == "/repo/dir") and 1 or 0
        end

        -- when
        neotree_util.copy_to_shared_clipboard({ "/repo/file.txt", "/repo/dir" })

        -- then
        assert.are.same({ path = clipboard_dir, flags = "rf" }, state.deleted_paths[1])
        assert.are.same({ path = clipboard_dir, flags = "p" }, state.mkdirs[1])
        assert.are.same({
            { "cp", "/repo/file.txt", clipboard_dir .. "/file.txt" },
            { "cp", "-r", "/repo/dir", clipboard_dir .. "/dir" },
        }, state.system_calls)
        assert.are.equal("Copied to shared clipboard:\nfile.txt\ndir", state.notifications[1].message)
    end)

    it("warns when pasting from a missing shared clipboard directory", function()
        -- given
        vim.fn.isdirectory = function()
            return 0
        end

        -- when
        neotree_util.paste_from_shared_clipboard("/dest")

        -- then
        assert.are.equal("Shared clipboard is empty", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
        assert.are.same({}, state.system_calls)
    end)

    it("warns when the shared clipboard directory has no items", function()
        -- given
        vim.fn.isdirectory = function(path)
            return path == clipboard_dir and 1 or 0
        end
        vim.fn.readdir = function()
            return {}
        end

        -- when
        neotree_util.paste_from_shared_clipboard("/dest")

        -- then
        assert.are.equal("Shared clipboard is empty", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("pastes clipboard items and renames destination conflicts", function()
        -- given
        vim.fn.readdir = function()
            return { "file.txt", "dir" }
        end
        vim.fn.filereadable = function(path)
            return path == "/dest/file.txt" and 1 or 0
        end
        vim.fn.isdirectory = function(path)
            if path == clipboard_dir or path == clipboard_dir .. "/dir" or path == "/dest/dir" then
                return 1
            end
            return 0
        end

        -- when
        neotree_util.paste_from_shared_clipboard("/dest")

        -- then
        assert.are.same({
            { "cp", clipboard_dir .. "/file.txt", "/dest/file_1.txt" },
            { "cp", "-r", clipboard_dir .. "/dir", "/dest/dir_1" },
        }, state.system_calls)
        assert.are.equal("Pasted from shared clipboard:\nfile_1.txt\ndir_1", state.notifications[1].message)
        assert.are.same({ path = clipboard_dir, flags = "rf" }, state.deleted_paths[1])
    end)

    it("copies the current non-message node", function()
        -- given
        local copied
        neotree_util.copy_to_shared_clipboard = function(paths)
            copied = paths
        end
        local state_arg = {
            tree = {
                get_node = function()
                    return {
                        type = "file",
                        get_id = function()
                            return "/repo/file.txt"
                        end,
                    }
                end,
            },
        }

        -- when
        neotree_util.shared_copy(state_arg)

        -- then
        assert.are.same({ "/repo/file.txt" }, copied)
        assert.are.same({ path = clipboard_dir, flags = "p" }, state.mkdirs[1])
    end)

    it("copies selected non-message nodes in visual mode", function()
        -- given
        local copied
        neotree_util.copy_to_shared_clipboard = function(paths)
            copied = paths
        end
        local selected_nodes = {
            {
                type = "file",
                get_id = function()
                    return "/repo/one.txt"
                end,
            },
            {
                type = "message",
                get_id = function()
                    return "/repo/message"
                end,
            },
            {
                type = "directory",
                get_id = function()
                    return "/repo/dir"
                end,
            },
        }

        -- when
        neotree_util.shared_copy_visual({}, selected_nodes)

        -- then
        assert.are.same({ "/repo/one.txt", "/repo/dir" }, copied)
        assert.are.same({ path = clipboard_dir, flags = "p" }, state.mkdirs[1])
    end)

    it("pastes into the current node directory and refreshes neo-tree", function()
        -- given
        local pasted_dir
        local refreshed_source
        neotree_util.paste_from_shared_clipboard = function(dest_dir)
            pasted_dir = dest_dir
        end
        helper.stub_module("neo-tree.sources.manager", {
            refresh = function(source)
                refreshed_source = source
            end,
        })
        local state_arg = {
            tree = {
                get_node = function()
                    return {
                        type = "file",
                        get_id = function()
                            return "/repo/src/file.txt"
                        end,
                    }
                end,
            },
        }

        -- when
        neotree_util.shared_paste(state_arg)

        -- then
        assert.are.equal("/repo/src", pasted_dir)
        assert.are.equal("filesystem", refreshed_source)
    end)
end)
