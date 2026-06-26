local helper = require("tests.utils.spec_helper")

describe("utils.project-util", function()
    local project_util

    before_each(function()
        helper.reset_vim()
        project_util = helper.reload("utils.project-util")
    end)

    it("detects a known multi-file project directory segment", function()
        -- given
        vim.fn.getcwd = function()
            return "/Users/me/dotfiles/nvim"
        end
        vim.fn.argv = function()
            return {}
        end

        -- when
        local result = project_util.is_multifile_proj()

        -- then
        assert.is_true(result)
    end)

    it("does not treat partial path segment matches as project directories", function()
        -- given
        vim.fn.getcwd = function()
            return "/Users/me/mydotfiles-backup"
        end
        vim.fn.argv = function()
            return {}
        end

        -- when
        local result = project_util.is_multifile_proj()

        -- then
        assert.is_false(result)
    end)

    it("disables project startup behavior when a startup argument is a file", function()
        -- given
        vim.fn.getcwd = function()
            return "/Users/me/dotfiles"
        end
        vim.fn.argv = function()
            return { "README.md" }
        end
        vim.fn.fnamemodify = function(path)
            return "/Users/me/dotfiles/" .. path
        end
        vim.fn.isdirectory = function()
            return 0
        end

        -- when
        local result = project_util.is_multifile_proj()

        -- then
        assert.is_false(result)
    end)

    it("allows project startup behavior when startup arguments are directories", function()
        -- given
        vim.fn.getcwd = function()
            return "/Users/me/personal/example"
        end
        vim.fn.argv = function()
            return { "." }
        end
        vim.fn.fnamemodify = function()
            return "/Users/me/personal/example"
        end
        vim.fn.isdirectory = function()
            return 1
        end

        -- when
        local result = project_util.is_init_open_neotree()

        -- then
        assert.is_true(result)
    end)

    it("uses a narrower directory list for initial neo-tree opening", function()
        -- given
        vim.fn.getcwd = function()
            return "/Users/me/tools/parser"
        end
        vim.fn.argv = function()
            return {}
        end

        -- when
        local is_project = project_util.is_multifile_proj()
        local should_open_neotree = project_util.is_init_open_neotree()

        -- then
        assert.is_true(is_project)
        assert.is_false(should_open_neotree)
    end)
end)
