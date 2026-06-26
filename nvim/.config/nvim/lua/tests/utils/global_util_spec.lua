local helper = require("tests.utils.spec_helper")

describe("utils.global-util", function()
    before_each(function()
        helper.reset_vim()
        _G.global = nil
    end)

    it("resolves paths under the user's dotfiles directory", function()
        -- given
        local global_util = helper.reload("utils.global-util")
        local expected = vim.fs.joinpath(os.getenv("HOME"), "dotfiles", "nvim/.config/nvim")

        -- when
        local resolved = global_util.dotfiles_path("nvim/.config/nvim")

        -- then
        assert.are.equal(expected, resolved)
    end)

    it("publishes feature flags from vim.env", function()
        -- given
        vim.env.LIMITED = "1"
        vim.env.LOAD_ALL = "1"

        -- when
        local global_util = helper.reload("utils.global-util")

        -- then
        assert.is_true(global_util.is_limited)
        assert.is_false(global_util.is_not_limited)
        assert.is_true(global_util.is_all)
    end)

    it("assigns the exported table to _G.global", function()
        -- given
        vim.env.LIMITED = nil

        -- when
        local global_util = helper.reload("utils.global-util")

        -- then
        assert.are.equal(global_util, _G.global)
        assert.is_false(global_util.is_limited)
        assert.is_true(global_util.is_not_limited)
    end)
end)
