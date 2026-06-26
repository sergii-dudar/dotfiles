local helper = require("tests.utils.spec_helper")

describe("utils.indent-util", function()
    local indent_util

    before_each(function()
        helper.reset_vim()
        indent_util = helper.reload("utils.indent-util")
    end)

    local function configure_lines(lines, indents)
        vim.fn.getline = function(lnum)
            return lines[lnum] or ""
        end
        vim.fn.indent = function(lnum)
            return indents[lnum] or 0
        end
        vim.fn.prevnonblank = function(lnum)
            while lnum > 0 do
                if (lines[lnum] or ""):match("%S") then
                    return lnum
                end
                lnum = lnum - 1
            end
            return 0
        end
    end

    it("returns zero when there is no previous non-blank line", function()
        -- given
        vim.v.lnum = 1
        configure_lines({ "" }, {})

        -- when
        local indent = indent_util.chain_indentexpr()

        -- then
        assert.are.equal(0, indent)
    end)

    it("starts a new method-chain indent from the origin line", function()
        -- given
        vim.v.lnum = 3
        vim.bo.shiftwidth = 4
        configure_lines({
            [2] = "    builder",
            [3] = "        .map()",
        }, {
            [2] = 4,
        })

        -- when
        local indent = indent_util.chain_indentexpr()

        -- then
        assert.are.equal(12, indent)
    end)

    it("aligns a new chain line with an existing chain indent", function()
        -- given
        vim.v.lnum = 4
        vim.bo.shiftwidth = 4
        configure_lines({
            [2] = "    builder",
            [3] = "            .map()",
            [4] = "            .filter()",
        }, {
            [2] = 4,
            [3] = 12,
        })

        -- when
        local indent = indent_util.chain_indentexpr()

        -- then
        assert.are.equal(12, indent)
    end)

    it("keeps the same indent after an unfinished previous chain member", function()
        -- given
        vim.v.lnum = 4
        configure_lines({
            [3] = "            .map()",
            [4] = "nextCall()",
        }, {
            [3] = 12,
        })

        -- when
        local indent = indent_util.chain_indentexpr()

        -- then
        assert.are.equal(12, indent)
    end)

    it("delegates to cindent after a previous chain member ending with a semicolon", function()
        -- given
        vim.v.lnum = 4
        configure_lines({
            [3] = "            .build();",
            [4] = "nextStatement();",
        }, {
            [3] = 12,
        })
        vim.fn.cindent = function(lnum)
            return lnum + 10
        end

        -- when
        local indent = indent_util.chain_indentexpr()

        -- then
        assert.are.equal(14, indent)
    end)

    it("delegates non-chain lines to cindent", function()
        -- given
        vim.v.lnum = 2
        configure_lines({
            [1] = "if (ready) {",
            [2] = "return value;",
        }, {
            [1] = 0,
        })
        vim.fn.cindent = function()
            return 8
        end

        -- when
        local indent = indent_util.chain_indentexpr()

        -- then
        assert.are.equal(8, indent)
    end)

    it("activates chain-aware indentation after the scheduled buffer callback", function()
        -- given
        local _, state = helper.reset_vim()
        indent_util = helper.reload("utils.indent-util")
        state.current_buf = 15

        -- when
        indent_util.activate({ cinoptions = "j1,+2s" })

        -- then
        assert.are.equal("v:lua.require'utils.indent-util'.chain_indentexpr()", vim.bo.indentexpr)
        assert.are.equal("j1,+2s", vim.bo.cinoptions)
        assert.are.same({ "0." }, vim.opt_local.indentkeys.appended)
    end)
end)
