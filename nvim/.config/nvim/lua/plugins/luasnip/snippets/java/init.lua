local common = require("snips.common")

local M = {}

function M.setup()
    local ls = require("luasnip")

    -- some shorthands...
    local s = ls.snippet
    local sn = ls.snippet_node
    local t = ls.text_node
    local i = ls.insert_node
    local f = ls.function_node
    local c = ls.choice_node
    local d = ls.dynamic_node
    local r = ls.restore_node
    local l = require("luasnip.extras").lambda
    local rep = require("luasnip.extras").rep
    local p = require("luasnip.extras").partial
    local m = require("luasnip.extras").match
    local n = require("luasnip.extras").nonempty
    local dl = require("luasnip.extras").dynamic_lambda
    local fmt = require("luasnip.extras.fmt").fmt
    local fmta = require("luasnip.extras.fmt").fmta
    local types = require("luasnip.util.types")
    local conds = require("luasnip.extras.conditions")
    local conds_expand = require("luasnip.extras.conditions.expand")

    local java = require("snips.java")

    local java_snippets = vim.list_extend({
        s("o", java.primitives.stdout()),
        s("c", java.primitives.class()),
        s("main", java.primitives.main()),
        s("mainc", java.primitives.main_class()), -- generates a class with a main
        s("f", java.primitives.method()), -- generates a class with a main
        s("for", java.primitives["for"]()), -- generates a class with a main
        s("r", common.primitives.returns()),
        s("v", java.choices.variable()),
    }, require("plugins.luasnip.snippets.java.snippets-java-mapstruct").snippets())
    java_snippets =
        vim.list_extend(java_snippets, require("plugins.luasnip.snippets.java.snippets-java-lombok").snippets())
    ls.add_snippets("java", java_snippets)
end

return M
