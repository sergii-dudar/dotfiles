local M = {}

function M.snippets()
    local snip_utils = require("utils.java.snippets-java-util")
    local ls = require("luasnip")
    local s = ls.snippet
    local sn = ls.snippet_node
    local t = ls.text_node
    local i = ls.insert_node
    local f = ls.function_node
    local c = ls.choice_node
    local d = ls.dynamic_node
    local r = ls.restore_node
    -- local l = require("luasnip.extras").lambda
    -- local rep = require("luasnip.extras").rep
    -- local p = require("luasnip.extras").partial
    -- local m = require("luasnip.extras").match
    -- local n = require("luasnip.extras").nonempty
    -- local dl = require("luasnip.extras").dynamic_lambda
    -- local fmt = require("luasnip.extras.fmt").fmt
    -- local fmta = require("luasnip.extras.fmt").fmta
    -- local types = require("luasnip.util.types")
    -- local conds = require("luasnip.extras.conditions")
    -- local conds_expand = require("luasnip.extras.conditions.expand")

    -- TODO:
    return {
        s("assertEq", {
            f(snip_utils.add_imports({
                "org.assertj.core.api.Assertions",
            })),
            t("Assertions.assertThat("),
            i(1),
            t(").isEqualTo("),
            i(2),
            t({ ");", "" }),
            i(0),
        }),
        s("@test", {
            f(snip_utils.add_imports({
                "org.junit.jupiter.api.Test",
                "org.junit.jupiter.params.ParameterizedTest",
                "org.junit.jupiter.params.provider.CsvSource",
                "org.junit.jupiter.params.provider.EnumSource",
                "org.junit.jupiter.params.provider.MethodSource",
                "org.junit.jupiter.params.provider.ValueSource",
                "java.util.stream.Stream",
            })),
            c(1, {
                sn(nil, {
                    t({
                        "@ParameterizedTest",
                        '@MethodSource("',
                    }),
                    i(1, "methodSourceName"),
                    t({
                        '")',
                        "void ",
                    }),
                    i(2, "testName"),
                    t("("),
                    i(3, "Object"),
                    t(" "),
                    i(4, "param"),
                    t({ ") {", "\t" }),
                    i(5),
                    t({
                        "",
                        "}",
                        "",
                        "static Stream<",
                    }),
                    i(6, "Object"),
                    t("> "),
                    i(7, "methodSourceName"),
                    t({
                        "() {",
                        "\t",
                    }),
                    t("return Stream.of("),
                    i(8),
                    t(");"),
                    t({
                        "",
                        "}",
                    }),
                }),
                sn(nil, {
                    t({
                        "@Test",
                        "void ",
                    }),
                    i(1, "testName"),
                    t({ "() {", "\t" }),
                    i(2),
                    t({ "", "}" }),
                }),
                sn(nil, {
                    t({
                        "@ParameterizedTest",
                        "@CsvSource({",
                        '\t"arg1, agr2"',
                        "})",
                        "void ",
                    }),
                    i(1, "testName"),
                    t({ "() {", "\t" }),
                    i(2),
                    t({ "", "}" }),
                }),
                sn(nil, {
                    t({
                        "@ParameterizedTest",
                        "@EnumSource(value = ",
                    }),
                    i(1, "Enum"),
                    t('.class, names = {"'),
                    i(2, "ENUM_ARG1"),
                    t('", "'),
                    i(3, "ENUM_ARG1"),
                    t({
                        '"})',
                        "void ",
                    }),
                    i(4, "testName"),
                    t({ "() {", "\t" }),
                    i(5),
                    t({ "", "}" }),
                }),
                sn(nil, {
                    t({
                        "@ParameterizedTest",
                        "@EnumSource(value = ",
                    }),
                    i(1),
                    t('.class, names = {"'),
                    i(2),
                    t('", "'),
                    i(3),
                    t({
                        '"})',
                        "void ",
                    }),
                    i(4, "testName"),
                    t({ "() {", "\t" }),
                    i(5),
                    t({ "", "}" }),
                }),
            }),

            -- t("void "),
            -- i(1),
            -- t("("),
            -- i(2),
            -- t(")"),
            -- t({ ");", "" }),
            -- i(0),
        }),
    }
end

return M