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

    return {
        s("@fieldd", {
            f(snip_utils.add_imports({
                "lombok.AccessLevel",
                "lombok.experimental.FieldDefaults",
            })),
            c(1, {
                sn(nil, {
                    t("@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)"),
                    i(1),
                }),
                sn(nil, {
                    t("@FieldDefaults(level = AccessLevel.PROTECTED)"),
                    i(1),
                }),
            }),
        }),
        s("immutable", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import lombok.Builder;",
                "import lombok.Value;",
                "",
                "",
            }),
            c(1, {
                t({
                    "@Value",
                    "@Builder",
                }),
                t('@Value(staticConstructor = "of")'),
            }),
            t({ "", "" }),
            t({ "public class " }),
            f(snip_utils.current_java_file_name),
            t({ "{", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("mutable", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import lombok.Builder;",
                "import lombok.Data;",
                "",
                "",
            }),
            t({
                "@Data",
                "@Builder",
            }),
            t({ "", "" }),
            t({ "public class " }),
            f(snip_utils.current_java_file_name),
            t({ "{", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("util", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import lombok.experimental.UtilityClass;",
                "",
                "",
            }),
            t({
                "@UtilityClass",
            }),
            t({ "", "" }),
            t({ "public class " }),
            f(snip_utils.current_java_file_name),
            t({ " {", "", "\t" }),
            t("public static "),
            i(1, "Result"),
            t(" "),
            i(2, "methodName"),
            t("("),
            i(3, "Input"),
            t(" "),
            i(4, "param"),
            t({
                ") {",
                "\t\t",
            }),
            i(0),
            t({ "", "\t" }),
            t({
                "}",
                "}",
            }),
        }),
    }
end

return M
