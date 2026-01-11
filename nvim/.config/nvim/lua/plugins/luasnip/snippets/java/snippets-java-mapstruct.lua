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
        s("@mapper", {
            f(snip_utils.add_imports({
                "org.mapstruct.Mapper",
                "org.mapstruct.ReportingPolicy",
            })),
            c(1, {
                t("@Mapper(unmappedTargetPolicy = ReportingPolicy.ERROR)"),
                t("@Mapper(unmappedTargetPolicy = ReportingPolicy.WARN)"),
            }),
        }),
        s("mapper", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import org.mapstruct.Mapper;",
                "import org.mapstruct.ReportingPolicy;",
                "",
                "",
            }),
            c(1, {
                t("@Mapper(unmappedTargetPolicy = ReportingPolicy.ERROR)"),
                t("@Mapper"),
            }),
            t({ "", "" }),
            c(2, {
                t({ "public interface " }),
                t({ "public abstract class " }),
            }),
            f(snip_utils.current_java_file_name),
            t({ "{", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("@mapping", {
            f(snip_utils.add_imports({
                "org.mapstruct.Mapping",
            })),
            c(1, {
                sn(nil, {
                    t('@Mapping(target = "'),
                    r(1, "target"),
                    t('", source = "'),
                    r(2, "source"),
                    t('")'),
                }),
                sn(nil, {
                    t('@Mapping(target = "'),
                    r(1, "target"),
                    t('", ignore = true)'),
                }),
                sn(nil, {
                    t('@Mapping(target = "'),
                    r(1, "target"),
                    t('", expression = "java('),
                    r(2, "expression"),
                    t(')")'),
                }),
            }),
        }),
        s("@vmapping", {
            f(snip_utils.add_imports({
                "org.mapstruct.ValueMappings",
                "org.mapstruct.MappingConstants",
            })),
            c(1, {
                sn(nil, {
                    t('@ValueMapping(target = "'),
                    r(1, "target"),
                    t('", source = "'),
                    r(2, "source"),
                    t('")'),
                }),
                sn(nil, {
                    t(
                        "@ValueMapping(target = MappingConstants.THROW_EXCEPTION, source = MappingConstants.ANY_UNMAPPED)"
                    ),
                    i(1), -- to hold cursor and be able to go to next\back choice.
                }),
            }),
        }),
    }
end

return M
