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
    return {
        s("@map", {
            t({
                "import org.mapstruct.Mapper;",
                "import org.mapstruct.Mapping;",
                "import org.mapstruct.ReportingPolicy;",
                "",
                "",
            }),
            c(1, {
                t("@Mapper(unmappedTargetPolicy = ReportingPolicy.ERROR)"),
                t("@Mapper(unmappedTargetPolicy = ReportingPolicy.WARN)"),
            }),
        }),
        -- s("mapper", {
        --     t({
        --         "import org.mapstruct.Mapper;",
        --         "import org.mapstruct.Mapping;",
        --         "import org.mapstruct.ReportingPolicy;",
        --         "",
        --         "",
        --     }),
        --     c(1, {
        --         t("@Mapper(unmappedTargetPolicy = ReportingPolicy.ERROR)"),
        --         t("@Mapper(unmappedTargetPolicy = ReportingPolicy.WARN)"),
        --     }),
        --     -- Choice: Switch between two different Nodes, first parameter is its position, second a list of nodes.
        --     t("public "),
        --     c(2, {
        --         t("interface "),
        --         t("abstract class "),
        --     }),
        --     i(2),
        --     t("Mapper "),
        --     c(3, {
        --         t("{"),
        --         -- sn: Nested Snippet. Instead of a trigger, it has a position, just like insertNodes. !!! These don't expect a 0-node!!!!
        --         -- Inside Choices, Nodes don't need a position as the choice node is the one being jumped to.
        --         sn(nil, {
        --             t("extends "),
        --             -- restoreNode: stores and restores nodes.
        --             -- pass position, store-key and nodes.
        --             r(1, "other_class", i(1)),
        --             t(" {"),
        --         }),
        --         sn(nil, {
        --             t("implements "),
        --             -- no need to define the nodes for a given key a second time.
        --             r(1, "other_class"),
        --             t(" {"),
        --         }),
        --     }),
        --     t({ "", "\t" }),
        --     i(0),
        --     t({ "", "}" }),
        -- }),
        s("mapi", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import org.mapstruct.Mapper;",
                "import org.mapstruct.Mapping;",
                "import org.mapstruct.ReportingPolicy;",
                "",
                "",
            }),
            c(1, {
                t("@Mapper(unmappedTargetPolicy = ReportingPolicy.ERROR)"),
                t("@Mapper"),
            }),
            -- Choice: Switch between two different Nodes, first parameter is its position, second a list of nodes.
            t({ "", "public interface " }),
            f(snip_utils.current_java_file_name),
            t({ "{", "", "\t" }),
            i(0),

            -- c(5, {
            --     t({"UserDto toUser(User user);"}),
            --     t({"public abstract UserDto toUser(User user);"}),
            -- }),

            t({ "", "}" }),
        }),
    }
end

return M
