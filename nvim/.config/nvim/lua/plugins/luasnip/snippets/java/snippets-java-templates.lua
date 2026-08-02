-- Whole-file Java templates used by the file-template engine
-- (`modules/common/file-template` + `modules/java/file-template`).
--
-- These are registered under the synthetic LuaSnip filetype `java-template`
-- instead of `java`, so they never show up in normal completion while typing;
-- they are looked up by trigger only when a new empty `.java` file is created.
-- Regular `java` snippets (util, usecase, component, controller, mapper,
-- immutable, mutable, properties, gateway) are reused as templates directly and
-- are intentionally NOT duplicated here.

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

    return {
        s("class", {
            f(snip_utils.current_java_package),
            t({ "", "", "public class " }),
            f(snip_utils.current_java_class_name),
            t({ " {", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("abstract", {
            f(snip_utils.current_java_package),
            t({ "", "", "public abstract class " }),
            f(snip_utils.current_java_class_name),
            t({ " {", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("interface", {
            f(snip_utils.current_java_package),
            t({ "", "", "public interface " }),
            f(snip_utils.current_java_class_name),
            t({ " {", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("enum", {
            f(snip_utils.current_java_package),
            t({ "", "", "public enum " }),
            f(snip_utils.current_java_class_name),
            t({ " {", "", "\t" }),
            i(1, "VALUE"),
            t({ ";", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("record", {
            f(snip_utils.current_java_package),
            t({ "", "", "public record " }),
            f(snip_utils.current_java_class_name),
            t("("),
            i(1),
            t({ ") {", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("exception", {
            f(snip_utils.current_java_package),
            t({ "", "", "public class " }),
            f(snip_utils.current_java_class_name),
            t(" extends "),
            c(1, {
                t("RuntimeException"),
                t("Exception"),
                sn(nil, { i(1, "ParentException") }),
            }),
            t({ " {", "", "\t" }),
            t("public "),
            f(snip_utils.current_java_class_name),
            t({ "(String message) {", "\t\tsuper(message);", "\t}", "", "\t" }),
            t("public "),
            f(snip_utils.current_java_class_name),
            t({ "(String message, Throwable cause) {", "\t\tsuper(message, cause);", "\t}", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("repository", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import org.springframework.data.jpa.repository.JpaRepository;",
                "import org.springframework.stereotype.Repository;",
                "",
                "@Repository",
                "public interface ",
            }),
            f(snip_utils.current_java_class_name),
            t(" extends JpaRepository<"),
            i(1, "Entity"),
            t(", "),
            i(2, "Long"),
            t({ "> {", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("test", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import org.assertj.core.api.Assertions;",
                "import org.junit.jupiter.api.Test;",
                "",
                "class ",
            }),
            f(snip_utils.current_java_class_name),
            t({ " {", "", "\t@Test", "\tvoid " }),
            i(1, "shouldDoSomething"),
            t({ "() {", "\t\t" }),
            i(0),
            t({ "", "\t}", "}" }),
        }),
    }
end

return M
