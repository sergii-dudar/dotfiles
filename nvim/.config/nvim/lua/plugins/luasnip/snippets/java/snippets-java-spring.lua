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

    -- @Slf4j
    --
    -- @RequiredArgsConstructor
    -- @AllArgsConstructor
    return {
        s("properties", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import org.springframework.boot.context.properties.ConfigurationProperties;",
                "import org.springframework.validation.annotation.Validated;",
                "",
                "",
            }),
            t({
                "@Validated",
                "@ConfigurationProperties",
            }),
            t({ "", "" }),
            t({ "public record " }),
            f(snip_utils.current_java_file_name),
            t("("),
            i(0),
            t({ ") {", "", "}" }),
        }),
        s("controller", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import lombok.RequiredArgsConstructor;",
                "import lombok.extern.slf4j.Slf4j;",
                "import lombok.AccessLevel;",
                "import lombok.experimental.FieldDefaults;",
                "import org.springframework.web.bind.annotation.RestController;",
                "",
                "",
            }),
            t({
                "@Slf4j",
                "@RestController",
                "@RequiredArgsConstructor",
                "@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)",
            }),
            t({ "", "" }),
            t({ "public class " }),
            f(snip_utils.current_java_file_name),
            t({ "{", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("component", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import lombok.RequiredArgsConstructor;",
                "import lombok.extern.slf4j.Slf4j;",
                "import lombok.AccessLevel;",
                "import lombok.experimental.FieldDefaults;",
                "import org.springframework.stereotype.Component;",
                "import org.springframework.context.annotation.Configuration;",
                "import org.springframework.stereotype.Repository;",
                "import org.springframework.stereotype.Service;",
                "",
                "",
            }),
            t({ "@Slf4j", "" }),
            c(1, {
                t({ "@Component" }),
                t({ "@Configuration" }),
                t({ "@Service" }),
                t({ "@Repository" }),
            }),
            t({
                "",
                "@RequiredArgsConstructor",
                "@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)",
            }),
            t({ "", "" }),
            t({ "public class " }),
            f(snip_utils.current_java_file_name),
            t({ "{", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("usecase", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import lombok.RequiredArgsConstructor;",
                "import lombok.extern.slf4j.Slf4j;",
                "import lombok.AccessLevel;",
                "import lombok.experimental.FieldDefaults;",
                "import org.springframework.stereotype.Component;",
                "",
                "",
            }),
            t({
                "@Slf4j",
                "@Component",
                "@RequiredArgsConstructor",
                "@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)",
            }),
            t({ "", "" }),
            t({ "public class " }),
            f(snip_utils.current_java_file_name),
            c(1, {
                sn(nil, {
                    t("implements UseCase"),
                    -- no need to define the nodes for a given key a second time.
                    r(1, "other_class", i(1)),
                    t("<"),
                    i(2),
                    t(">"),
                }),
                sn(nil, {
                    t("extends "),
                    -- restoreNode: stores and restores nodes.
                    -- pass position, store-key and nodes.
                    r(1, "other_class"),
                }),
            }),
            t({ " {", "", "\t" }),
            t({
                "@Override",
                "\t",
            }),
            t("public "),
            i(2, "Output"),
            t(" execute("),
            i(3, "Input"),
            t(" "),
            i(3, "input"),
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
        s("gateway", {
            f(snip_utils.current_java_package),
            t({
                "",
                "",
                "import lombok.RequiredArgsConstructor;",
                "import lombok.extern.slf4j.Slf4j;",
                "import lombok.AccessLevel;",
                "import lombok.experimental.FieldDefaults;",
                "import org.springframework.stereotype.Component;",
                "",
                "",
            }),
            t({
                "@Slf4j",
                "@Component",
                "@RequiredArgsConstructor",
                "@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)",
            }),
            t({ "", "" }),
            t({ "public class " }),
            f(snip_utils.current_java_file_name),
            t("implements "),
            i(1),
            t("Gateway"),
            t({ " {", "", "\t" }),
            i(0),
            t({ "", "}" }),
        }),
        s("@properties", {
            f(snip_utils.add_imports({
                "org.springframework.boot.context.properties.ConfigurationProperties",
                "org.springframework.validation.annotation.Validated",
            })),
            t({
                "@Validated",
                "@ConfigurationProperties",
            }),
        }),
        s("@controller", {
            f(snip_utils.add_imports({
                "lombok.RequiredArgsConstructor",
                "lombok.extern.slf4j.Slf4j",
                "lombok.AccessLevel",
                "lombok.experimental.FieldDefaults",
                "org.springframework.web.bind.annotation.RestController",
            })),
            t({
                "@Slf4j",
                "@RestController",
                "@RequiredArgsConstructor",
                "@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)",
            }),
        }),
        s("@component", {
            f(snip_utils.add_imports({
                "lombok.RequiredArgsConstructor",
                "lombok.extern.slf4j.Slf4j",
                "lombok.AccessLevel",
                "lombok.experimental.FieldDefaults",
                "org.springframework.stereotype.Component",
                "org.springframework.context.annotation.Configuration",
                "org.springframework.stereotype.Repository",
                "org.springframework.stereotype.Service",
            })),
            t({ "@Slf4j", "" }),
            c(1, {
                t({ "@Component" }),
                t({ "@Configuration" }),
                t({ "@Service" }),
                t({ "@Repository" }),
            }),
            t({
                "",
                "@RequiredArgsConstructor",
                "@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)",
            }),
        }),
        s("@usecase", {
            f(snip_utils.add_imports({
                "lombok.RequiredArgsConstructor",
                "lombok.extern.slf4j.Slf4j",
                "lombok.AccessLevel",
                "lombok.experimental.FieldDefaults",
                "org.springframework.stereotype.Component",
            })),
            t({
                "@Slf4j",
                "@Component",
                "@RequiredArgsConstructor",
                "@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)",
            }),
        }),
    }
end

return M
