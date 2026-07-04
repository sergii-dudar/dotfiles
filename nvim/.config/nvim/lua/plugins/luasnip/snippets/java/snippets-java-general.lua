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
    -- local postfix = require("luasnip.extras.postfix").postfix
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

    local function extract_builder_type(context)
        return context:match("(%u[%w_]*)%s+[%w_]+%s*=%s*$")
    end

    local function show_builder_context(line_to_cursor)
        -- Strip the partial trigger word being typed to check the preceding context
        local context = line_to_cursor:match("^(.-)[%w_]*$")
        return context ~= nil and extract_builder_type(context) ~= nil
    end

    local function is_builder_context(line_to_cursor)
        return line_to_cursor:match("%u[%w_]*%s+[%w_]+%s*=%s*build$") ~= nil
    end

    return {
        s("nie", {
            f(snip_utils.add_imports({
                "org.apache.commons.lang3.NotImplementedException",
            })),
            t({ "throw new NotImplementedException();" }),
        }),
        s({
            trig = "@sunused",
            dscr = '@SuppressWarnings("unused")',
            docstring = '@SuppressWarnings("unused")',
        }, {
            t('@SuppressWarnings("unused")'),
        }),
        s({
            trig = "@sdeprecated",
            dscr = '@SuppressWarnings("deprecation")',
            docstring = '@SuppressWarnings("deprecation")',
        }, {
            t('@SuppressWarnings("deprecation")'),
        }),
        s({
            trig = "@sunchecked",
            dscr = '@SuppressWarnings("unchecked")',
            docstring = '@SuppressWarnings("unchecked")',
        }, {
            t('@SuppressWarnings("unchecked")'),
        }),
        s({
            trig = "buildsnip",
            dscr = "Type.builder().<methods>.build();",
            docstring = "Type.builder().$1.build();",
            resolveExpandParams = function(_, line_to_cursor, matched_trigger, _)
                local context = line_to_cursor:sub(1, #line_to_cursor - #matched_trigger)
                local type_name = extract_builder_type(context)
                if not type_name then
                    return nil
                end
                return {
                    env_override = {
                        BUILDER_TYPE = { type_name },
                    },
                }
            end,
        }, {
            f(function(_, snip)
                local type_name = snip.env.BUILDER_TYPE and snip.env.BUILDER_TYPE[1]
                if not type_name then
                    local line = vim.api.nvim_get_current_line()
                    local col = vim.api.nvim_win_get_cursor(0)[2]
                    type_name = extract_builder_type(line:sub(1, col))
                end
                return type_name and (type_name .. ".builder().") or ""
            end),
            i(1),
            t(".build();"),
        }, {
            condition = is_builder_context,
            show_condition = show_builder_context,
        }),
        -- postfix({
        --     trig = ".bb",
        --     dscr = "Type.builder().<methods>.build()",
        --     docstring = "Type.builder().$1.build()",
        -- }, {
        --     f(function(_, snip)
        --         return snip.env.POSTFIX_MATCH .. ".builder()."
        --     end),
        --     i(1),
        --     t(".build()"),
        -- }),
    }
end

return M