local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-nested-target-mappings", function()
    local module
    local state

    --- Describe an annotation that occupies one buffer row.
    ---@param row integer
    ---@param line string
    ---@param arguments { key: string, value: string }[] values as written, quotes included
    ---@return JavaAnnotation
    local function annotation(row, line, arguments)
        local start_col = line:find("@", 1, true) - 1
        local parsed = {}
        for _, argument in ipairs(arguments) do
            local pair_start = assert(line:find(argument.key .. " = " .. argument.value, 1, true))
            local value_index = pair_start + #argument.key + 3
            local is_string = argument.value:sub(1, 1) == '"'
            parsed[#parsed + 1] = {
                key = argument.key,
                value = is_string and argument.value:sub(2, -2) or argument.value,
                is_string = is_string,
                start_row = row,
                start_col = value_index - 1,
                end_row = row,
                end_col = value_index - 1 + #argument.value,
            }
        end
        return {
            name = "Mapping",
            start_row = row,
            start_col = start_col,
            end_row = row,
            end_col = #line,
            arguments = parsed,
        }
    end

    before_each(function()
        _, state = helper.reset_vim()
        vim.api.nvim_buf_get_lines = function(bufnr, start_row, end_row)
            local lines = state.buffer_lines[bufnr] or {}
            if end_row == -1 then
                end_row = #lines
            end

            local result = {}
            for index = start_row + 1, end_row do
                result[#result + 1] = lines[index]
            end
            return result
        end
        module = helper.reload("modules.java.diagnostics-resolver.mapstruct-nested-target-mappings")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver.java-context",
            "modules.java.diagnostics-resolver.mapstruct-nested-target-mappings",
        })
    end)

    it("lists the source prefixes shared by every nested mapping, longest first", function()
        -- then
        assert.are.same({ "doc.header", "doc" }, module.shared_prefixes({ "doc.header.text", "doc.header.code" }))
        assert.are.same({ "doc.header.text", "doc.header", "doc" }, module.shared_prefixes({ "doc.header.text" }))
        assert.are.same({ "doc" }, module.shared_prefixes({ "doc.header.text", "doc.footer" }))
        assert.are.same({}, module.shared_prefixes({ "doc.header.text", "other.text" }))
        assert.are.same({}, module.shared_prefixes({}))
    end)

    it("returns the source of the owning method's own mapping", function()
        -- given
        local direct = annotation(0, '    @Mapping(target = "info", source = "doc")', {
            { key = "target", value = '"info"' },
            { key = "source", value = '"doc"' },
        })

        -- then
        assert.are.equal("doc", module.direct_source({ property = "info", direct = direct, mappings = {} }))
        assert.is_nil(module.direct_source({ property = "info", mappings = {} }))
    end)

    it("renders moved annotations relative to the generated method", function()
        -- given
        state.buffer_lines[1] = {
            '    @Mapping(target = "info.text", source = "doc.header.text")',
            '    @Mapping(target = "info.whole", source = "doc.header")',
            '    @Mapping(target = "info.code", ignore = true)',
        }
        local lines = state.buffer_lines[1]
        local group = {
            property = "info",
            mappings = {
                {
                    annotation = annotation(0, lines[1], {
                        { key = "target", value = '"info.text"' },
                        { key = "source", value = '"doc.header.text"' },
                    }),
                    target = "info.text",
                    rest = "text",
                    source = "doc.header.text",
                },
                {
                    annotation = annotation(1, lines[2], {
                        { key = "target", value = '"info.whole"' },
                        { key = "source", value = '"doc.header"' },
                    }),
                    target = "info.whole",
                    rest = "whole",
                    source = "doc.header",
                },
                {
                    annotation = annotation(2, lines[3], {
                        { key = "target", value = '"info.code"' },
                        { key = "ignore", value = "true" },
                    }),
                    target = "info.code",
                    rest = "code",
                },
            },
            source_paths = { "doc.header.text", "doc.header" },
        }

        -- when
        local moved = module.moved_annotations(1, group, "doc.header", "info")

        -- then
        assert.are.same({
            { '@Mapping(target = "text", source = "text")' },
            { '@Mapping(target = "whole", source = "info")' },
            { '@Mapping(target = "code", ignore = true)' },
        }, moved)
    end)

    it("keeps the layout of a multi-line annotation", function()
        -- given
        state.buffer_lines[1] = {
            "    @Mapping(",
            '        target = "info.text",',
            '        source = "doc.header.text"',
            "    )",
        }
        local group = {
            property = "info",
            mappings = {
                {
                    annotation = {
                        name = "Mapping",
                        start_row = 0,
                        start_col = 4,
                        end_row = 3,
                        end_col = 5,
                        arguments = {
                            {
                                key = "target",
                                value = "info.text",
                                is_string = true,
                                start_row = 1,
                                start_col = 17,
                                end_row = 1,
                                end_col = 28,
                            },
                            {
                                key = "source",
                                value = "doc.header.text",
                                is_string = true,
                                start_row = 2,
                                start_col = 17,
                                end_row = 2,
                                end_col = 34,
                            },
                        },
                    },
                    target = "info.text",
                    rest = "text",
                    source = "doc.header.text",
                },
            },
            source_paths = { "doc.header.text" },
        }

        -- when
        local moved = module.moved_annotations(1, group, "doc.header", "info")

        -- then
        assert.are.same({
            { "@Mapping(", '        target = "text",', '        source = "text"', "    )" },
        }, moved)
    end)

    it("replaces the nested annotations by one mapping of the shared source", function()
        -- given
        state.buffer_lines[1] = {
            '    @Mapping(target = "other", source = "doc.other")',
            '    @Mapping(target = "info.text", source = "doc.header.text")',
            '    @Mapping(target = "info.code", ignore = true)',
            "    Target map(Doc doc);",
        }
        local lines = state.buffer_lines[1]
        local group = {
            property = "info",
            mappings = {
                {
                    annotation = annotation(1, lines[2], {
                        { key = "target", value = '"info.text"' },
                        { key = "source", value = '"doc.header.text"' },
                    }),
                    target = "info.text",
                    rest = "text",
                    source = "doc.header.text",
                },
                {
                    annotation = annotation(2, lines[3], {
                        { key = "target", value = '"info.code"' },
                        { key = "ignore", value = "true" },
                    }),
                    target = "info.code",
                    rest = "code",
                },
            },
            source_paths = { "doc.header.text" },
        }

        -- when
        local removed = module.regroup_owning_method(1, group, "doc.header")

        -- then
        assert.are.equal(1, removed)
        assert.are.same({
            '    @Mapping(target = "other", source = "doc.other")',
            '    @Mapping(target = "info", source = "doc.header")',
            "    Target map(Doc doc);",
        }, state.buffer_lines[1])
    end)

    it("retargets the owning method's own mapping to the shared source", function()
        -- given
        state.buffer_lines[1] = {
            '    @Mapping(target = "info.text", source = "doc.header.text")',
            '    @Mapping(target = "info", source = "doc", qualifiedByName = "info")',
            "    Target map(Doc doc);",
        }
        local lines = state.buffer_lines[1]
        local group = {
            property = "info",
            direct = annotation(1, lines[2], {
                { key = "target", value = '"info"' },
                { key = "source", value = '"doc"' },
                { key = "qualifiedByName", value = '"info"' },
            }),
            mappings = {
                {
                    annotation = annotation(0, lines[1], {
                        { key = "target", value = '"info.text"' },
                        { key = "source", value = '"doc.header.text"' },
                    }),
                    target = "info.text",
                    rest = "text",
                    source = "doc.header.text",
                },
            },
            source_paths = { "doc.header.text" },
        }

        -- when
        local removed = module.regroup_owning_method(1, group, "doc.header")

        -- then
        assert.are.equal(1, removed)
        assert.are.same({
            '    @Mapping(target = "info", source = "doc.header", qualifiedByName = "info")',
            "    Target map(Doc doc);",
        }, state.buffer_lines[1])
    end)

    it("leaves an owning mapping of the shared source untouched", function()
        -- given
        state.buffer_lines[1] = {
            '    @Mapping(target = "info", source = "doc.header")',
            '    @Mapping(target = "info.text", source = "doc.header.text")',
            "    Target map(Doc doc);",
        }
        local lines = state.buffer_lines[1]
        local group = {
            property = "info",
            direct = annotation(0, lines[1], {
                { key = "target", value = '"info"' },
                { key = "source", value = '"doc.header"' },
            }),
            mappings = {
                {
                    annotation = annotation(1, lines[2], {
                        { key = "target", value = '"info.text"' },
                        { key = "source", value = '"doc.header.text"' },
                    }),
                    target = "info.text",
                    rest = "text",
                    source = "doc.header.text",
                },
            },
            source_paths = { "doc.header.text" },
        }

        -- when
        local removed = module.regroup_owning_method(1, group, "doc.header")

        -- then
        assert.are.equal(1, removed)
        assert.are.same({
            '    @Mapping(target = "info", source = "doc.header")',
            "    Target map(Doc doc);",
        }, state.buffer_lines[1])
    end)

    it("refuses to move an annotation that shares its line with other code", function()
        -- given
        state.buffer_lines[1] = {
            '    @Named("x") @Mapping(target = "info.text", source = "doc.header.text")',
        }
        local line = state.buffer_lines[1][1]
        local shared = annotation(0, line, {
            { key = "target", value = '"info.text"' },
            { key = "source", value = '"doc.header.text"' },
        })
        shared.start_col = line:find("@Mapping", 1, true) - 1

        -- when
        local movable, reason = module.movable(1, {
            property = "info",
            mappings = { { annotation = shared, target = "info.text", rest = "text", source = "doc.header.text" } },
            source_paths = { "doc.header.text" },
        })

        -- then
        assert.is_false(movable)
        assert.are.equal("the annotation on line 1 shares its line with other code", reason)
    end)
end)
