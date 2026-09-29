local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-unmapped-target", function()
    local resolver
    local state

    before_each(function()
        _, state = helper.reset_vim()
        helper.stub_module("utils.nio-util", {
            run = function(callback)
                callback()
            end,
            multi_select = function(items)
                return { items[1] }
            end,
        })
        resolver = helper.reload("modules.java.diagnostics-resolver.mapstruct-unmapped-target")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver.java-context",
            "modules.java.diagnostics-resolver.mapstruct-unmapped-target",
            "utils.nio-util",
        })
    end)

    it("parses all property names from a MapStruct diagnostic", function()
        -- given
        local message = 'Unmapped target properties: "instructionType, originalUetr, hiddenDebtor"'

        -- when
        local properties = resolver.parse_properties(message)

        -- then
        assert.are.same({ "instructionType", "originalUetr", "hiddenDebtor" }, properties)
    end)

    it("parses a singular property from a MapStruct diagnostic", function()
        -- given
        local message = 'Unmapped target property: "instructedAmount"'

        -- when
        local properties = resolver.parse_properties(message)

        -- then
        assert.are.same({ "instructedAmount" }, properties)
    end)

    it("builds all and per-property choices in the expected order", function()
        -- when
        local choices = resolver.build_choices({ "instructionType", "originalUetr" })

        -- then
        assert.are.same({
            {
                name = "Add all unmapped target properties",
                kind = "map_all",
                chunks = { { "Add", "DiagnosticOk" }, { " all unmapped target properties" } },
            },
            {
                name = "Add unmapped target property: instructionType",
                kind = "map",
                property = "instructionType",
                chunks = {
                    { "Add", "DiagnosticOk" },
                    { " unmapped target property: " },
                    { "instructionType", "Identifier" },
                },
            },
            {
                name = "Add unmapped target property: originalUetr",
                kind = "map",
                property = "originalUetr",
                chunks = {
                    { "Add", "DiagnosticOk" },
                    { " unmapped target property: " },
                    { "originalUetr", "Identifier" },
                },
            },
            {
                name = "Ignore all unmapped target properties",
                kind = "ignore_all",
                chunks = { { "Ignore", "DiagnosticWarn" }, { " all unmapped target properties" } },
            },
            {
                name = "Ignore unmapped target property: instructionType",
                kind = "ignore",
                property = "instructionType",
                chunks = {
                    { "Ignore", "DiagnosticWarn" },
                    { " unmapped target property: " },
                    { "instructionType", "Identifier" },
                },
            },
            {
                name = "Ignore unmapped target property: originalUetr",
                kind = "ignore",
                property = "originalUetr",
                chunks = {
                    { "Ignore", "DiagnosticWarn" },
                    { " unmapped target property: " },
                    { "originalUetr", "Identifier" },
                },
            },
        }, choices)
    end)

    it("renders ignore and source mapping annotations", function()
        -- then
        assert.are.equal(
            '@Mapping(target = "hiddenDebtor", ignore = true)',
            resolver.annotation_line("hiddenDebtor", "ignore")
        )
        assert.are.equal(
            '@Mapping(target = "hiddenDebtor", source = "")',
            resolver.annotation_line("hiddenDebtor", "map")
        )
    end)

    it("inserts selected add-all mappings above the diagnostic method", function()
        -- given
        state.buffer_lines[1] = {
            "package com.acme;",
            "",
            "import org.mapstruct.Mapper;",
            "",
            "@Mapper",
            "public interface FooMapper {",
            "    Target map(Source source);",
            "}",
        }

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

        local method = {
            type = function()
                return "method_declaration"
            end,
            start = function()
                return 6, 4
            end,
            parent = function()
                return nil
            end,
        }
        vim.treesitter = {
            get_parser = function()
                return {
                    parse = function()
                        return {
                            {
                                root = function()
                                    return {
                                        named_descendant_for_range = function()
                                            return method
                                        end,
                                    }
                                end,
                            },
                        }
                    end,
                }
            end,
        }

        -- when
        resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 6,
                col = 10,
                message = 'Unmapped target properties: "first, second"',
            },
        })

        -- then
        assert.are.same({
            "package com.acme;",
            "",
            "import org.mapstruct.Mapper;",
            "import org.mapstruct.Mapping;",
            "",
            "@Mapper",
            "public interface FooMapper {",
            '    @Mapping(target = "first", source = "")',
            '    @Mapping(target = "second", source = "")',
            "    Target map(Source source);",
            "}",
        }, state.buffer_lines[1])
    end)

    describe("with a mapper buffer", function()
        local selection

        --- Configure the buffer reader and a Tree-sitter method node at a row.
        ---@param method_row integer
        local function stub_mapper(method_row)
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

            local method = {
                type = function()
                    return "method_declaration"
                end,
                start = function()
                    return method_row, 4
                end,
                parent = function()
                    return nil
                end,
            }
            vim.treesitter = {
                get_parser = function()
                    return {
                        parse = function()
                            return {
                                {
                                    root = function()
                                        return {
                                            named_descendant_for_range = function()
                                                return method
                                            end,
                                        }
                                    end,
                                },
                            }
                        end,
                    }
                end,
            }
        end

        before_each(function()
            selection = "map_all"
            helper.stub_module("utils.nio-util", {
                run = function(callback)
                    callback()
                end,
                multi_select = function(items)
                    for _, item in ipairs(items) do
                        if item.kind == selection then
                            return { item }
                        end
                    end
                end,
            })
            resolver = helper.reload("modules.java.diagnostics-resolver.mapstruct-unmapped-target")
        end)

        it("keeps the Mapping import out of the static import group", function()
            -- given
            state.buffer_lines[1] = {
                "package com.acme;",
                "",
                "import com.acme.dto.Target;",
                "import org.mapstruct.Mapper;",
                "",
                "import java.util.List;",
                "",
                "import static org.mapstruct.MappingConstants.ComponentModel.SPRING;",
                "",
                "@Mapper(componentModel = SPRING)",
                "public interface FooMapper {",
                "    Target map(Source source);",
                "}",
            }
            stub_mapper(11)

            -- when
            resolver.resolve({
                bufnr = 1,
                diagnostic = { lnum = 11, col = 10, message = 'Unmapped target properties: "first, second"' },
            })

            -- then
            assert.are.same({
                "package com.acme;",
                "",
                "import com.acme.dto.Target;",
                "import org.mapstruct.Mapper;",
                "import org.mapstruct.Mapping;",
                "",
                "import java.util.List;",
                "",
                "import static org.mapstruct.MappingConstants.ComponentModel.SPRING;",
                "",
                "@Mapper(componentModel = SPRING)",
                "public interface FooMapper {",
                '    @Mapping(target = "first", source = "")',
                '    @Mapping(target = "second", source = "")',
                "    Target map(Source source);",
                "}",
            }, state.buffer_lines[1])
        end)

        it("does not import Mapping again when it is already available", function()
            -- given
            state.buffer_lines[1] = {
                "package com.acme;",
                "",
                "import org.mapstruct.*;",
                "",
                "public interface FooMapper {",
                "    Target map(Source source);",
                "}",
            }
            stub_mapper(5)

            -- when
            resolver.resolve({
                bufnr = 1,
                diagnostic = { lnum = 5, col = 10, message = 'Unmapped target property: "first"' },
            })

            -- then
            assert.are.same({
                "package com.acme;",
                "",
                "import org.mapstruct.*;",
                "",
                "public interface FooMapper {",
                '    @Mapping(target = "first", source = "")',
                "    Target map(Source source);",
                "}",
            }, state.buffer_lines[1])
        end)

        it("qualifies the annotation when another Mapping type is imported", function()
            -- given
            selection = "ignore_all"
            state.buffer_lines[1] = {
                "package com.acme;",
                "",
                "import com.other.Mapping;",
                "import org.mapstruct.Mapper;",
                "",
                "public interface FooMapper {",
                "    Target map(Source source);",
                "}",
            }
            stub_mapper(6)

            -- when
            resolver.resolve({
                bufnr = 1,
                diagnostic = { lnum = 6, col = 10, message = 'Unmapped target property: "first"' },
            })

            -- then
            assert.are.same({
                "package com.acme;",
                "",
                "import com.other.Mapping;",
                "import org.mapstruct.Mapper;",
                "",
                "public interface FooMapper {",
                '    @org.mapstruct.Mapping(target = "first", ignore = true)',
                "    Target map(Source source);",
                "}",
            }, state.buffer_lines[1])
        end)

        it("places the cursor inside the first empty source", function()
            -- given
            state.buffer_lines[1] = {
                "package com.acme;",
                "",
                "import org.mapstruct.Mapper;",
                "import org.mapstruct.Mapping;",
                "",
                "public interface FooMapper {",
                "    Target map(Source source);",
                "}",
            }
            stub_mapper(6)

            -- when
            resolver.resolve({
                bufnr = 1,
                diagnostic = { lnum = 6, col = 10, message = 'Unmapped target properties: "first, second"' },
            })

            -- then
            local line = state.buffer_lines[1][7]
            assert.are.equal('    @Mapping(target = "first", source = "")', line)
            assert.are.same({ 7, #line - 2 }, state.cursor)
            assert.are.equal("startinsert", state.commands[#state.commands])
        end)

        it("leaves the cursor alone when every property is ignored", function()
            -- given
            selection = "ignore_all"
            state.cursor = { 7, 11 }
            state.buffer_lines[1] = {
                "package com.acme;",
                "",
                "import org.mapstruct.Mapper;",
                "import org.mapstruct.Mapping;",
                "",
                "public interface FooMapper {",
                "    Target map(Source source);",
                "}",
            }
            stub_mapper(6)

            -- when
            resolver.resolve({
                bufnr = 1,
                diagnostic = { lnum = 6, col = 10, message = 'Unmapped target properties: "first, second"' },
            })

            -- then
            assert.are.same({ 7, 11 }, state.cursor)
            assert.are.equal(0, #state.commands)
        end)
    end)
end)
