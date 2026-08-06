local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-mapping-method", function()
    local resolver
    local state
    local path_requests

    --- Configure Java Tree-sitter test nodes for a mapper method and its owner.
    ---@param owner_kind string
    ---@param method_row integer
    ---@param owner_end_row integer
    local function stub_java_tree(owner_kind, method_row, owner_end_row)
        local owner = {
            type = function()
                return owner_kind
            end,
            start = function()
                return 0, 0
            end,
            range = function()
                return 0, 0, owner_end_row, 1
            end,
            parent = function()
                return nil
            end,
        }
        local method = {
            type = function()
                return "method_declaration"
            end,
            start = function()
                return method_row, 4
            end,
            parent = function()
                return owner
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
        _, state = helper.reset_vim()
        path_requests = {}
        helper.stub_module("modules.java.mapstruct", {
            get_method_types = function(_, callback)
                callback({
                    sources = { { name = "source", type = "example.Source" } },
                    target_type = "example.Target",
                })
            end,
            resolve_path_type = function(params, callback)
                path_requests[#path_requests + 1] = vim.deepcopy(params)
                if params.sources[1].name == "$target" then
                    callback({ className = "long", simpleName = "long", packageName = "" })
                else
                    callback({ className = "java.time.Duration", simpleName = "Duration", packageName = "java.time" })
                end
            end,
        })

        state.buffer_options[1] = {
            expandtab = true,
            shiftwidth = 4,
            tabstop = 4,
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

        resolver = helper.reload("modules.java.diagnostics-resolver.mapstruct-mapping-method")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.diagnostics-resolver.java-context",
            "modules.java.diagnostics-resolver.java-import-resolver",
            "modules.java.diagnostics-resolver.mapstruct-method-type-resolver",
            "modules.java.diagnostics-resolver.mapstruct-mapping-method",
            "modules.java.mapstruct",
        })
    end)

    it("parses the mapping method suggested by MapStruct", function()
        -- given
        local message = 'Can\'t map property "Duration ttl" to "long ttl". '
            .. 'Consider to declare/implement a mapping method: "long map(Duration value)"'

        -- when
        local suggested = resolver.parse_suggested_method(message)

        -- then
        assert.are.same({
            signature = "long map(Duration value)",
            return_type = "long",
            name = "map",
            parameters = "(Duration value)",
            parameter_type = "Duration",
            parameter_name = "value",
            source_type = "Duration",
            source_property = "ttl",
            target_type = "long",
            target_property = "ttl",
        }, suggested)
    end)

    it("inserts a protected mapping method into an abstract mapper class", function()
        -- given
        state.buffer_lines[1] = {
            "package example;",
            "",
            "import example.Source;",
            "import example.Target;",
            "",
            "public abstract class FooMapper {",
            "",
            "    public abstract Target map(Source source);",
            "}",
        }
        stub_java_tree("class_declaration", 7, 8)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 7,
                col = 30,
                message = 'Can\'t map property "Duration ttl" to "long ttl". '
                    .. 'Consider to declare/implement a mapping method: "long map(Duration value)"',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.same({
            "package example;",
            "",
            "import example.Source;",
            "import example.Target;",
            "",
            "import java.time.Duration;",
            "",
            "public abstract class FooMapper {",
            "",
            "    public abstract Target map(Source source);",
            "",
            "    protected long map(Duration value) {",
            "        return ;",
            "    }",
            "}",
        }, state.buffer_lines[1])
        assert.are.same({ 13, 15 }, state.cursor)
        assert.are.equal("startinsert", state.commands[#state.commands])
        assert.are.equal(2, #path_requests)
        assert.are.equal("ttl.", path_requests[1].path_expression)
        assert.are.equal("source", path_requests[1].sources[1].name)
        assert.are.equal("ttl.", path_requests[2].path_expression)
        assert.are.equal("$target", path_requests[2].sources[1].name)
    end)

    it("uses a default method for a mapper interface", function()
        -- given
        state.buffer_lines[1] = {
            "package example;",
            "",
            "import example.Source;",
            "import example.Target;",
            "",
            "public interface FooMapper {",
            "    Target map(Source source);",
            "}",
        }
        stub_java_tree("interface_declaration", 6, 7)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 6,
                col = 20,
                message = 'Can\'t map property "Duration ttl" to "long ttl". '
                    .. 'Consider to declare/implement a mapping method: "long map(Duration value)"',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.equal("    default long map(Duration value) {", state.buffer_lines[1][11])
    end)
end)
