local helper = require("tests.utils.spec_helper")

describe("modules.java.diagnostics-resolver.mapstruct-mapping-method", function()
    local resolver
    local state
    local resolve_imports_calls
    local resolve_imports_cursor
    local resolve_imports_delay

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
        resolve_imports_calls = 0
        resolve_imports_cursor = nil
        resolve_imports_delay = nil
        vim.defer_fn = function(callback, delay)
            resolve_imports_delay = delay
            callback()
        end
        helper.stub_module("utils.lang.java.lsp-java", {
            resolve_imports = function()
                resolve_imports_calls = resolve_imports_calls + 1
                resolve_imports_cursor = vim.deepcopy(state.cursor)
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
            "modules.java.diagnostics-resolver.mapstruct-mapping-method",
            "utils.lang.java.lsp-java",
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
        }, suggested)
    end)

    it("inserts a protected mapping method into an abstract mapper class", function()
        -- given
        state.buffer_lines[1] = {
            "public abstract class FooMapper {",
            "",
            "    public abstract Target map(Source source);",
            "}",
        }
        stub_java_tree("class_declaration", 2, 3)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 2,
                col = 30,
                message = 'Can\'t map property "Duration ttl" to "long ttl". '
                    .. 'Consider to declare/implement a mapping method: "long map(Duration value)"',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.same({
            "public abstract class FooMapper {",
            "",
            "    public abstract Target map(Source source);",
            "",
            "    protected long map(Duration value) {",
            "        return ;",
            "    }",
            "}",
        }, state.buffer_lines[1])
        assert.are.same({ 6, 15 }, state.cursor)
        assert.are.equal("startinsert", state.commands[#state.commands])
        assert.are.equal(1, resolve_imports_calls)
        assert.are.same({ 5, 23 }, resolve_imports_cursor)
        assert.are.equal(250, resolve_imports_delay)
    end)

    it("uses a default method for a mapper interface", function()
        -- given
        state.buffer_lines[1] = {
            "public interface FooMapper {",
            "    Target map(Source source);",
            "}",
        }
        stub_java_tree("interface_declaration", 1, 2)

        -- when
        local resolved = resolver.resolve({
            bufnr = 1,
            diagnostic = {
                lnum = 1,
                col = 20,
                message = 'Can\'t map property "Duration ttl" to "long ttl". '
                    .. 'Consider to declare/implement a mapping method: "long map(Duration value)"',
            },
        })

        -- then
        assert.is_true(resolved)
        assert.are.equal("    default long map(Duration value) {", state.buffer_lines[1][4])
    end)
end)
