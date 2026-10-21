local helper = require("tests.utils.spec_helper")

describe("utils.java.lombok-builder-navigation", function()
    local navigation
    local state
    local client

    -- Library class as jdtls serves it from a sources jar: every Lombok member lands on an annotation.
    local account_id = {
        "package demo;",
        "",
        "/**",
        " * Customer identification.",
        " */",
        "@Value",
        "@Builder",
        "public class AccountId {",
        "    @NonNull",
        "    Type type;",
        "    String name;",
        "    @NonNull",
        "    String iban;",
        "    SchemeName schemeName;",
        "",
        "    public static class AccountIdBuilder {",
        "        public Type getType() {",
        "            return this.type;",
        "        }",
        "    }",
        "",
        "    @Getter",
        "    @RequiredArgsConstructor",
        "    public enum SchemeName {",
        "        RNRCT(PRIVATE), PSPT(PRIVATE),",
        "        USRC(ORGANIZATION);",
        "",
        "        private final Type type;",
        "    }",
        "}",
    }

    -- In-project class: multi-line @Builder arguments, braces and parentheses inside strings.
    local value_class = {
        "package demo;",
        "",
        "@Value",
        "@Builder(",
        "    toBuilder = true,",
        '    builderMethodName = "with(x"',
        ")",
        "public class ValueClass {",
        '    String json = "{";',
        "    String iban;",
        "    boolean active;",
        "    boolean isLegacy;",
        "    @Singular",
        "    List<String> items;",
        "    @Size(min = 1)",
        "    List<Map<String, Integer>> entries = new ArrayList<>();",
        "    @Size(",
        "        min = 1",
        "    )",
        "    String code;",
        "}",
    }

    --- Position the test cursor on a token within the supplied source line.
    ---@param line string
    ---@param token string
    ---@param row? integer
    local function set_cursor_on(line, token, row)
        local start_idx = assert(line:find(token, 1, true))
        state.current_line = line
        state.cursor = { row or 1, start_idx - 1 }
    end

    --- Return the zero-based byte column of an identifier in a line.
    ---@param line string
    ---@param token string
    ---@return integer
    local function col_of(line, token)
        return assert(line:find("%f[%w_$]" .. token .. "%f[^%w_$]")) - 1
    end

    --- Configure a synthetic destination buffer (bufnr 2) and the window APIs used to jump into it.
    ---@param lines string[]
    local function configure_target_buffer(lines)
        state.valid_windows = { [state.current_win] = true }
        state.buffer_lines[2] = lines

        vim.api.nvim_buf_get_lines = function(bufnr, start_row, end_row)
            local buffer_lines = state.buffer_lines[bufnr] or {}
            local last_row = end_row == -1 and #buffer_lines or end_row
            local selected = {}
            for idx = start_row + 1, last_row do
                table.insert(selected, buffer_lines[idx])
            end
            return selected
        end
        vim.api.nvim_win_call = function(_, callback)
            callback()
        end
        vim.fn.settagstack = function(win, stack, action)
            state.tagstack = { win = win, stack = stack, action = action }
        end
        vim.fn.win_getid = function(win)
            return win
        end
    end

    --- Answer the latest captured LSP request with locations the `locations_to_items` stub unwraps.
    ---@param items table[]
    local function respond(items)
        local request = assert(state.requests[#state.requests], "no pending LSP request")
        local locations = {}
        for _, item in ipairs(items) do
            table.insert(locations, { item = item })
        end
        request.callback({ [7] = { result = locations } })
    end

    --- Build a single definition hit on a line of the target buffer.
    ---@param lnum integer
    ---@param col? integer 1-based column
    ---@return table[]
    local function landing(lnum, col)
        return { { bufnr = 2, filename = "/workspace/Target.java", lnum = lnum, col = col or 1 } }
    end

    --- Run `gd` on the configured source line and answer the definition request with one hit.
    ---@param lines string[] target buffer
    ---@param lnum integer landing line in the target buffer
    ---@param col? integer 1-based landing column
    local function navigate_to(lines, lnum, col)
        configure_target_buffer(lines)
        assert.is_true(navigation.goto_definition({ filetype = "java", bufnr = 1 }))
        assert.are.equal(1, #state.requests)
        assert.are.equal("textDocument/definition", state.requests[1].method)
        respond(landing(lnum, col))
        assert.are.same({ win = state.current_win, bufnr = 2 }, state.window_set_buf)
    end

    before_each(function()
        _, state = helper.reset_vim()
        client = { offset_encoding = "utf-16" }
        state.requests = {}

        vim.str_utfindex = function(_, _, byte_col)
            return byte_col
        end
        vim.lsp.get_clients = function()
            return { client }
        end
        vim.lsp.get_client_by_id = function(client_id)
            return client_id == 7 and client or nil
        end
        vim.lsp.buf_request_all = function(bufnr, method, params, callback)
            table.insert(state.requests, {
                bufnr = bufnr,
                method = method,
                params = params(client),
                callback = callback,
            })
        end
        vim.lsp.util.make_text_document_params = function(bufnr)
            return { uri = "file:///buffer/" .. bufnr }
        end
        vim.lsp.util.locations_to_items = function(locations)
            local items = {}
            for _, location in ipairs(locations) do
                table.insert(items, location.item)
            end
            return items
        end
        navigation = helper.reload("utils.java.lombok-builder-navigation")
    end)

    after_each(function()
        helper.unload("utils.java.lombok-builder-navigation")
    end)

    describe("target detection", function()
        it("requests the definition of a member invocation at the cursor", function()
            -- given
            local lines = {
                "var request = PacsRequestContext.builder()",
                '        .source("instant")',
                "        .build();",
            }
            state.buffer_lines[1] = lines
            set_cursor_on(lines[2], "source", 2)

            -- when
            local handled = navigation.goto_definition({ filetype = "java", bufnr = 1 })

            -- then
            assert.is_true(handled)
            assert.are.equal("textDocument/definition", state.requests[1].method)
            -- the request targets the last character of the member name (see the module comment)
            assert.are.same(
                { line = 1, character = col_of(lines[2], "source") + #"source" - 1 },
                state.requests[1].params.position
            )
        end)

        it("leaves builder lifecycle and Object methods to the LSP fallback", function()
            for _, line in ipairs({
                "        .build();",
                "var b = AccountId.builder();",
                "var b = debtor.toBuilder();",
                "String s = debtor.toString();",
                "boolean eq = debtor.equals(other);",
            }) do
                set_cursor_on(line, line:match("%.(%a+)%("))
                assert.is_false(navigation.goto_definition({ filetype = "java", bufnr = 1 }), line)
            end
            assert.are.same({}, state.requests)
        end)

        it("leaves identifiers that are not member invocations to the LSP fallback", function()
            local line = "String iban = debtor.getIban();"
            set_cursor_on(line, "debtor")
            assert.is_false(navigation.goto_definition({ filetype = "java", bufnr = 1 }))

            set_cursor_on(line, "iban")
            assert.is_false(navigation.goto_definition({ filetype = "java", bufnr = 1 }))

            assert.is_false(navigation.goto_definition({ filetype = "kotlin", bufnr = 1 }))
            assert.are.same({}, state.requests)
        end)
    end)

    describe("library classes", function()
        it("redirects a @Value getter from the class annotation to its field", function()
            -- given
            local line = "        String iban = debtor.getIban();"
            set_cursor_on(line, "getIban")

            -- when
            navigate_to(account_id, 6)

            -- then
            assert.are.same({ 13, col_of("    String iban;", "iban") }, state.cursor)
            assert.is_table(state.tagstack)
        end)

        it("prefers the field of the annotated type over same-named members of nested types", function()
            -- given
            set_cursor_on("AccountId.Type t = a.getType();", "getType")

            -- when
            navigate_to(account_id, 6)

            -- then
            assert.are.same({ 10, col_of("    Type type;", "type") }, state.cursor)
        end)

        it("resolves a nested enum getter inside the enum body", function()
            -- given
            set_cursor_on("AccountId.Type st = sn.getType();", "getType")

            -- when
            navigate_to(account_id, 22)

            -- then
            assert.are.same({ 28, col_of("        private final Type type;", "type") }, state.cursor)
        end)

        it("redirects a @Builder setter of a library class to its field", function()
            -- given
            set_cursor_on('AccountId a3 = AccountId.builder().name("n").build();', "name")

            -- when
            navigate_to(account_id, 7)

            -- then
            assert.are.same({ 11, col_of("    String name;", "name") }, state.cursor)
        end)

        it("stays on the annotation when no field matches the member", function()
            -- given
            set_cursor_on("Object o = a.foo();", "foo")

            -- when
            navigate_to(account_id, 6)

            -- then
            assert.are.same({ 6, 0 }, state.cursor)
        end)
    end)

    describe("builders", function()
        it("skips multi-line annotation arguments and string braces when locating the field", function()
            -- given
            set_cursor_on('ValueClass v2 = ValueClass.builder().iban("x").build();', "iban")

            -- when
            navigate_to(value_class, 4)

            -- then
            assert.are.same({ 10, col_of("    String iban;", "iban") }, state.cursor)
        end)

        it("ignores multi-line annotation arguments inside the class body", function()
            -- given: `min = 1` on its own line must not be taken for a field named `min`
            set_cursor_on("builder.min(1)", "min")
            navigate_to(value_class, 4)
            assert.are.same({ 4, 0 }, state.cursor)

            state.requests = {}
            set_cursor_on('builder.code("c")', "code")
            navigate_to(value_class, 4)
            assert.are.same({ 20, col_of("    String code;", "code") }, state.cursor)
        end)

        it("resolves @Singular builder methods to the plural collection field", function()
            -- given
            set_cursor_on('builder.item("a")', "item")
            navigate_to(value_class, 4)
            assert.are.same({ 14, col_of("    List<String> items;", "items") }, state.cursor)

            -- and: `y` -> `ies`
            state.requests = {}
            set_cursor_on('builder.entry("e")', "entry")
            navigate_to(value_class, 4)
            assert.are.same(
                { 16, col_of("    List<Map<String, Integer>> entries = new ArrayList<>();", "entries") },
                state.cursor
            )
        end)

        it("keeps the is-prefixed spelling of boolean fields for builder setters", function()
            -- given
            set_cursor_on("builder.isLegacy(true)", "isLegacy")

            -- when
            navigate_to(value_class, 4)

            -- then
            assert.are.same({ 12, col_of("    boolean isLegacy;", "isLegacy") }, state.cursor)
        end)

        it("redirects @SuperBuilder setters to the field of the owning class", function()
            -- given
            local super_base = {
                "@Getter",
                "@SuperBuilder",
                "public class SuperBase {",
                "    private String baseField;",
                "}",
            }
            set_cursor_on('SuperChild sc2 = SuperChild.builder().baseField("b").childField("c").build();', "baseField")

            -- when
            navigate_to(super_base, 2)

            -- then
            assert.are.same({ 4, col_of(super_base[4], "baseField") }, state.cursor)
        end)

        it("resolves builder setters of records to the record component", function()
            -- given
            local record = {
                "@Builder",
                "public record Point(int x, @NonNull String iban) {",
                "}",
            }
            set_cursor_on('Point p = Point.builder().iban("x").build();', "iban")

            -- when
            navigate_to(record, 1)

            -- then
            assert.are.same({ 2, col_of(record[2], "iban") }, state.cursor)
        end)
    end)

    describe("accessors", function()
        it("maps is-getters to the plain boolean field or its is-prefixed spelling", function()
            set_cursor_on("boolean b1 = v.isActive();", "isActive")
            navigate_to(value_class, 3)
            assert.are.same({ 11, col_of("    boolean active;", "active") }, state.cursor)

            state.requests = {}
            set_cursor_on("boolean b3 = v.isLegacy();", "isLegacy")
            navigate_to(value_class, 3)
            assert.are.same({ 12, col_of("    boolean isLegacy;", "isLegacy") }, state.cursor)
        end)

        it("redirects @With withers to the field", function()
            -- given
            local with_class = { "@Value", "@With", "public class WithClass {", "    String name;", "}" }
            set_cursor_on('WithClass w2 = w.withName("z");', "withName")

            -- when
            navigate_to(with_class, 2)

            -- then
            assert.are.same({ 4, col_of(with_class[4], "name") }, state.cursor)
        end)

        it("redirects @Accessors(fluent = true) getters and setters to the field", function()
            local fluent = {
                "@Getter",
                "@Setter",
                "@Accessors(fluent = true)",
                "public class FluentClass {",
                "    private String name;",
                "}",
            }

            set_cursor_on("String s7 = f.name();", "name")
            navigate_to(fluent, 1)
            assert.are.same({ 5, col_of(fluent[5], "name") }, state.cursor)

            state.requests = {}
            set_cursor_on('f.name("q");', "name")
            navigate_to(fluent, 2)
            assert.are.same({ 5, col_of(fluent[5], "name") }, state.cursor)
        end)

        it("resolves @Accessors(prefix) fields through the relaxed name match", function()
            -- given
            local prefixed = {
                "@Getter",
                '@Accessors(prefix = "m")',
                "public class PrefixClass {",
                "    private String other;",
                "    private String mName;",
                "}",
            }
            set_cursor_on("String s8 = p.getName();", "getName")

            -- when
            navigate_to(prefixed, 1)

            -- then
            assert.are.same({ 5, col_of(prefixed[5], "mName") }, state.cursor)
        end)

        it("jumps from a field-level annotation on its own line to the field below", function()
            -- given
            local field_level = {
                "public class FieldLevelClass {",
                "    @Getter @Setter private String name;",
                "    @Getter",
                "    private String other;",
                "}",
            }
            set_cursor_on("String s6 = fl.getOther();", "getOther")

            -- when
            navigate_to(field_level, 3)

            -- then
            assert.are.same({ 4, col_of(field_level[4], "other") }, state.cursor)
        end)

        it("keeps the position when the definition already points at the annotated field", function()
            -- given
            local field_level = {
                "public class FieldLevelClass {",
                "    @Getter @Setter private String name;",
                "}",
            }
            set_cursor_on('fl.setName("y");', "setName")

            -- when
            navigate_to(field_level, 2, col_of(field_level[2], "name") + 1)

            -- then
            assert.are.same({ 2, col_of(field_level[2], "name") }, state.cursor)
        end)
    end)

    describe("method references", function()
        it("resolves builder method references through definition and falls back to the receiver type", function()
            -- given
            local line = "        .map(builder::balanceBooking)"
            set_cursor_on(line, "balanceBooking")
            configure_target_buffer({
                "@Value",
                "@Builder(toBuilder = true)",
                "public class TransactionInitiation {",
                "    Set<BalanceBookingType> balanceBooking;",
                "}",
            })

            -- when
            assert.is_true(navigation.goto_definition({ filetype = "java", bufnr = 1 }))
            assert.are.equal("textDocument/definition", state.requests[1].method)
            assert.are.same(
                { line = 0, character = col_of(line, "balanceBooking") + #"balanceBooking" - 1 },
                state.requests[1].params.position
            )
            respond({})

            -- then: jdtls had no answer for the generated method, so the receiver type is asked
            assert.are.equal(2, #state.requests)
            assert.are.equal("textDocument/typeDefinition", state.requests[2].method)
            assert.are.same({ line = 0, character = col_of(line, "builder") }, state.requests[2].params.position)

            respond(landing(2))
            assert.are.same({ win = state.current_win, bufnr = 2 }, state.window_set_buf)
            assert.are.same({ 4, 28 }, state.cursor)
            assert.is_table(state.tagstack)
        end)

        it("redirects method references on any receiver", function()
            -- given
            set_cursor_on("        .map(AccountId::getIban)", "getIban")

            -- when
            navigate_to(account_id, 6)

            -- then
            assert.are.same({ 13, col_of("    String iban;", "iban") }, state.cursor)
        end)

        it("requests single-character member references right after the name", function()
            -- given
            local line = "        .map(Point::x)"
            set_cursor_on(line, "x")

            -- when
            assert.is_true(navigation.goto_definition({ filetype = "java", bufnr = 1 }))

            -- then
            assert.are.same({ line = 0, character = col_of(line, "x") + 1 }, state.requests[1].params.position)
        end)

        it("reports missing locations for expression receivers without a type fallback", function()
            -- given
            set_cursor_on("Function<String, Builder> fn = v.toBuilder()::iban;", "iban")

            -- when
            assert.is_true(navigation.goto_definition({ filetype = "java", bufnr = 1 }))
            respond({})

            -- then
            assert.are.equal(1, #state.requests)
            assert.are.equal("No locations found", state.notifications[1].message)
        end)
    end)

    it("preserves normal navigation for a non-Lombok member invocation", function()
        -- given
        local target = {
            "public class BalanceBookingEvaluator {",
            "    public Result evaluate(Transaction transaction) {",
            "    }",
            "}",
        }
        set_cursor_on("return evaluator.evaluate(transaction);", "evaluate")

        -- when
        navigate_to(target, 2, col_of(target[2], "evaluate") + 1)

        -- then
        assert.are.same({ 2, col_of(target[2], "evaluate") }, state.cursor)
    end)

    it("lists multiple definitions in the quickfix window", function()
        -- given
        set_cursor_on("String s = debtor.getIban();", "getIban")
        configure_target_buffer(account_id)
        vim.fn.setqflist = function(_, action, options)
            state.qflist = { action = action, options = options }
        end

        -- when
        assert.is_true(navigation.goto_definition({ filetype = "java", bufnr = 1 }))
        respond({ landing(6)[1], landing(13)[1] })

        -- then
        assert.are.equal(2, #state.qflist.options.items)
        assert.is_nil(state.window_set_buf)
    end)
end)
