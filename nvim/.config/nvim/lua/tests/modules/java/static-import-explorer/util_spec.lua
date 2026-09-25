local helper = require("tests.utils.spec_helper")

describe("modules.java.static-import-explorer.util", function()
    local util
    local state
    local java_common
    local dep_search
    local existing_dirs
    local fqcn_calls

    before_each(function()
        _, state = helper.reset_vim()
        existing_dirs = {}
        fqcn_calls = {}

        java_common = {
            get_buffer_project_path = function()
                return "/repo/module"
            end,
            is_test_file = function()
                return true
            end,
            file_to_fqcn = function(file, line_num)
                table.insert(fqcn_calls, { file = file, line_num = line_num })
                if file:match("Assertions%.java$") then
                    return "org.assertj.core.api.Assertions"
                end
                if file:match("Local%.java$") then
                    return "Local"
                end
                return nil
            end,
        }
        dep_search = {
            coord_match_path = function(path)
                return path:gsub("org%.assertj", "org/assertj")
            end,
            is_loaded = function()
                return true
            end,
            get_source_dirs = function(scope)
                return scope == "test" and { "/deps/filtered-test" } or { "/deps/filtered-main" }
            end,
            get_source_dirs_all = function(scope)
                return scope == "test" and { "/deps/all-test" } or { "/deps/all-main" }
            end,
        }

        vim.fn.isdirectory = function(path)
            return existing_dirs[path] and 1 or 0
        end

        helper.stub_module("utils.java.java-common", java_common)
        helper.stub_module("modules.java.dependencies-search", dep_search)
        util = helper.reload("modules.java.static-import-explorer.util")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.static-import-explorer.util",
            "modules.java.dependencies-search",
            "utils.java.java-common",
        })
    end)

    it("converts dependency coordinates to path patterns and filters matching source dirs", function()
        -- given
        local patterns = util.to_dep_patterns({ "org.assertj:assertj-core", "com.acme" })
        local source_dirs = {
            "/m2/repository/org/assertj/assertj-core/3.25.1/assertj-core-3.25.1-sources",
            "/gradle/caches/modules-2/files-2.1/org.assertj/assertj-core/3.25.1/hash/assertj-core-3.25.1-sources",
            "/m2/repository/org/junit/junit-jupiter-api/5.10.0/junit-jupiter-api-5.10.0-sources",
        }

        -- when
        local filtered = util.filter_dirs_by_patterns(source_dirs, patterns)

        -- then
        assert.are.same({
            "/m2/repository/org/assertj/assertj-core/3.25.1/assertj-core-3.25.1-sources",
            "/gradle/caches/modules-2/files-2.1/org.assertj/assertj-core/3.25.1/hash/assertj-core-3.25.1-sources",
        }, filtered)
    end)

    it("deduplicates directories while preserving first-seen order", function()
        -- given
        local dirs = { "/a", "", "/b", "/a", "/c", "/b" }

        -- when
        local deduped = util.dedup_dirs(dirs)

        -- then
        assert.are.same({ "/a", "/b", "/c" }, deduped)
    end)

    it("returns module source dirs for a test buffer including existing generated sources", function()
        -- given
        existing_dirs["/repo/module/src/main/java"] = true
        existing_dirs["/repo/module/src/test/java"] = true
        existing_dirs["/repo/module/target/generated-sources"] = true
        existing_dirs["/repo/module/target/generated-test-sources"] = true

        -- when
        local dirs = util.get_module_src_dirs(10, true)

        -- then
        assert.are.same({
            "/repo/module/src/main/java",
            "/repo/module/target/generated-sources",
            "/repo/module/target/generated-test-sources",
            "/repo/module/src/test/java",
        }, dirs)
    end)

    it("builds search dirs from module sources plus requested dependency scope", function()
        -- given
        existing_dirs["/repo/module/src/main/java"] = true
        existing_dirs["/repo/module/src/test/java"] = true
        local picker_state = {
            include_deps = true,
            include_all_deps = false,
            source_bufnr = 10,
        }

        -- when
        local dirs = util.get_search_dirs(picker_state, { preferred_deps_test = {}, preferred_deps_main = {} })

        -- then
        assert.are.same({
            "/repo/module/src/main/java",
            "/repo/module/src/test/java",
            "/deps/filtered-test",
        }, dirs)
    end)

    it("extracts importable static members and ignores non-importable lines", function()
        -- given
        local field_line = 'public static final String REQUEST_ID = "requestId";'
        local enum_line = "    NONE, DEBTOR, CREDITOR;"
        local method_line = "public static Payment paymentOf(String id) {"
        local control_flow_line = "if (ready) {"

        -- when
        local field = util.extract_static_member(field_line, "REQUEST_ID")
        local enum = util.extract_static_member(enum_line, "DEBTOR")
        local method = util.extract_static_member(method_line, "paymentOf")
        local control_flow = util.extract_static_member(control_flow_line)

        -- then
        assert.is_true(util.is_excluded_line("return REQUEST_ID;"))
        assert.is_true(util.is_excluded_line('private static final String SECRET = "x";'))
        assert.is_false(util.is_excluded_line('public static final String REQUEST_ID = "x";'))
        assert.are.equal("REQUEST_ID", field)
        assert.are.equal("DEBTOR", enum)
        assert.are.equal("paymentOf", method)
        assert.is_nil(control_flow)
    end)

    it("builds explicit and wildcard static import lines", function()
        -- given
        local fqcn = "org.assertj.core.api.Assertions"

        -- when
        local explicit = util.build_import_line(fqcn, "assertThat", "explicit")
        local wildcard = util.build_import_line(fqcn, "assertThat", "wildcard")

        -- then
        assert.are.equal("import static org.assertj.core.api.Assertions.assertThat;", explicit)
        assert.are.equal("import static org.assertj.core.api.Assertions.*;", wildcard)
    end)

    it("inserts a static import after package and existing imports", function()
        -- given
        state.buffer_lines[1] = {
            "package com.acme;",
            "",
            "import java.util.List;",
            "",
            "class Foo {}",
        }

        -- when
        util.add_import_to_buffer("import static org.assertj.core.api.Assertions.assertThat;", 1)

        -- then
        assert.are.same({
            "package com.acme;",
            "",
            "import java.util.List;",
            "import static org.assertj.core.api.Assertions.assertThat;",
            "",
            "class Foo {}",
        }, state.buffer_lines[1])
    end)

    it("does not insert duplicate static imports", function()
        -- given
        state.buffer_lines[1] = {
            "package com.acme;",
            "import static org.assertj.core.api.Assertions.assertThat;",
        }

        -- when
        util.add_import_to_buffer("import static org.assertj.core.api.Assertions.assertThat;", 1)

        -- then
        assert.are.same({
            "package com.acme;",
            "import static org.assertj.core.api.Assertions.assertThat;",
        }, state.buffer_lines[1])
        assert.are.equal("[Static Import] Already exists", state.notifications[1].message)
    end)

    it("builds rg search patterns for static fields and methods", function()
        -- given
        local field_word = "REQUEST_ID"
        local method_word = "assertThat"

        -- when
        local field_pattern = util.build_search(field_word, false)
        local method_pattern = util.build_search(method_word, true)
        local empty_pattern = util.build_search("", false)

        -- then
        assert.matches("REQUEST_ID", field_pattern)
        assert.matches("static", method_pattern)
        assert.matches("assertThat", method_pattern)
        assert.is_nil(empty_pattern)
    end)

    it("uses an invocation cache when resolving files to FQCNs", function()
        -- given
        local cache = {}
        local file = "/deps/org/assertj/core/api/Assertions.java"

        -- when
        local first = util.file_to_fqcn(file, 12, cache)
        local second = util.file_to_fqcn(file, 12, cache)

        -- then
        assert.are.equal("org.assertj.core.api.Assertions", first)
        assert.are.equal("org.assertj.core.api.Assertions", second)
        assert.are.equal(1, #fqcn_calls)
    end)

    it("derives FQCN and class name from Java source paths", function()
        -- given
        local file = "/repo/module/src/test/java/com/acme/FooTest.java"

        -- when
        local fqcn, class_name = util.fqcn_from_path(file)
        local missing_fqcn, fallback_class = util.fqcn_from_path("/tmp/Plain.java")

        -- then
        assert.are.equal("com.acme.FooTest", fqcn)
        assert.are.equal("FooTest", class_name)
        assert.is_nil(missing_fqcn)
        assert.are.equal("Plain", fallback_class)
    end)

    it("parses rg output into deduplicated static import candidates", function()
        -- given
        local stdout = table.concat({
            "/deps/org/assertj/core/api/Assertions.java:12: public static void assertThat(Object actual) {",
            "/deps/org/assertj/core/api/Assertions.java:12: public static void assertThat(Object actual) {",
            "/deps/org/assertj/core/api/Assertions.java:13: private static void hidden() {",
            "/repo/module/src/test/java/com/acme/FooTest.java:9: public static void localHelper() {",
            "/deps/Local.java:1: public static void helper() {",
        }, "\n")

        -- when
        local items = util.parse_rg_results(
            stdout,
            "explicit",
            "assertThat",
            {},
            "/repo/module/src/test/java/com/acme/FooTest.java"
        )

        -- then
        assert.are.same({
            {
                name = "import static org.assertj.core.api.Assertions.assertThat;",
                fqcn = "org.assertj.core.api.Assertions",
                member = "assertThat",
            },
        }, items)
    end)

    it("completes a camelCase prefix to the method and prefers the exact identifier", function()
        -- given
        local generic_line = "  public static <ELEMENT> AbstractIterableAssert<?, Iterable<? extends ELEMENT>, ELEMENT, "
            .. "ObjectAssert<ELEMENT>> assertThat(Iterable<? extends ELEMENT> actual) {"
        local longer_first_line = "  public static <E> ImmutableList<E> ofNullable(E e) { of("
        local caps_line = "    public static final int SIGNED_NUM = 1;"
        local enum_line = "    FOO(1), FOO_BAR(2);"

        -- when
        local prefix = util.extract_static_member(generic_line, "assertTh")
        local exact = util.extract_static_member(generic_line, "assertThat")
        local shortest_exact = util.extract_static_member(longer_first_line, "of")
        local caps_prefix = util.extract_static_member(caps_line, "SIGNE")
        local enum_exact = util.extract_static_member(enum_line, "FOO")
        local enum_prefix = util.extract_static_member(enum_line, "FOO_B")

        -- then
        assert.are.equal("assertThat", prefix)
        assert.are.equal("assertThat", exact)
        assert.are.equal("of", shortest_exact)
        assert.are.equal("SIGNED_NUM", caps_prefix)
        assert.are.equal("FOO", enum_exact)
        assert.are.equal("FOO_BAR", enum_prefix)
    end)

    it("completes the cursor word only when the member extends it", function()
        -- given
        state.buffer_lines[1] = { "assertTh(1);" }
        local range = { row = 0, col_start = 0, col_end = 8 }

        -- when
        util.complete_word_in_buffer(1, range, "assertTh", "fail")
        local unrelated = state.set_text
        util.complete_word_in_buffer(1, range, "assertTh", "assertThat")
        local completed = state.set_text

        -- then
        assert.is_nil(unrelated)
        assert.are.same({ "assertThat" }, completed.lines)
        assert.are.equal(8, completed.end_col)
    end)

    it("orders equal members by import line after ranking exact matches first", function()
        -- given
        java_common.file_to_fqcn = function(file)
            return (file:match("^/deps/(.+)%.java$"):gsub("/", "."))
        end
        local stdout = table.concat({
            "/deps/org/c/Gamma.java:1: public static void goFar() {",
            "/deps/org/b/Zeta.java:1: public static void go() {",
            "/deps/org/a/Alpha.java:1: public static void go() {",
        }, "\n")

        -- when
        local items = util.parse_rg_results(stdout, "explicit", "go", {}, "/nope")

        -- then
        assert.are.same(
            {
                "import static org.a.Alpha.go;",
                "import static org.b.Zeta.go;",
                "import static org.c.Gamma.goFar;",
            },
            vim.tbl_map(function(item)
                return item.name
            end, items)
        )
    end)

    it("separates the first import from the package line", function()
        -- given
        state.buffer_lines[1] = { "package com.acme;", "class Foo {}" }
        state.buffer_lines[2] = { "package com.acme;", "", "class Foo {}" }

        -- when
        util.add_import_to_buffer("import static org.assertj.core.api.Assertions.assertThat;", 1)
        util.add_import_to_buffer("import static org.assertj.core.api.Assertions.assertThat;", 2)

        -- then
        local expected = {
            "package com.acme;",
            "",
            "import static org.assertj.core.api.Assertions.assertThat;",
            "",
            "class Foo {}",
        }
        assert.are.same(expected, state.buffer_lines[1])
        assert.are.same(expected, state.buffer_lines[2])
    end)
end)
