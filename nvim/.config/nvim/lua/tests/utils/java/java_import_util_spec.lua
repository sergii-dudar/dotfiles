local helper = require("tests.utils.spec_helper")

describe("utils.java.java-import-util", function()
    local import_util
    local state

    before_each(function()
        _, state = helper.reset_vim()
        import_util = helper.reload("utils.java.java-import-util")
    end)

    it("detects regular imports for a class name", function()
        -- given
        state.buffer_lines[3] = {
            "package ua.example;",
            "import java.util.List;",
            "import static java.util.Collections.emptyList;",
        }

        -- when
        local list_imported = import_util.import_exists("List", 3)
        local empty_list_imported_as_regular = import_util.import_exists("emptyList", 3)

        -- then
        assert.is_true(list_imported)
        assert.is_false(empty_list_imported_as_regular)
    end)

    it("detects explicit static imports for a member name", function()
        -- given
        state.buffer_lines[3] = {
            "import static org.assertj.core.api.Assertions.assertThat;",
            "import org.assertj.core.api.Assertions;",
        }

        -- when
        local assert_that_imported = import_util.static_import_exists("assertThat", 3)
        local assertions_imported_as_static = import_util.static_import_exists("Assertions", 3)

        -- then
        assert.is_true(assert_that_imported)
        assert.is_false(assertions_imported_as_static)
    end)
end)
