local helper = require("tests.utils.spec_helper")

describe("modules.java.dependencies-search.jarentry", function()
    local jarentry
    local state
    local jdtls_client
    local artifact_id

    before_each(function()
        _, state = helper.reset_vim()
        jdtls_client = { config = { root_dir = "/repo/payments-service" } }
        artifact_id = "payments-service"

        helper.stub_module("utils.lsp-util", {
            get_client_by_name = function(name)
                if name == "jdtls" then
                    return jdtls_client
                end
                return nil
            end,
        })
        helper.stub_module("utils.java.project_name_resolver.pom_parser", {
            get_artifact_id = function(root_dir)
                assert.are.equal("/repo/payments-service", root_dir)
                return artifact_id
            end,
        })

        jarentry = helper.reload("modules.java.dependencies-search.jarentry")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.dependencies-search.jarentry",
            "utils.java.project_name_resolver.pom_parser",
            "utils.lsp-util",
        })
    end)

    it("builds a JDT jarentry URI for a file inside an extracted Maven sources directory", function()
        -- given
        local source_dir = "/Users/me/.m2/repository/org/assertj/assertj-core/3.25.1/assertj-core-3.25.1-sources"
        local file = source_dir .. "/org/assertj/core/api/Assertions.java"

        -- when
        local uri = jarentry.build_uri(file, { source_dir })

        -- then
        assert.matches("^jdt://jarentry/org/assertj/core/api/Assertions%.java%?=payments%-service/", uri)
        assert.truthy(uri:find("maven.groupId=/org.assertj", 1, true))
        assert.truthy(uri:find("maven.artifactId=/assertj-core", 1, true))
        assert.truthy(uri:find("maven.version=/3.25.1", 1, true))
    end)

    it("returns nil when the file is not under a known source directory", function()
        -- given
        local source_dirs = { "/Users/me/.m2/repository/org/assertj/assertj-core/3.25.1/assertj-core-3.25.1-sources" }

        -- when
        local uri = jarentry.build_uri("/tmp/Assertions.java", source_dirs)

        -- then
        assert.is_nil(uri)
    end)

    it("returns nil when jdtls is unavailable", function()
        -- given
        jdtls_client = nil
        local source_dir = "/Users/me/.m2/repository/org/assertj/assertj-core/3.25.1/assertj-core-3.25.1-sources"

        -- when
        local uri = jarentry.build_uri(source_dir .. "/org/assertj/core/api/Assertions.java", { source_dir })

        -- then
        assert.is_nil(uri)
    end)

    it("opens a resource through the generated URI and jumps to a requested line", function()
        -- given
        local source_dir = "/Users/me/.m2/repository/org/assertj/assertj-core/3.25.1/assertj-core-3.25.1-sources"
        local file = source_dir .. "/org/assertj/core/api/Assertions.java"

        -- when
        local opened = jarentry.open(file, { source_dir }, 12)

        -- then
        assert.is_true(opened)
        assert.matches("^edit jdt://jarentry/org/assertj/core/api/Assertions%.java", state.commands[1])
        assert.are.same({ 12, 0 }, state.cursor)
    end)
end)
