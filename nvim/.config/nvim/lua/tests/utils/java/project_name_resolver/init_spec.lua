local helper = require("tests.utils.spec_helper")

describe("utils.java.project_name_resolver", function()
    local resolver
    local pom_parser
    local gradle_parser

    local function stub_logger()
        helper.stub_module("utils.logging-util", {
            new = function()
                return {
                    set_level = function() end,
                    debug = function() end,
                    info = function() end,
                    warn = function() end,
                }
            end,
        })
    end

    before_each(function()
        helper.reset_vim()
        stub_logger()
        pom_parser = {
            get_artifact_id = function()
                return nil
            end,
        }
        gradle_parser = {
            get_project_name = function()
                return nil
            end,
        }
        helper.stub_module("utils.java.project_name_resolver.pom_parser", pom_parser)
        helper.stub_module("utils.java.project_name_resolver.gradle_parser", gradle_parser)
        helper.stub_module("utils.java.java-common", {
            detect_project_type = function()
                return "unknown"
            end,
            get_buffer_project_path = function()
                return "/repo/current"
            end,
        })
        resolver = helper.reload("utils.java.project_name_resolver")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "utils.logging-util",
            "utils.java.project_name_resolver",
            "utils.java.project_name_resolver.pom_parser",
            "utils.java.project_name_resolver.gradle_parser",
            "utils.java.java-common",
        })
    end)

    it("uses an explicit Maven project type to read artifactId from pom.xml", function()
        -- given
        vim.fn.filereadable = function(path)
            return path == "/repo/maven/pom.xml" and 1 or 0
        end
        pom_parser.get_artifact_id = function(module_dir)
            return module_dir == "/repo/maven" and "payment-api" or nil
        end

        -- when
        local name = resolver.resolve_project_name("/repo/maven", "maven")

        -- then
        assert.are.equal("payment-api", name)
    end)

    it("uses an explicit Gradle project type to read the Gradle project name", function()
        -- given
        gradle_parser.get_project_name = function(module_dir)
            return module_dir == "/repo/gradle" and "payment-service" or nil
        end

        -- when
        local name = resolver.resolve_project_name("/repo/gradle", "gradle")

        -- then
        assert.are.equal("payment-service", name)
    end)

    it("falls back to the module directory name when build metadata is unavailable", function()
        -- given
        local module_dir = "/repo/unknown-module"

        -- when
        local name = resolver.resolve_project_name(module_dir, "unknown")

        -- then
        assert.are.equal("unknown-module", name)
    end)
end)
