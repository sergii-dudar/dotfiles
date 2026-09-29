local helper = require("tests.utils.spec_helper")

describe("utils.java.project_name_resolver", function()
    local resolver
    local eclipse_project_parser
    local pom_parser
    local gradle_parser
    local java_common

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
        eclipse_project_parser = {
            get_project_name = function()
                return nil
            end,
        }
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
        helper.stub_module("utils.java.project_name_resolver.eclipse_project_parser", eclipse_project_parser)
        helper.stub_module("utils.java.project_name_resolver.pom_parser", pom_parser)
        helper.stub_module("utils.java.project_name_resolver.gradle_parser", gradle_parser)
        java_common = {
            detect_project_type = function()
                return "unknown"
            end,
            detect_project_type_at = function()
                return "unknown"
            end,
            get_buffer_project_path = function()
                return "/repo/current"
            end,
        }
        helper.stub_module("utils.java.java-common", java_common)
        resolver = helper.reload("utils.java.project_name_resolver")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "utils.logging-util",
            "utils.java.project_name_resolver",
            "utils.java.project_name_resolver.eclipse_project_parser",
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

    it("prefers the Eclipse project name over the build file", function()
        -- given
        local gradle_called = false
        eclipse_project_parser.get_project_name = function(module_dir)
            return module_dir == "/repo/test-gradle" and "test-gradle-app-test-gradle" or nil
        end
        gradle_parser.get_project_name = function()
            gradle_called = true
            return "test-gradle"
        end

        -- when
        local name = resolver.resolve_project_name("/repo/test-gradle", "gradle")

        -- then
        assert.are.equal("test-gradle-app-test-gradle", name)
        assert.is_false(gradle_called)
    end)

    it("keeps a dotted module directory name whole in the fallback", function()
        -- given
        local module_dir = "/repo/com.example.app"

        -- when
        local name = resolver.resolve_project_name(module_dir, "unknown")

        -- then
        assert.are.equal("com.example.app", name)
    end)

    it("ignores trailing slashes of the module directory", function()
        -- given
        local parsed_dirs = {}
        vim.fn.filereadable = function(path)
            return path == "/repo/maven/pom.xml" and 1 or 0
        end
        vim.fn.fnamemodify = function(path, modifier)
            -- real Neovim: the tail of a path with a trailing slash is empty
            return modifier == ":t" and (path:match("([^/]*)$") or "") or path
        end
        pom_parser.get_artifact_id = function(module_dir)
            table.insert(parsed_dirs, module_dir)
            return nil
        end

        -- when
        local name = resolver.resolve_project_name("/repo/maven//", "maven")

        -- then
        assert.are.same({ "/repo/maven" }, parsed_dirs)
        assert.are.equal("maven", name)
    end)

    it("uses cwd when the current buffer is outside of any project", function()
        -- given
        local detected_at
        java_common.get_buffer_project_path = function()
            return nil
        end
        java_common.detect_project_type_at = function(path)
            detected_at = path
            return "maven"
        end
        vim.fn.filereadable = function(path)
            return path == "/workspace/pom.xml" and 1 or 0
        end
        pom_parser.get_artifact_id = function(module_dir)
            return module_dir == "/workspace" and "payment-api" or nil
        end

        -- when
        local name = resolver.resolve_project_name()

        -- then
        assert.are.equal("/workspace", detected_at)
        assert.are.equal("payment-api", name)
    end)
end)
