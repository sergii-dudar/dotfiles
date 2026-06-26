local helper = require("tests.utils.spec_helper")

describe("utils.java.maven-util", function()
    local maven_util

    before_each(function()
        helper.reset_vim()
        maven_util = helper.reload("utils.java.maven-util")
    end)

    it("prefers MAVEN_SETTINGS_XML over discovered settings files", function()
        -- given
        vim.env.MAVEN_SETTINGS_XML = "/custom/settings.xml"
        vim.env.MAVEN_HOME = "/maven-home"
        vim.fn.expand = function(path)
            return path == "~/.m2/settings.xml" and "/home/me/.m2/settings.xml" or path
        end
        vim.fn.filereadable = function()
            return 1
        end

        -- when
        local settings = maven_util.get_maven_settings()

        -- then
        assert.are.equal("/custom/settings.xml", settings)
    end)

    it("falls back to Maven home settings when user settings are missing", function()
        -- given
        vim.env.MAVEN_HOME = "/maven-home"
        vim.fn.expand = function(path)
            return path == "~/.m2/settings.xml" and "/home/me/.m2/settings.xml" or path
        end
        vim.fn.filereadable = function(path)
            return path == "/maven-home/conf/settings.xml" and 1 or 0
        end

        -- when
        local settings = maven_util.get_maven_settings()

        -- then
        assert.are.equal("/maven-home/conf/settings.xml", settings)
    end)

    it("detects pom.xml file paths", function()
        -- given
        local pom = "/repo/module/pom.xml"
        local other = "/repo/module/build.gradle"

        -- when
        local is_pom = maven_util.is_pom_file(pom)
        local is_other_pom = maven_util.is_pom_file(other)

        -- then
        assert.is_true(is_pom)
        assert.is_false(is_other_pom)
    end)

    it("maps Maven log levels to diagnostic severities", function()
        -- given
        local warning = "WARNING"
        local unknown = "strange"

        -- when
        local warning_severity = maven_util.to_severity(warning)
        local unknown_severity = maven_util.to_severity(unknown)

        -- then
        assert.are.equal(vim.diagnostic.severity.WARN, warning_severity)
        assert.are.equal(vim.diagnostic.severity.ERROR, unknown_severity)
    end)

    it("deduplicates diagnostics by line and column while preserving order", function()
        -- given
        local first = { lnum = 1, col = 2, message = "first" }
        local duplicate = { lnum = 1, col = 2, message = "duplicate" }
        local second = { lnum = 2, col = 1, message = "second" }

        -- when
        local result = maven_util.dedupe_file_diagnstics({ first, duplicate, second })

        -- then
        assert.are.same({ first, second }, result)
    end)
end)
