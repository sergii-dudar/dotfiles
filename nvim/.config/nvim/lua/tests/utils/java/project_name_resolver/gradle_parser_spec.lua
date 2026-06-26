local helper = require("tests.utils.spec_helper")

describe("utils.java.project_name_resolver.gradle_parser", function()
    local gradle_parser
    local original_popen

    before_each(function()
        helper.reset_vim()
        original_popen = io.popen
        gradle_parser = helper.reload("utils.java.project_name_resolver.gradle_parser")
    end)

    after_each(function()
        io.popen = original_popen
    end)

    it("extracts a project name from Gradle properties output", function()
        -- given
        local commands = {}
        io.popen = function(command)
            table.insert(commands, command)
            return {
                read = function()
                    return "payment-service\n"
                end,
                close = function() end,
            }
        end

        -- when
        local name = gradle_parser.get_project_name_from_gradle("/repo/payment-service")

        -- then
        assert.are.equal("payment-service", name)
        assert.matches("./gradlew properties", commands[1], nil, true)
    end)

    it("falls back to the project directory name when Gradle command has no valid result", function()
        -- given
        gradle_parser.get_project_name_from_gradle = function()
            return nil
        end

        -- when
        local name = gradle_parser.get_project_name("/repo/parent/api-module/")

        -- then
        assert.are.equal("api-module", name)
    end)

    it("returns nil when a path has no final directory segment", function()
        -- given
        local path = "/"

        -- when
        local name = gradle_parser.get_project_name_from_path(path)

        -- then
        assert.is_nil(name)
    end)
end)
