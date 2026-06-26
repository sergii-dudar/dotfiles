local helper = require("tests.utils.spec_helper")

describe("utils.java.project_name_resolver.pom_parser", function()
    local pom_parser
    local files

    before_each(function()
        helper.reset_vim()
        files = {}
        helper.stub_module("lib.file", {
            read_file = function(path)
                return files[path]
            end,
        })
        helper.stub_module("lib.xml", {
            parse = function(content)
                return { project = { artifactId = content:match("<artifactId>(.-)</artifactId>") } }
            end,
        })
        pom_parser = helper.reload("utils.java.project_name_resolver.pom_parser")
    end)

    after_each(function()
        helper.clear_stub_modules({ "lib.file", "lib.xml", "utils.java.project_name_resolver.pom_parser" })
    end)

    it("extracts the artifactId from a module pom.xml", function()
        -- given
        files["/repo/module/pom.xml"] = "<project><artifactId>payment-api</artifactId></project>"

        -- when
        local artifact_id = pom_parser.get_artifact_id("/repo/module")

        -- then
        assert.are.equal("payment-api", artifact_id)
    end)

    it("returns nil when pom.xml cannot be read", function()
        -- given
        files["/repo/missing/pom.xml"] = nil

        -- when
        local artifact_id = pom_parser.get_artifact_id("/repo/missing")

        -- then
        assert.is_nil(artifact_id)
    end)
end)
