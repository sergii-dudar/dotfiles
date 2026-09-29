local helper = require("tests.utils.spec_helper")

describe("utils.java.project_name_resolver.eclipse_project_parser", function()
    local eclipse_project_parser
    local files

    -- Shape of a real Buildship generated .project: nested <name> elements must not win over the top-level one.
    local buildship_project = [[
<?xml version="1.0" encoding="UTF-8"?>
<projectDescription>
	<name>test-gradle-app-test-gradle</name>
	<comment>Project test-gradle created by Buildship.</comment>
	<projects>
	</projects>
	<buildSpec>
		<buildCommand>
			<name>org.eclipse.buildship.core.gradleprojectbuilder</name>
			<arguments>
			</arguments>
		</buildCommand>
	</buildSpec>
	<natures>
		<nature>org.eclipse.buildship.core.gradleprojectnature</nature>
	</natures>
	<filteredResources>
		<filter>
			<id>1769794911701</id>
			<name></name>
			<type>30</type>
			<matcher>
				<id>org.eclipse.core.resources.regexFilterMatcher</id>
				<arguments>node_modules|\.git|__CREATED_BY_JAVA_LANGUAGE_SERVER__</arguments>
			</matcher>
		</filter>
	</filteredResources>
</projectDescription>
]]

    before_each(function()
        helper.reset_vim()
        files = {}
        helper.stub_module("lib.file", {
            read_file = function(path)
                return files[path]
            end,
        })
        eclipse_project_parser = helper.reload("utils.java.project_name_resolver.eclipse_project_parser")
    end)

    after_each(function()
        helper.clear_stub_modules({ "lib.file", "utils.java.project_name_resolver.eclipse_project_parser" })
    end)

    it("extracts the top-level project name from a real .project", function()
        -- given
        files["/repo/test-gradle/.project"] = buildship_project

        -- when
        local name = eclipse_project_parser.get_project_name("/repo/test-gradle")

        -- then
        assert.are.equal("test-gradle-app-test-gradle", name)
    end)

    it("trims whitespace around the project name", function()
        -- given
        files["/repo/module/.project"] = "<projectDescription><name>\n  payment-api\n</name></projectDescription>"

        -- when
        local name = eclipse_project_parser.get_project_name("/repo/module")

        -- then
        assert.are.equal("payment-api", name)
    end)

    it("returns nil when .project cannot be read", function()
        -- given
        files["/repo/missing/.project"] = nil

        -- when
        local name = eclipse_project_parser.get_project_name("/repo/missing")

        -- then
        assert.is_nil(name)
    end)

    it("returns nil when the project name is missing or empty", function()
        -- given
        files["/repo/no-name/.project"] = "<projectDescription><comment>x</comment></projectDescription>"
        files["/repo/empty-name/.project"] = "<projectDescription><name></name></projectDescription>"
        files["/repo/other-root/.project"] = "<project><name>payment-api</name></project>"

        -- when
        local no_name = eclipse_project_parser.get_project_name("/repo/no-name")
        local empty_name = eclipse_project_parser.get_project_name("/repo/empty-name")
        local other_root = eclipse_project_parser.get_project_name("/repo/other-root")

        -- then
        assert.is_nil(no_name)
        assert.is_nil(empty_name)
        assert.is_nil(other_root)
    end)

    it("returns nil when .project is not valid xml", function()
        -- given
        files["/repo/broken/.project"] = "<projectDescription><name>payment-api</name>"
        files["/repo/empty/.project"] = ""

        -- when
        local broken = eclipse_project_parser.get_project_name("/repo/broken")
        local empty = eclipse_project_parser.get_project_name("/repo/empty")

        -- then
        assert.is_nil(broken)
        assert.is_nil(empty)
    end)
end)
