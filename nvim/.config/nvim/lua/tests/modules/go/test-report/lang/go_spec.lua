local helper = require("tests.utils.spec_helper")

describe("modules.go.test-report.lang.go", function()
    local go_lang
    local original_popen

    --- Return a no-op logger for language adapter tests.
    local function logger()
        return {
            debug = function() end,
            error = function() end,
            info = function() end,
            warn = function() end,
        }
    end

    local function popen_with_output(output)
        return function()
            return {
                read = function()
                    return output
                end,
                close = function() end,
            }
        end
    end

    before_each(function()
        helper.reset_vim()
        original_popen = io.popen

        helper.stub_module("utils.logging-util", {
            new = logger,
        })
        helper.stub_module("modules.go.test-report.json-parser", {
            parse_results = function()
                return {
                    ["github.com/acme/pkg#TestWorks"] = { status = "passed" },
                }
            end,
        })

        go_lang = helper.reload("modules.go.test-report.lang.go")
    end)

    after_each(function()
        io.popen = original_popen
        helper.clear_stub_modules({
            "modules.go.test-report.lang.go",
            "modules.go.test-report.json-parser",
            "utils.logging-util",
        })
    end)

    it("delegates result parsing to the Go JSON parser", function()
        -- given
        local dirs = { "/tmp/report" }

        -- when
        local results = go_lang.parse_results(dirs)

        -- then
        assert.are.same({
            ["github.com/acme/pkg#TestWorks"] = { status = "passed" },
        }, results)
    end)

    it("splits Go package test ids into display parts", function()
        -- given
        local id = "github.com/acme/project/internal/service#TestWorks"

        -- when
        local display = go_lang.id_to_display(id)

        -- then
        assert.are.same({
            container = "service",
            member = "TestWorks",
            group = "github.com/acme/project/internal",
        }, display)
    end)

    it("uses a package without slash as a container without a group", function()
        -- given
        local id = "main#TestWorks"

        -- when
        local display = go_lang.id_to_display(id)

        -- then
        assert.are.same({
            container = "main",
            member = "TestWorks",
            group = nil,
        }, display)
    end)

    it("extracts source lines from Go failure output", function()
        -- given
        local output = "    service_test.go:27: expected true\n"

        -- when
        local line = go_lang.extract_error_line("github.com/acme/pkg", output)

        -- then
        assert.are.equal(27, line)
    end)

    it("builds the cached Go report directory from go env GOMOD", function()
        -- given
        io.popen = popen_with_output("/repo/go-service/go.mod\n")

        -- when
        local report_dir = go_lang.get_test_report_dir()

        -- then
        assert.are.equal("/tmp/nvim-test-cache/test-report/go/repo_go-service", report_dir)
    end)
end)
