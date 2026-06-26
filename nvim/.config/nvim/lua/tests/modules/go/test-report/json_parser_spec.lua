local helper = require("tests.utils.spec_helper")

describe("modules.go.test-report.json-parser", function()
    local json_parser
    local state
    local file_content
    local events_by_line

    --- Return a no-op logger for parser tests.
    local function logger()
        return {
            debug = function() end,
            error = function() end,
            info = function() end,
            warn = function() end,
        }
    end

    local function event_line(name, event)
        events_by_line[name] = event
        return name
    end

    before_each(function()
        _, state = helper.reset_vim()
        file_content = ""
        events_by_line = {}

        vim.fn.json_decode = function(line)
            local event = events_by_line[line]
            if not event then
                error("unexpected json line: " .. tostring(line))
            end
            return event
        end

        helper.stub_module("utils.logging-util", {
            new = logger,
        })
        helper.stub_module("lib.file", {
            read_file = function()
                return file_content
            end,
        })

        json_parser = helper.reload("modules.go.test-report.json-parser")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.go.test-report.json-parser",
            "utils.logging-util",
            "lib.file",
        })
    end)

    it("lists JSON reports from a Go report directory", function()
        -- given
        vim.fn.glob = function(pattern, _, list)
            assert.are.equal("/tmp/report/*.json", pattern)
            assert.is_true(list)
            return { "/tmp/report/go-test.json" }
        end

        -- when
        local files = json_parser.list_report_files("/tmp/report")

        -- then
        assert.are.same({ "/tmp/report/go-test.json" }, files)
    end)

    it("parses passed, failed, and skipped tests from go test JSON events", function()
        -- given
        file_content = table.concat({
            event_line("run-pass", { Action = "run", Package = "github.com/acme/pkg", Test = "TestPass" }),
            event_line("pass", { Action = "pass", Package = "github.com/acme/pkg", Test = "TestPass", Elapsed = 0.1 }),
            event_line("run-fail", { Action = "run", Package = "github.com/acme/pkg", Test = "TestFail" }),
            event_line("output-fail", {
                Action = "output",
                Package = "github.com/acme/pkg",
                Test = "TestFail",
                Output = "    service_test.go:42: expected true\n",
            }),
            event_line("fail", { Action = "fail", Package = "github.com/acme/pkg", Test = "TestFail", Elapsed = 0.2 }),
            event_line("skip", { Action = "skip", Package = "github.com/acme/pkg", Test = "TestSkip", Elapsed = 0 }),
        }, "\n")

        -- when
        local results = json_parser.parse_file("/tmp/report/go-test.json")

        -- then
        assert.are.equal("passed", results["github.com/acme/pkg#TestPass"].status)
        assert.are.equal("failed", results["github.com/acme/pkg#TestFail"].status)
        assert.are.same({
            { message = "service_test.go:42: expected true", line = 42 },
        }, results["github.com/acme/pkg#TestFail"].errors)
        assert.matches("service_test.go:42", results["github.com/acme/pkg#TestFail"].invocations[1].stdout)
        assert.are.equal("skipped", results["github.com/acme/pkg#TestSkip"].status)
    end)

    it("aggregates subtests under the parent test result", function()
        -- given
        file_content = table.concat({
            event_line("run-a", { Action = "run", Package = "github.com/acme/pkg", Test = "TestParent/A" }),
            event_line(
                "pass-a",
                { Action = "pass", Package = "github.com/acme/pkg", Test = "TestParent/A", Elapsed = 0.1 }
            ),
            event_line("run-b", { Action = "run", Package = "github.com/acme/pkg", Test = "TestParent/B" }),
            event_line("output-b", {
                Action = "output",
                Package = "github.com/acme/pkg",
                Test = "TestParent/B",
                Output = "    parent_test.go:9: boom\n",
            }),
            event_line(
                "fail-b",
                { Action = "fail", Package = "github.com/acme/pkg", Test = "TestParent/B", Elapsed = 0.2 }
            ),
        }, "\n")

        -- when
        local results = json_parser.parse_file("/tmp/report/go-test.json")

        -- then
        local result = results["github.com/acme/pkg#TestParent"]
        assert.are.equal("failed", result.status)
        assert.are.equal(0.3, tonumber(string.format("%.1f", result.time)))
        local invocation_names = { result.invocations[1].name, result.invocations[2].name }
        table.sort(invocation_names)
        assert.are.same({ "A", "B" }, invocation_names)
        assert.are.same({ { message = "parent_test.go:9: boom", line = 9 } }, result.errors)
    end)

    it("infers a passed status for benchmark-like entries from package-level pass", function()
        -- given
        file_content = table.concat({
            event_line("run-bench", { Action = "run", Package = "github.com/acme/pkg", Test = "BenchmarkFast" }),
            event_line("output-bench", {
                Action = "output",
                Package = "github.com/acme/pkg",
                Test = "BenchmarkFast",
                Output = "BenchmarkFast-10 100 ns/op\n",
            }),
            event_line("package-pass", { Action = "pass", Package = "github.com/acme/pkg" }),
        }, "\n")

        -- when
        local results = json_parser.parse_file("/tmp/report/go-test.json")

        -- then
        assert.are.equal("passed", results["github.com/acme/pkg#BenchmarkFast"].status)
        assert.are.equal(
            "BenchmarkFast-10 100 ns/op\n",
            results["github.com/acme/pkg#BenchmarkFast"].invocations[1].stdout
        )
    end)

    it("warns when go reports that the run regex matched no tests", function()
        -- given
        file_content = event_line("no-tests", {
            Action = "output",
            Package = "github.com/acme/pkg",
            Output = "testing: warning: no tests to run\n",
        })

        -- when
        local results = json_parser.parse_file("/tmp/report/go-test.json")

        -- then
        assert.are.same({}, results)
        assert.matches("go clean %-testcache", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)
end)
