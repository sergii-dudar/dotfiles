local helper = require("tests.utils.spec_helper")

describe("modules.rust.test-report.lang.rust", function()
    local rust_lang
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
        helper.stub_module("modules.rust.test-report.nextest-xml", {
            parse_results = function()
                return {
                    ["crate_a#works"] = { status = "passed" },
                }
            end,
        })

        rust_lang = helper.reload("modules.rust.test-report.lang.rust")
    end)

    after_each(function()
        io.popen = original_popen
        helper.clear_stub_modules({
            "modules.rust.test-report.lang.rust",
            "modules.rust.test-report.nextest-xml",
            "utils.logging-util",
        })
    end)

    it("returns raw parsed results when no workspace root can be resolved", function()
        -- given
        io.popen = popen_with_output("not json")
        vim.fn.json_decode = function()
            error("invalid json")
        end

        -- when
        local results = rust_lang.parse_results({ "/tmp/reports" })

        -- then
        assert.are.same({
            ["crate_a#works"] = { status = "passed" },
        }, results)
    end)

    it("splits nested Rust test ids into display parts", function()
        -- given
        local id = "payments_core::parser::inner#works"

        -- when
        local display = rust_lang.id_to_display(id)

        -- then
        assert.are.same({
            container = "inner",
            member = "works",
            group = "payments_core::parser",
        }, display)
    end)

    it("uses a binary root test id as a container without a group", function()
        -- given
        local id = "payments_core#root_test"

        -- when
        local display = rust_lang.id_to_display(id)

        -- then
        assert.are.same({
            container = "payments_core",
            member = "root_test",
            group = nil,
        }, display)
    end)

    it("extracts source lines from Rust panic output", function()
        -- given
        local stacktrace = "thread 'x' panicked at src/parser.rs:88:13\nstack backtrace:\n"

        -- when
        local line = rust_lang.extract_error_line("payments_core::parser", stacktrace)

        -- then
        assert.are.equal(88, line)
    end)

    it("builds the default nextest report directory from cargo metadata", function()
        -- given
        io.popen = popen_with_output('{"workspace_root":"/repo/rust"}')
        vim.fn.json_decode = function()
            return { workspace_root = "/repo/rust" }
        end

        -- when
        local report_dir = rust_lang.get_test_report_dir()

        -- then
        assert.are.equal("/repo/rust/target/nextest/default", report_dir)
    end)
end)
