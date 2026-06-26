local helper = require("tests.utils.spec_helper")

describe("modules.rust.cargo-test", function()
    local cargo_test
    local original_execute
    local original_popen
    local old_task
    local nextest_exit_code
    local metadata

    local function popen_with_metadata()
        return function()
            return {
                read = function()
                    return metadata
                end,
                close = function() end,
            }
        end
    end

    before_each(function()
        helper.reset_vim()
        original_execute = os.execute
        original_popen = io.popen
        old_task = _G.task
        nextest_exit_code = 0
        metadata = '{"workspace_root":"/repo/rust","packages":[],"workspace_members":[]}'

        _G.task = {
            test_type = {
                ALL_TESTS = "ALL_TESTS",
                ALL_MODULES_TESTS = "ALL_MODULES_TESTS",
                SELECTED_MODULES_TESTS = "SELECTED_MODULES_TESTS",
                ALL_DIR_TESTS = "ALL_DIR_TESTS",
                FILE_TESTS = "FILE_TESTS",
                CURRENT_TEST = "CURRENT_TEST",
                CURRENT_PARAMETRIZED_NUM_TEST = "CURRENT_PARAMETRIZED_NUM_TEST",
                TOGGLE_LAST_DEBUG = "TOGGLE_LAST_DEBUG",
            },
        }

        os.execute = function()
            return nextest_exit_code
        end
        io.popen = popen_with_metadata()
        vim.fn.json_decode = function()
            return {
                workspace_root = "/repo/rust",
                packages = {},
                workspace_members = {},
            }
        end
        vim.fn.filereadable = function()
            return 1
        end

        helper.stub_module("utils.nio-util", {
            multi_select = function(items)
                return items
            end,
            run = function(callback)
                callback()
            end,
        })
        helper.stub_module("utils.lsp-util", {
            get_client_by_name = function()
                return nil
            end,
        })

        cargo_test = helper.reload("modules.rust.cargo-test")
    end)

    after_each(function()
        os.execute = original_execute
        io.popen = original_popen
        _G.task = old_task
        helper.clear_stub_modules({
            "modules.rust.cargo-test",
            "utils.lsp-util",
            "utils.nio-util",
        })
    end)

    it("builds a workspace nextest command for all Rust tests", function()
        -- given
        local context = { test_type = task.test_type.ALL_TESTS }

        -- when
        local result = cargo_test.build_run_test_cmd(context)

        -- then
        assert.are.same({
            "cargo",
            "nextest",
            "run",
            "--tool-config-file",
            "copilot-cli:/tmp/nvim-test-cache/test-report/nextest.toml",
            "--no-fail-fast",
            "--workspace",
        }, result.cmd)
        assert.are.equal("/repo/rust/target/nextest/default", result.report_dir)
    end)

    it("returns an abort command when cargo-nextest is unavailable", function()
        -- given
        nextest_exit_code = 1
        cargo_test = helper.reload("modules.rust.cargo-test")

        -- when
        local result = cargo_test.build_run_test_cmd({ test_type = task.test_type.ALL_TESTS })

        -- then
        assert.are.same({ "echo", "cargo-nextest not installed; aborting." }, result.cmd)
    end)

    it("passes preselected packages into a multi-package command", function()
        -- given
        local context = {
            test_type = task.test_type.SELECTED_MODULES_TESTS,
            selected_packages = {
                { name = "crate-a", path = "/repo/rust/crate-a" },
                { name = "crate-b", path = "/repo/rust/crate-b" },
            },
        }

        -- when
        local result = cargo_test.build_run_test_cmd(context)

        -- then
        assert.are.same(
            { "/repo/rust/crate-a/target/nextest/default", "/repo/rust/crate-b/target/nextest/default" },
            result.report_dir
        )
        assert.are.equal("sh", result.cmd[1])
        assert.are.equal("-c", result.cmd[2])
        assert.matches("crate%-a", result.cmd[3])
        assert.matches("crate%-b", result.cmd[3])
    end)
end)
