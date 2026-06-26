local helper = require("tests.utils.spec_helper")

describe("modules.go.go-test", function()
    local go_test
    local original_execute
    local original_popen
    local old_task
    local go_exit_code
    local gomod_path

    local function popen_with_gomod()
        return function()
            return {
                read = function()
                    return gomod_path .. "\n"
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
        go_exit_code = 0
        gomod_path = "/repo/go-service/go.mod"

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
            return go_exit_code
        end
        io.popen = popen_with_gomod()

        helper.stub_module("utils.nio-util", {
            multi_select = function(items)
                return items
            end,
        })

        go_test = helper.reload("modules.go.go-test")
    end)

    after_each(function()
        os.execute = original_execute
        io.popen = original_popen
        _G.task = old_task
        helper.clear_stub_modules({
            "modules.go.go-test",
            "utils.nio-util",
        })
    end)

    it("builds a go test JSON command for all packages in the module", function()
        -- given
        local context = { test_type = task.test_type.ALL_TESTS }

        -- when
        local result = go_test.build_run_test_cmd(context)

        -- then
        assert.are.equal("/tmp/nvim-test-cache/test-report/go/repo_go-service", result.report_dir)
        assert.are.equal("bash", result.cmd[1])
        assert.are.equal("-c", result.cmd[2])
        assert.matches("go' 'test' '%-json", result.cmd[3])
        assert.matches("'%.%/%.%.%.", result.cmd[3])
        assert.matches("go%-test%.json", result.cmd[3])
    end)

    it("returns an abort command when the Go binary is unavailable", function()
        -- given
        go_exit_code = 1
        go_test = helper.reload("modules.go.go-test")

        -- when
        local result = go_test.build_run_test_cmd({ test_type = task.test_type.ALL_TESTS })

        -- then
        assert.are.same({ "echo", "go binary not found; aborting." }, result.cmd)
    end)

    it("returns nil report directory outside a Go module", function()
        -- given
        gomod_path = "/dev/null"
        go_test = helper.reload("modules.go.go-test")

        -- when
        local report_dir = go_test.get_test_report_dir()

        -- then
        assert.is_nil(report_dir)
    end)
end)
