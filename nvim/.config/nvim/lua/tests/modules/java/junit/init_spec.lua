local helper = require("tests.utils.spec_helper")

describe("modules.java.junit", function()
    local junit
    local current_class
    local current_method
    local old_task

    before_each(function()
        helper.reset_vim()
        old_task = _G.task
        _G.task = {
            test_type = {
                ALL_TESTS = "ALL_TESTS",
                ALL_DIR_TESTS = "ALL_DIR_TESTS",
                FILE_TESTS = "FILE_TESTS",
                CURRENT_TEST = "CURRENT_TEST",
                CURRENT_PARAMETRIZED_NUM_TEST = "CURRENT_PARAMETRIZED_NUM_TEST",
                ALL_MODULES_TESTS = "ALL_MODULES_TESTS",
                SELECTED_MODULES_TESTS = "SELECTED_MODULES_TESTS",
                TOGGLE_LAST_DEBUG = "TOGGLE_LAST_DEBUG",
            },
            last_test = {},
        }
        current_class = nil
        current_method = nil

        vim.fn.glob = function(pattern, _, list)
            if list then
                return {}
            end
            if pattern:match("junit%-platform%-console%-standalone%.jar") then
                return "/tools/junit-platform-console-standalone.jar"
            end
            return "/sdk/java/current"
        end

        helper.stub_module("utils.java.java-common", {
            java_bin = "/sdk/java/current/bin/java",
            get_buffer_project_path = function()
                return "/repo/module"
            end,
            get_build_layout = function(module_path)
                return {
                    test_classes_dir = module_path .. "/target/test-classes",
                    report_dir = module_path .. "/target/junit-report",
                }
            end,
        })
        helper.stub_module("utils.java.java-ts-util", {
            get_class_package = function()
                return "com.acme"
            end,
            get_class_name_with_abstract = function()
                return current_class
            end,
            get_root_class_with_abstract = function()
                return nil
            end,
            get_full_method_with_params_and_abstract = function()
                return current_method
            end,
            get_full_method_with_params = function()
                return current_method and current_method.fsignature or nil
            end,
        })
        helper.stub_module("utils.java.javap-util", {
            resolve_parametrized_method_signature = function(signature)
                return signature
            end,
        })
        helper.stub_module("utils.java.jdtls-util", {
            jdt_find_implementations_nio = function()
                return {}
            end,
        })
        helper.stub_module("utils.nio-util", {
            input = function()
                return "1"
            end,
            multi_select = function(items)
                return items
            end,
            select = function(items)
                return items[1]
            end,
        })
        helper.stub_module("utils.java.jdtls-classpath-util", {
            get_classpath_for_main_method = function(opts)
                assert.are.equal("test", opts.scope)
                return "/repo/module/target/test-classes:/repo/module/target/classes"
            end,
            get_all_project_modules = function()
                return nil
            end,
        })

        junit = helper.reload("modules.java.junit")
    end)

    after_each(function()
        _G.task = old_task
        helper.clear_stub_modules({
            "modules.java.junit",
            "utils.java.java-common",
            "utils.java.java-ts-util",
            "utils.java.javap-util",
            "utils.java.jdtls-util",
            "utils.nio-util",
            "utils.java.jdtls-classpath-util",
        })
    end)

    it("builds a command that scans the current module test classes", function()
        -- given
        local context = { test_type = task.test_type.ALL_TESTS }

        -- when
        local result = junit.build_run_test_cmd(context)

        -- then
        assert.are.equal("/repo/module/target/junit-report", result.report_dir)
        assert.is_true(vim.tbl_contains(result.cmd, "--scan-class-path=/repo/module/target/test-classes"))
        assert.is_true(
            vim.tbl_contains(result.cmd, "--classpath=/repo/module/target/test-classes:/repo/module/target/classes")
        )
        assert.are.equal(task.test_type.ALL_TESTS, task.last_test.type)
    end)

    it("builds a command for the current concrete test class", function()
        -- given
        current_class = { fqn = "com.acme.FooTest", is_abstract = false }
        local context = { test_type = task.test_type.FILE_TESTS }

        -- when
        local result = junit.build_run_test_cmd(context)

        -- then
        assert.is_true(vim.tbl_contains(result.cmd, "--select-class=com.acme.FooTest"))
        assert.are.equal("--select-class=com.acme.FooTest", task.last_test.test_selector)
    end)

    it("builds a command for the current concrete test method", function()
        -- given
        current_method = {
            fsignature = "com.acme.FooTest#works(java.lang.String)",
            is_abstract = false,
        }
        local context = { test_type = task.test_type.CURRENT_TEST }

        -- when
        local result = junit.build_run_test_cmd(context)

        -- then
        assert.is_true(vim.tbl_contains(result.cmd, "--select-method=com.acme.FooTest#works(java.lang.String)"))
        assert.are.equal("--select-method=com.acme.FooTest#works(java.lang.String)", task.last_test.test_selector)
    end)

    it("returns an echo command when the current test selector cannot be resolved", function()
        -- given
        current_method = nil
        local context = { test_type = task.test_type.CURRENT_TEST }

        -- when
        local result = junit.build_run_test_cmd(context)

        -- then
        assert.are.same({ "echo", "Wrong test selector context!" }, result.cmd)
    end)
end)
