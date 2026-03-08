local M = {}

local util = require("utils.common-util")
local cache_util = require("utils.cache-util")
local nio = require("nio")

---@return table|nil
local exec_javap_cached_nio = function(class_name, classpath)
    local result = cache_util.java.javap_results_map[class_name]
    if result then
        return result
    end

    result = nio.process
        .run({
            cmd = "bash",
            args = {
                "-c",
                string.format(
                    -- "javap -cp /home/serhii/serhii.home/git/tests/serhii-application/target/classes:/home/serhii/serhii.home/git/tests/serhii-application/target/test-classes %s",
                    -- "javap -cp target/classes:target/test-classes %s",
                    "javap -cp %s %s",
                    classpath,
                    class_name
                ),
            },
        }).stdout
        .read()

    --[[ 
    Compiled from "Something1Test.java"
    public class ua.serhii.application.Something1Test {
        public ua.serhii.application.Something1Test();
        void testSomething();
        void testSomething1();
        void someMonths_scv(int, java.lang.String, java.lang.Integer, ua.serhii.application.model.TestArgs$Role);
        void someMonths_enum(ua.serhii.application.Something1Test$TestMonth);
        void testPersonFromCsv(ua.serhii.application.model.Person);
        void testPersonAge(ua.serhii.application.model.Person);
        static java.util.stream.Stream<ua.serhii.application.model.Person> personProvider();
    }
    ]]
    if not result or result == "" then
        return nil
    end
    -- filter with no args
    local filtered_result = vim.iter(vim.split(result, "\n", { trimempty = true }))
        :filter(function(l)
            return not l:match("%(%s*%)")
        end)
        :totable()

    cache_util.java.javap_results_map[class_name] = filtered_result
    return filtered_result
end

local resolve_test_method_params_nio = function(class_name, method_name, classpath)
    local atempts = 0
    while atempts < 2 do
        local class_details = exec_javap_cached_nio(class_name, classpath)
        if not class_details then
            return nil
        end

        local method_line = vim.iter(class_details)
            :filter(function(l)
                return l:find(method_name, 1, true)
            end)
            :next()

        if method_line then
            return method_line:match("(%([^)]*%))")
        end

        -- possible only after caching and new or rename method, clean and rerun
        cache_util.java.javap_results_map[class_name] = nil
        atempts = atempts + 1
        -- vim.notify(atempts)
    end
    return nil
end

---@param context { qualified_name:string, classpath:string }
---@return string
function M.resolve_parametrized_method_signature_nio(context)
    local class_name, method_name, method_parameters = context.qualified_name:match("^([^%#]+)#([^%(]+)(%([^)]*%))$")
    if not method_parameters or method_parameters == "()" then
        return context.qualified_name
    end

    local resolved_method_parameters = resolve_test_method_params_nio(class_name, method_name, context.classpath)
    if resolved_method_parameters then
        local final_qualifier = class_name .. "#" .. method_name .. resolved_method_parameters
        -- print(final_qualifier)
        return final_qualifier
    end
    vim.notify("Default qualitied name will be used " .. context.qualified_name, vim.log.levels.WARN)
    return context.qualified_name
end

--[[ nio.run(function()

  print(require("utils.lsp-util").get_client_by_name("jdtls"):request_sync('workspace/executeCommand', {command = 'vscode.java.resolveMainClass'}))


  local jdtls_client = nio.lsp.get_clients({ name = "jdtls" })[1]
  local err, result = jdtls_client.request.workspace_executeCommand({ command = 'vscode.java.resolveMainClass' })


    print(
        M.resolve_parametrized_method_signature(
            "ua.serhii.application.Something1Test#someMonths_enum(ua.serhii.application.TestMonth)"
        )
        -- M.resolve_parametrized_method_signature("ua.serhii.application.Something1Test#someMonths_enum")
        -- M.resolve_parametrized_method_signature("ua.serhii.application.Something1Test")
    )
end) ]]

--[[ local jdtlsutil = require("jdtls.util")
local options = vim.fn.json_encode({ scope = "test" })
local cmd = {
    command = "java.project.getClasspaths",
    arguments = { vim.uri_from_bufnr(0), options },
}
jdtlsutil.execute_command(cmd, function(err1, resp)
    dd({ table.concat(resp.classpaths, ":") })
end, 0) ]]

--[[ local util = require("jdtls.util")
local options = vim.fn.json_encode({ scope = "test" })
local cmd = {
    command = "java.project.getClasspaths",
    arguments = { vim.uri_from_bufnr(0), options },
}
util.execute_command(cmd, function(err1, resp)
    if err1 then
        local msg =
            string.format("%s bufnr=%d fname=%s", err1.message, context.bufnr, api.nvim_buf_get_name(context.bufnr))
        error(msg)
    end
    dd({ table.concat(resp.classpaths, ":") })
end, 0) ]]

return M

--[[
--

lua/neotest-java/command/junit_command_builder.lua
...
if v.method_name then
	table.insert(selectors, "--select-method='" .. require("utils.java.neotest-java-util").resolve_parametrized_method_signature_nio(v.method_name, self._classpath_file_arg) .. "'")
else
...


local neotest_java_util = require("utils.java.neotest-java-util")

lua/neotest-java/core/positions_discoverer.lua
```
        ...
		elseif package_name and package_name ~= "" then
			qualified = neotest_java_util.build_method_test_param_qualified_name(t, package_name .. "." .. t)
		else
			qualified = neotest_java_util.build_method_test_param_qualified_name(t, t)
		end
        ...
```

lua/neotest-java/command/junit_command_builder.lua
```
    local jdtls_client = require("nio").lsp.get_clients({ name = "jdtls" })[1]
    dd(jdtls_client)
    local err, result = jdtls_client.request.workspace_symbol({ query = "TestMonth" })
    dd({ err = err, result = result })

    ...
	local selectors = {}
	for _, v in ipairs(self._test_references) do
		if v.type == "test" then
			v.qualified_name = neotest_java_util.parse_and_resolve_method_params_nio(v.qualified_name)
			local class_name = v.qualified_name:match("^(.-)#") or v.qualified_name
			table.insert(selectors, "--select-class='" .. class_name .. "'")
			if v.method_name then
				v.method_name = neotest_java_util.parse_and_resolve_method_params_nio(v.method_name)
				table.insert(selectors, "--select-method='" .. v.method_name .. "'")
			end
		end
	end
    ...

    ...
	local junit_command = {
		command = java(),
		args = vim.iter({
			jvm_args,
			"-jar",
			self._junit_jar.to_string(),
			"execute",
			"--classpath=" .. self._classpath_file_arg,
			"--reports-dir=" .. self._reports_dir.to_string(),
			"--fail-if-no-tests",
			"--disable-banner",
			"--details=testfeed",
			-- "--include-classname="^(Test.*|.+[.$]Test.*|.*Tests?|I[Tt].*|.+[.$]I[Tt].*|.*I[Tt]?)$",
			-- '--include-classname="^(Test.*|.+[.$]Test.*|.*Tests?|I[Tt].*|.+[.$]I[Tt].*|.*I[Tt]?)$"',
			"--config=junit.platform.output.capture.stdout=true",
		})
			:flatten()
			:totable(),
	}
    ...
```
]]