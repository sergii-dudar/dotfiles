local M = {}

local cache_util = require("utils.cache-util")

---@return table|nil
local exec_javap_cached = function(class_name, class_path)
    local result = cache_util.java.javap_results_map[class_name]
    if result then
        return result
    end

    local cmd = string.format("javap -cp %s %s", class_path, class_name)
    local obj = vim.system({ "bash", "-c", cmd }, { text = true }):wait()
    result = obj.stdout or ""

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

    -- local pattern = "%s*([%w%.$<>_]+)%s+([%w_]+)%s*%(([^)]*)%)"
    -- local filtered_result = vim.iter(result:gmatch(pattern))
    --     :map(function(return_type, name, params)
    --         return { return_type = return_type, name = name, params = params }
    --     end)
    --     :filter(function(entry)
    --         return entry.name == method_id
    --     end)
    --     :totable()

    cache_util.java.javap_results_map[class_name] = filtered_result
    return filtered_result
end

local resolve_test_method_params = function(class_name, method_name, classpath)
    local atempts = 0
    while atempts < 2 do
        local class_details = exec_javap_cached(class_name, classpath)
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

---@return string
function M.resolve_parametrized_method_signature(method_qualified_name, class_path)
    local class_name, method_name, method_parameters = method_qualified_name:match("^([^%#]+)#([^%(]+)(%([^)]*%))$")
    if not method_parameters or method_parameters == "()" then
        return method_qualified_name
    end

    local resolved_method_parameters = resolve_test_method_params(class_name, method_name, class_path)
    if resolved_method_parameters then
        local final_qualifier = class_name .. "#" .. method_name .. resolved_method_parameters
        -- print(final_qualifier)
        return final_qualifier
    end
    vim.notify("Default qualitied name will be used " .. method_qualified_name, vim.log.levels.WARN)
    return method_qualified_name
end

return M