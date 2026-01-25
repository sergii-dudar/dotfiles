local lsp_util = require("utils.lsp-util")

local M = {}

local function get_jdtls_client()
    return lsp_util.get_client_by_name("jdtls")
end

function M.list_projects()
    local client = get_jdtls_client()
    local response, err = client:request_sync("workspace/executeCommand", {
        command = "java.project.getAll",
        arguments = {},
    }, 5000)
    if err then
        error(vim.inspect(err))
    end
    return response.result
end

function M.get_classpath_for_project(project_uri, scope)
    local client = get_jdtls_client()
    local response, err = client:request_sync("workspace/executeCommand", {
        command = "java.project.getClasspaths",
        arguments = { project_uri, vim.json.encode({ scope = scope or "runtime" }) },
    }, 5000)
    if err then
        error(vim.inspect(err))
    end
    return response.result
end

function M.get_full_reactor_classpath(scope)
    scope = scope or "runtime"
    local projects = M.list_projects()

    local remaining = #projects
    local all_classpaths = {}

    for _, project in ipairs(projects) do
        local result = M.get_classpath_for_project(project, scope)
        for _, cp in ipairs(result.classpaths or {}) do
            all_classpaths[cp] = true -- use set to avoid duplicates
        end

        remaining = remaining - 1
        if remaining == 0 then
            local final_class_path_table = {}
            for cp, _ in pairs(all_classpaths) do
                table.insert(final_class_path_table, cp)
            end
            if #final_class_path_table > 0 then
                return table.concat(final_class_path_table, ":")
            end
        end
    end
end

return M
