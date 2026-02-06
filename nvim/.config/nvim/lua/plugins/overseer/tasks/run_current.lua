local build_resolvers = function()
    local type_to_resolver = {}
    type_to_resolver["go"] = function()
        local file = vim.fn.expand("%:p")
        return { "go", "run", file }
    end
    type_to_resolver["python"] = function()
        local file = vim.fn.expand("%:p")
        return { "python", file }
    end
    type_to_resolver["cpp"] = function()
        local fileNameWithoutExt = vim.fn.expand("%:t:r")
        local fileDir = vim.fn.expand("%:p:h")
        return {
            "sh",
            "-c",
            "g++ -std=c++23 "
                .. fileDir
                .. "/"
                .. fileNameWithoutExt
                .. "*.cpp -o /tmp/"
                .. fileNameWithoutExt
                .. " && /tmp/"
                .. fileNameWithoutExt
                .. " && rm /tmp/"
                .. fileNameWithoutExt,
        }
    end
    type_to_resolver["java"] = function()
        local java_runner = require("plugins.overseer.tasks.runner.java-runner")
        return java_runner.build_cmd()
    end

    return type_to_resolver
end

return {
    name = "RUN_CURRENT",
    builder = function()
        local resolvers = build_resolvers()
        local type_resolver = resolvers[vim.bo.filetype]
        local result_cmd
        if type_resolver then
            result_cmd = type_resolver()
        else
            local file = vim.fn.expand("%:p")
            result_cmd = { file }
        end
        return {
            cmd = result_cmd,
            -- add some components that will pipe the output to quickfix,
            -- parse it using errorformat, and display any matching lines as diagnostics.
            components = {
                { "on_output_quickfix", set_diagnostics = true },
                "on_result_diagnostics",
                "on_exit_set_status",
                "on_complete_dispose",
                -- "on_complete_notify",
                -- { "on_complete_dispose", require_view = { "SUCCESS", "FAILURE" } },
                -- "on_result_diagnostics_trouble",
                -- "open_output",
                -- "on_output_notify",
                --"default",
            },
        }
    end,
    condition = {
        filetype = { "sh", "python", "go", "cpp", "java" },
    },
}