local M = {}

local jdtls_util = require("utils.java.jdtls-util")

-- There possible special cases where more than one type have declared in project, and resolution in such case can be tricky
-- (not very elegant  ) for now, just add hardcoded dictionaty to resolve such cases (but it's rare cases)

local static_mappings = {
    ["serhii-application"] = {
        TestEnum = "ua.serhii.application.model.TestEnum",
        TestMonth = "ua.serhii.application.Something1Test$TestMonth",
    },
}

--[[ local root = require("jdtls.setup").find_root({ ".git", "mvnw", "gradlew", "pom.xml", "build.gradle" })
local current_proj_name = vim.fn.fnamemodify(root, ":t") ]]
local current_proj_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
local current_proj_mappings = static_mappings[current_proj_name]

---@param type_symbol string
---return string
M.resolve_jdt_type_symbol = function(type_symbol)
    -- if current_proj_mappings and current_proj_mappings[type_symbol] then
    --     return current_proj_mappings[type_symbol]
    -- end
    vim.notify(type_symbol .. " loading started ...")
    local result = jdtls_util.jdt_load_workspace_symbol_sync(type_symbol)
    vim.notify(type_symbol .. " loaded: " .. result.fqn)
    dd(result)
    if not result then
        vim.notify(
            string.format(
                "⚠️ type symbol [ %s ] of proj [ %s ] for neotest-java was not resolved",
                type_symbol,
                current_proj_name
            ),
            vim.log.levels.WARN
        )
        return
    end
    return result.fqn
end

--[[ nio.run(function()
		-- Open the file in write mode
		local file = assert(io.open(filepath, "w"))

		file:write(data)

		-- Close the file
		file:close()
	end) ]]

return M
