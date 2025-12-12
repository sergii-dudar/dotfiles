local M = {}

local jdtls_util = require("utils.java.jdtls-util")
local util = require("utils.common-util")

-- There possible special cases where more than one type have declared in project, and resolution in such case can be tricky
-- (not very elegant  ) for now, just add hardcoded dictionaty to resolve such cases (but it's rare cases)

local static_mappings = {
    ["serhii-application"] = {
        TestEnum = "ua.serhii.application.model.TestEnum",
        TestMonth = "ua.serhii.application.Something1Test$TestMonth",
    },
}

local private_mappings =
    util.load_optional_module(os.getenv("HOME") .. "/dotfiles/other/private/jdtls_static_symbol_mappings.lua")
static_mappings = vim.tbl_extend("force", static_mappings, private_mappings.static_mappings or {})

--[[ local root = require("jdtls.setup").find_root({ ".git", "mvnw", "gradlew", "pom.xml", "build.gradle" })
local current_proj_name = vim.fn.fnamemodify(root, ":t") ]]
local current_proj_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
local current_proj_mappings = static_mappings[current_proj_name]

---@param type_symbol string
---@return string|nil
local resolve_jdt_type_symbol_nio = function(type_symbol)
    if current_proj_mappings and current_proj_mappings[type_symbol] then
        return current_proj_mappings[type_symbol]
    end
    local result = jdtls_util.jdt_load_workspace_symbol_sync_nio(type_symbol)
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

--[[ {
  method_name = "ua.serhii.application.Something1Test#someMonths_scv(jdtls:{{TestEnum}}||default:{{ua.serhii.application.TestEnum}})",
  qualified_name = "ua.serhii.application.Something1Test#someMonths_scv(jdtls:{{TestEnum}}||default:{{ua.serhii.application.TestEnum}})",
  type = "test"
} ]]
--[[ local sig =
    "ua.serhii.application.Something1Test#someMonths_scv(jdtls:{{TestEnum}}||default:{{ua.serhii.application.TestEnum}}, String, Object, jdtls:{{TestEnum}}||default:{{ua.serhii.application.TestEnum}})" ]]

local function get_params(sig)
    -- get everything inside the parentheses
    local inside = sig:match("%((.*)%)")
    if not inside or inside == "" then
        return {}
    end

    local params = {}

    for p in inside:gmatch("[^,]+") do
        -- trim spaces
        p = p:match("^%s*(.-)%s*$")
        table.insert(params, p)
    end

    return params
end

---@param qualified_name string
---return string
M.parse_and_resolve_method_params_nio = function(qualified_name)
    local params = get_params(qualified_name)
    -- print(params)
    for _, value in ipairs(params) do
        if value:find("^jdtls:") then
            local jdtls_val = value:match("jdtls:%s*{{(.-)}}")
            local default_val = value:match("default:%s*{{(.-)}}")
            local resolved_symbol = resolve_jdt_type_symbol_nio(jdtls_val)
            resolved_symbol = resolved_symbol or default_val
            qualified_name = qualified_name:gsub(value, resolved_symbol)
            -- print({
            -- 	jdtls = jdtls_val,
            -- 	default = default_val,
            -- })
        end
    end
    return qualified_name
end

return M
