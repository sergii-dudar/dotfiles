---@param symbol string
local M = {}

local list_util = require("utils.list-util")
local util = require("utils.common-util")
local lsp_util = require("utils.lsp-util")
local string_util = require("utils.string-util")
local jdtls_util = require("utils.java.jdtls-util")

M.jdt_load_workspace_symbol_sync_inner = function(symbol)
    -- Request LSP to find the symbol
    local jdtls_client = lsp_util.get_client_by_name("jdtls")

    if not jdtls_client then
        vim.notify("⚠️ JDTLS is not connected to current buffer to resolve symbol request")
        return
    end
    local response = jdtls_client:request_sync("workspace/symbol", { query = symbol }, 3000)
    if not response then
        vim.notify("⚠️ Error: no response from workspace/symbol by query = " .. symbol, vim.log.levels.WARN)
        return
    elseif response.err then
        vim.notify("⚠️ Error: " .. tostring(response.err), vim.log.levels.WARN)
        return
    end
    local result = response.result
    if not result or vim.tbl_isempty(result) then
        vim.notify("⚠️ Class not found: " .. symbol, vim.log.levels.WARN)
        return
    end

    --[[ { {
    containerName = "ua.raiffeisen.payments.cardtransferinitiation.core.model.enumeration",
    kind = 10,
    location = {
      range = {
        ["end"] = {
          character = 29,
          line = 15
        },
        start = {
          character = 12,
          line = 15
        }
      },
      uri = "file:///Users/iuada144/serhii.home/work/git.work/ua-payments-payment-card-transfer-initiation/src/main/java/ua/raiffeisen/payments/cardtransferinitiation/core/model/enumeration/OperationCodeType.java"
    },
    name = "OperationCodeType"
  } } ]]

    -- If multiple results, try to find the one that is a Class (Kind 5) or Interface (Kind 11)
    local target = nil
    --[[ if #result > 1 then
		local single_result = list_util.findFirst(result, function(item)
			return string_util.starts_with(item.containerName, class_package) and item.name == simple_class_name
		end)

		if single_result then
			target = single_result
		else
			vim.notify(
				"⚠️ Found more than one lsp symbols, and not found unique by qualifier, first will be picked"
			)
			target = result[1]
		end
	else
		target = result[1]
	end ]]
    -- TODO: need filtering more smart project specific
    if #result > 1 then
        vim.notify("⚠️ Found more than one lsp symbols, and not found unique by qualifier, first will be picked")
    end
    target = result[1]
    return {
        fqn = jdtls_util.build_fqn(target),
    }
end

local workspace_symbol_sync_cache = {}
M.jdt_load_workspace_symbol_sync = function(symbol)
    local result = workspace_symbol_sync_cache[symbol]
    if result then
        print("cached")
        return result
    end
    print("load")
    result = M.jdt_load_workspace_symbol_sync_inner(symbol)
    workspace_symbol_sync_cache[symbol] = result
    return result
end

print(M.jdt_load_workspace_symbol_sync("OperationCodeType"))
print(M.jdt_load_workspace_symbol_sync("OperationCodeType"))
print(M.jdt_load_workspace_symbol_sync("OperationCodeType"))
print(M.jdt_load_workspace_symbol_sync("OperationCodeType"))

--[[ require("utils.java.jdtls-util").jdt_open_class("OperationCodeType")
 ua.raiffeisen.payments.cardtransferinitiation.core.model.enumeration

 require("utils.java.jdtls-util").jdt_open_class("TestInitiationParams")
 ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT

 require("utils.java.jdtls-util").jdt_open_class("TestInner2")
 ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT.TestInner1 ]]

--[[ print(build_fqn({ containerName = "a.b", name = "User" }))
print(build_fqn({ containerName = "a.b.Outer", name = "Inner" }))
print(build_fqn({ containerName = "a.b.Outer.Inner", name = "Deep" }))
print(build_fqn({ containerName = "x.y.z", name = "MyClass" }))
print(build_fqn({ containerName = "", name = "Unknown" })) ]]