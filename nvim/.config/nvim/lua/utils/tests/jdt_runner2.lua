---@param symbol string
local M = {}

local list_util = require("utils.list-util")
local util = require("utils.common-util")
local lsp_util = require("utils.lsp-util")
local string_util = require("utils.string-util")
local jdtls_util = require("utils.java.jdtls-util")

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
