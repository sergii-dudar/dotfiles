--- Resolver for MapStruct diagnostics suggesting a custom mapping method.
---
--- It inserts the suggested method into the owning mapper type and leaves the
--- cursor in its return statement so conversion semantics remain user-defined.

local java_context = require("modules.java.diagnostics-resolver.java-context")
local lsp_java = require("utils.lang.java.lsp-java")

local M = {}

---@class MapStructSuggestedMethod
---@field signature string
---@field return_type string
---@field name string
---@field parameters string

--- Parse the method signature suggested by a MapStruct diagnostic.
---@param message string
---@return MapStructSuggestedMethod|nil
function M.parse_suggested_method(message)
    local raw = message:match('Consider to declare/implement a mapping method:%s*"([^"]+)"')
    if not raw then
        return nil
    end

    local signature = vim.trim(raw)
    local return_type, name, parameters = signature:match("^(.+)%s+([%a_$][%w_$]*)%s*(%b())$")
    if not return_type then
        return nil
    end

    return {
        signature = signature,
        return_type = vim.trim(return_type),
        name = name,
        parameters = parameters,
    }
end

--- Check whether the suggested signature is already present in the buffer.
---@param bufnr integer
---@param signature string
---@return boolean
local function method_exists(bufnr, signature)
    for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)) do
        if line:find(signature, 1, true) then
            return true
        end
    end
    return false
end

--- Find the generated parameter type position used by the import resolver.
---@param suggested MapStructSuggestedMethod
---@param method_line integer one-based line
---@param signature_column integer zero-based signature start column
---@return integer[]|nil cursor
local function parameter_type_cursor(suggested, method_line, signature_column)
    local parameter_type = suggested.parameters:match("^%(%s*(.-)%s+[%a_$][%w_$]*%s*%)$")
    local simple_type = parameter_type and parameter_type:match("([%u][%w_$]*)")
    local type_start = simple_type and suggested.signature:find(simple_type, 1, true)
    if not type_start then
        return nil
    end
    return { method_line, signature_column + type_start - 1 }
end

--- Resolve the generated parameter type import after receiving the buffer change.
---@param bufnr integer
---@param import_cursor integer[]|nil
local function resolve_imports(bufnr, import_cursor)
    if not import_cursor then
        return
    end

    vim.defer_fn(function()
        if vim.api.nvim_get_current_buf() ~= bufnr then
            return
        end

        local restore_cursor = vim.api.nvim_win_get_cursor(0)
        vim.api.nvim_win_set_cursor(0, import_cursor)
        pcall(lsp_java.resolve_imports)
        vim.api.nvim_win_set_cursor(0, restore_cursor)
    end, 250)
end

--- Insert a suggested mapping method into its owning mapper type.
---@param bufnr integer
---@param diagnostic table
---@param suggested MapStructSuggestedMethod
---@return boolean
local function insert_mapping_method(bufnr, diagnostic, suggested)
    if method_exists(bufnr, suggested.signature) then
        vim.notify("[MapStruct] Mapping method already exists: " .. suggested.signature, vim.log.levels.INFO)
        return false
    end

    local method = java_context.method_at_diagnostic(bufnr, diagnostic)
    if not method then
        vim.notify("[MapStruct] Could not find method for diagnostic", vim.log.levels.WARN)
        return false
    end

    local owner = java_context.enclosing_type(method)
    if not owner then
        vim.notify("[MapStruct] Could not find mapper type for diagnostic", vim.log.levels.WARN)
        return false
    end

    local owner_kind = owner:type()
    local modifier = owner_kind == "interface_declaration" and "default" or "protected"
    local method_row = method:start()
    local member_indent = java_context.line_indent(bufnr, method_row)
    if member_indent == "" then
        member_indent = java_context.line_indent(bufnr, owner:start()) .. java_context.indent_unit(bufnr)
    end
    local body_indent = member_indent .. java_context.indent_unit(bufnr)

    local _, _, owner_end_row = owner:range()
    local lines = {
        "",
        member_indent .. modifier .. " " .. suggested.signature .. " {",
        body_indent .. "return ;",
        member_indent .. "}",
    }
    vim.api.nvim_buf_set_lines(bufnr, owner_end_row, owner_end_row, false, lines)

    local return_line = owner_end_row + 3
    local return_column = #body_indent + #"return "
    local method_line = owner_end_row + 2
    local signature_column = #member_indent + #modifier + 1
    local import_cursor = parameter_type_cursor(suggested, method_line, signature_column)
    vim.api.nvim_win_set_cursor(0, { return_line, return_column })
    vim.notify("[MapStruct] Added mapping method: " .. suggested.signature, vim.log.levels.INFO)
    resolve_imports(bufnr, import_cursor)
    vim.cmd("startinsert")
    return true
end

--- Resolve a MapStruct custom mapping-method diagnostic.
---@param ctx { bufnr: integer, diagnostic: table }
---@return boolean
function M.resolve(ctx)
    local suggested = M.parse_suggested_method(ctx.diagnostic.message or "")
    if not suggested then
        vim.notify("[MapStruct] Could not parse suggested mapping method", vim.log.levels.WARN)
        return false
    end
    return insert_mapping_method(ctx.bufnr, ctx.diagnostic, suggested)
end

return M
