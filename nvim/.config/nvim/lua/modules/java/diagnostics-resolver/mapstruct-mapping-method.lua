--- Resolver for MapStruct diagnostics suggesting a custom mapping method.
---
--- It inserts the suggested method into the owning mapper type and leaves the
--- cursor in its return statement so conversion semantics remain user-defined.

local java_context = require("modules.java.diagnostics-resolver.java-context")
local java_import_resolver = require("modules.java.diagnostics-resolver.java-import-resolver")
local mapstruct_method_type_resolver = require("modules.java.diagnostics-resolver.mapstruct-method-type-resolver")

local M = {}

---@class MapStructSuggestedMethod
---@field signature string
---@field return_type string
---@field name string
---@field parameters string
---@field parameter_type string
---@field parameter_name string
---@field source_type string
---@field source_property string
---@field target_type string
---@field target_property string

--- Parse the method signature suggested by a MapStruct diagnostic.
---@param message string
---@return MapStructSuggestedMethod|nil
function M.parse_suggested_method(message)
    local source, target = message:match('Can\'t map property%s+"([^"]+)"%s+to%s+"([^"]+)"')
    local raw = message:match('Consider to declare/implement a mapping method:%s*"([^"]+)"')
    if not source or not target or not raw then
        return nil
    end

    local signature = vim.trim(raw)
    local return_type, name, parameters = signature:match("^(.+)%s+([%a_$][%w_$]*)%s*(%b())$")
    local parameter_type, parameter_name = nil, nil
    if parameters then
        parameter_type, parameter_name = parameters:match("^%(%s*(.-)%s+([%a_$][%w_$]*)%s*%)$")
    end
    local source_type, source_property = source:match("^%s*(.-)%s+([%a_$][%w_$]*)%s*$")
    local target_type, target_property = target:match("^%s*(.-)%s+([%a_$][%w_$]*)%s*$")
    if not return_type or not parameter_type or not source_type or not target_type then
        return nil
    end

    return {
        signature = signature,
        return_type = vim.trim(return_type),
        name = name,
        parameters = parameters,
        parameter_type = vim.trim(parameter_type),
        parameter_name = parameter_name,
        source_type = source_type,
        source_property = source_property,
        target_type = target_type,
        target_property = target_property,
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

--- Add resolved generic arguments to the import plan for one method type.
---@param imports table[]
---@param key string
---@param arguments JavaResolvedType[]|nil
local function add_argument_imports(imports, key, arguments)
    for index, argument in ipairs(arguments or {}) do
        imports[#imports + 1] = { key = key .. "_argument_" .. index, type = argument }
    end
end

--- Render a planned type reference with its resolved generic arguments.
---@param references table<string, string>
---@param key string
---@param arguments JavaResolvedType[]|nil
---@return string
local function render_type_reference(references, key, arguments)
    local argument_references = {}
    for index, _ in ipairs(arguments or {}) do
        argument_references[#argument_references + 1] = references[key .. "_argument_" .. index]
    end
    if #argument_references == 0 then
        return references[key]
    end
    return references[key] .. "<" .. table.concat(argument_references, ", ") .. ">"
end

--- Insert a suggested mapping method into its owning mapper type.
---@param bufnr integer
---@param diagnostic table
---@param suggested MapStructSuggestedMethod
---@param resolved_types { source: JavaResolvedType, source_arguments?: JavaResolvedType[], target: JavaResolvedType, target_arguments?: JavaResolvedType[] }
---@return boolean
local function insert_mapping_method(bufnr, diagnostic, suggested, resolved_types)
    local imports = {
        { key = "parameter", type = resolved_types.source },
        { key = "return", type = resolved_types.target },
    }
    add_argument_imports(imports, "parameter", resolved_types.source_arguments)
    add_argument_imports(imports, "return", resolved_types.target_arguments)

    local references, imports_or_error = java_import_resolver.plan(bufnr, imports)
    if not references then
        vim.notify("[MapStruct] Could not plan mapping method imports: " .. imports_or_error, vim.log.levels.WARN)
        return false
    end

    local return_type = render_type_reference(references, "return", resolved_types.target_arguments)
    local parameter_type = render_type_reference(references, "parameter", resolved_types.source_arguments)
    local signature = return_type
        .. " "
        .. suggested.name
        .. "("
        .. parameter_type
        .. " "
        .. suggested.parameter_name
        .. ")"
    if method_exists(bufnr, signature) then
        vim.notify("[MapStruct] Mapping method already exists: " .. signature, vim.log.levels.INFO)
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

    local lines = {
        member_indent .. modifier .. " " .. signature .. " {",
        body_indent .. "return ;",
        member_indent .. "}",
    }
    local insert_row = java_context.insert_after_method(bufnr, method, lines)

    local inserted_import_lines = java_import_resolver.apply(bufnr, imports_or_error)
    local return_line = insert_row + 3 + inserted_import_lines
    local return_column = #body_indent + #"return "
    vim.api.nvim_win_set_cursor(0, { return_line, return_column })
    vim.notify("[MapStruct] Added mapping method: " .. signature, vim.log.levels.INFO)
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

    mapstruct_method_type_resolver.resolve(ctx, suggested, function(resolved_types, err)
        if not resolved_types then
            vim.notify(
                "[MapStruct] Could not resolve mapping method types: " .. (err or "unknown error"),
                vim.log.levels.WARN
            )
            return
        end
        if vim.api.nvim_get_current_buf() ~= ctx.bufnr then
            vim.notify("[MapStruct] Mapper buffer is no longer active", vim.log.levels.WARN)
            return
        end
        insert_mapping_method(ctx.bufnr, ctx.diagnostic, suggested, resolved_types)
    end)
    return true
end

return M
