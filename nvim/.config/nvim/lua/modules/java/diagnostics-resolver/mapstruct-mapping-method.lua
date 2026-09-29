--- Resolver for MapStruct diagnostics suggesting a custom mapping method.
---
--- It inserts the suggested method into the owning mapper type and leaves the
--- cursor in its return statement so conversion semantics remain user-defined.

local java_context = require("modules.java.diagnostics-resolver.java-context")
local java_import_resolver = require("modules.java.diagnostics-resolver.java-import-resolver")
local mapstruct_method_type_resolver = require("modules.java.diagnostics-resolver.mapstruct-method-type-resolver")
local mapstruct_reference = require("modules.java.diagnostics-resolver.mapstruct-diagnostic-reference")

local M = {}

---@class MapStructSuggestedMethod
---@field signature string
---@field return_type string
---@field name string
---@field parameters string
---@field parameter_type string
---@field parameter_name string
---@field source_type string
---@field source_property string MapStruct path, e.g. `ttl` or `source.config.ttl`
---@field target_type string
---@field target_property string MapStruct path
---@field element_kind? string MapStruct element kind when the source is not a plain property

--- Parse the method signature suggested by a MapStruct diagnostic.
---@param message string
---@return MapStructSuggestedMethod|nil
function M.parse_suggested_method(message)
    local kind, source, target = message:match('Can\'t map ([%a ]-)%s+"([^"]+)"%s+to%s+"([^"]+)"')
    local raw = message:match('Consider to declare/implement a mapping method:%s*"([^"]+)"')
    if not source or not target or not raw then
        return nil
    end
    -- Whole-parameter mappings belong to the parameter mapping-method resolver.
    if kind ~= mapstruct_reference.PROPERTY_KIND and not mapstruct_reference.is_element_kind(kind) then
        return nil
    end

    local signature = vim.trim(raw)
    local return_type, name, parameters = signature:match("^(.+)%s+([%a_$][%w_$]*)%s*(%b())$")
    local parameter_type, parameter_name = nil, nil
    if parameters then
        parameter_type, parameter_name = parameters:match("^%(%s*(.-)%s+([%a_$][%w_$]*)%s*%)$")
    end
    local source_type, source_property = mapstruct_reference.parse_typed_reference(source)
    local target_type, target_property = mapstruct_reference.parse_typed_reference(target)
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
        element_kind = mapstruct_reference.element_kind(kind),
    }
end

--- Check whether the suggested signature is already present in the buffer.
---@param bufnr integer
---@param signature string
---@return boolean
local function method_exists(bufnr, signature)
    return java_context.method_exists(bufnr, signature)
end

--- Pick a signature that does not clash with an existing overload.
--- MapStruct always suggests `map`; two suggestions sharing a parameter type would
--- otherwise produce methods that differ only by return type, which Java rejects.
--- The suggested name is kept unless it clashes, then `to<ReturnType>` is used.
---@param bufnr integer
---@param suggested_name string
---@param return_type string rendered return type
---@param parameter_type string rendered parameter type
---@param parameter_name string
---@return string|nil signature signature to declare
---@return string|nil existing signature that is already declared, when there is one
local function available_signature(bufnr, suggested_name, return_type, parameter_type, parameter_name)
    local names = { suggested_name, mapstruct_reference.method_name_for(return_type) }
    return java_context.available_signature(bufnr, names, { parameter_type }, function(name)
        return return_type .. " " .. name .. "(" .. parameter_type .. " " .. parameter_name .. ")"
    end)
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

    local available, existing =
        available_signature(bufnr, suggested.name, return_type, parameter_type, suggested.parameter_name)
    if existing then
        vim.notify("[MapStruct] Mapping method already exists: " .. existing, vim.log.levels.INFO)
        return false
    end
    if not available then
        vim.notify(
            "[MapStruct] Mapping method name is already taken for parameter type: " .. signature,
            vim.log.levels.WARN
        )
        return false
    end
    signature = available

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
