-- MapStruct Completion Source for blink.cmp
-- Provides path completion for MapStruct @Mapping annotations
-- This is a thin wrapper around the isolated MapStruct module

local mapstruct = require("modules.java.mapstruct")
local logging_util = require("utils.logging-util")
local log = logging_util.new({ name = "MapStruct.BlinkSource", filename = "mapstruct-source.log" })

--- @class blink.cmp.Source
local source = {}
local completion_item_kind = nil

--- Initialize the blink.cmp MapStruct source.
---@param opts? table
---@return table
function source.new(opts)
    local self = setmetatable({}, { __index = source })
    self.opts = opts or {}

    -- Initialize the underlying MapStruct module (commands are set up automatically)
    local success = mapstruct.setup(self.opts)
    if not success then
        return self
    end

    -- Set log level for the blink source wrapper
    if self.opts.log_level then
        local numeric_level = logging_util.level_to_number(self.opts.log_level)
        log.set_level(numeric_level)
        log.info("Blink.cmp MapStruct source initialized")
    end

    return self
end

--- Enable the source only for Java mapper/builder files.
---@return boolean
function source:enabled()
    return mapstruct.is_mapper_file()
end

--- Return trigger characters for MapStruct path completion.
---@return string[]
function source:get_trigger_characters()
    return { "." }
end

--- Return a display-friendly simple type name.
---@param type_name string|nil
---@return string
local function simplify_type(type_name)
    if not type_name then
        return "Unknown"
    end
    -- Extract simple name from fully qualified name
    -- e.g., "java.lang.String" -> "String"
    local simple = type_name:match("%.([^%.]+)$") or type_name
    return simple
end

--- Return a documentation type name, shortening java.lang classes.
---@param type_name string|nil
---@return string
local function format_type_with_package(type_name)
    if not type_name then
        return "Unknown"
    end
    -- If it's a java.lang type, just show simple name
    if type_name:match("^java%.lang%.") then
        return simplify_type(type_name)
    end
    return type_name
end

--- Return blink.cmp completion item kinds.
---@return table
local function get_completion_item_kind()
    if not completion_item_kind then
        completion_item_kind = require("blink.cmp.types").CompletionItemKind
    end

    return completion_item_kind
end

--- Process MapStruct completion results into blink.cmp items.
---@param result table|nil
---@param completion_ctx table|nil
---@return table[]
local function process_completions(result, completion_ctx)
    if not result or not result.completions then
        return {}
    end

    local items = {}
    local completions = result.completions or {}
    local item_kind = get_completion_item_kind()

    for _, field_info in ipairs(completions) do
        -- Use appropriate kind based on field kind
        local kind
        local kind_label

        if field_info.kind == "PARAMETER" then
            kind = item_kind.Variable
            kind_label = "Parameter"
        elseif field_info.kind == "GETTER" then
            kind = item_kind.Field
            kind_label = "Getter Method"
        elseif field_info.kind == "SETTER" then
            kind = item_kind.Property
            kind_label = "Setter Method"
        else
            kind = item_kind.Field
            kind_label = "Field"
        end

        local simple_type = simplify_type(field_info.type)
        local full_type = format_type_with_package(field_info.type)

        local item = {
            label = field_info.name,
            labelDetails = {
                description = simple_type,
            },
            kind = kind,
            insertTextFormat = vim.lsp.protocol.InsertTextFormat.PlainText,
            insertText = field_info.name,
            documentation = {
                kind = "markdown",
                value = string.format(
                    "**%s**: %s\n**Type:** `%s`\n**Kind:** %s%s%s\n**Path:** `%s%s`",
                    kind_label,
                    field_info.name,
                    full_type,
                    field_info.kind,
                    result.simpleName and ("\n**Source Class:** `" .. result.simpleName .. "`") or "",
                    result.packageName and ("\n**Package:** `" .. result.packageName .. "`") or "",
                    completion_ctx and completion_ctx.path_expression or "",
                    field_info.name
                ),
            },
            data = {
                mapstruct_field_type = field_info.type,
                mapstruct_field_kind = field_info.kind,
                mapstruct_class_name = result.className,
                mapstruct_package = result.packageName,
            },
        }
        table.insert(items, item)
    end

    return items
end

--- Get MapStruct completions for blink.cmp.
---@param ctx table
---@param callback fun(response: table)
---@return fun()
function source:get_completions(ctx, callback)
    ctx = ctx or {}
    local cursor = ctx.cursor or vim.api.nvim_win_get_cursor(0)

    -- Get cursor position (ctx uses 1-indexed line, 0-indexed character)
    local params = {
        bufnr = ctx.bufnr or vim.api.nvim_get_current_buf(),
        row = cursor[1] - 1, -- Convert to 0-indexed
        col = cursor[2], -- Already 0-indexed
    }

    -- Get completions from MapStruct module
    -- Note: get_completions will internally call get_completion_context
    mapstruct.get_completions(params, function(result, err)
        if err then
            log.warn("get_completions failed:", err)
            callback({ items = {}, is_incomplete_forward = false, is_incomplete_backward = false })
            return
        end

        -- result.completion_ctx contains the context if needed
        -- Process completions into blink.cmp format
        local items = process_completions(result, result.completion_ctx)

        callback({
            items = items,
            is_incomplete_forward = false,
            is_incomplete_backward = false,
        })
    end)

    -- Return cancel function
    return function()
        -- TODO: Implement request cancellation if needed
    end
end

--- Resolve additional details for an item.
---@param item table
---@param callback fun(item: table)
function source:resolve(item, callback)
    -- All information is provided upfront
    callback(item)
end

return source
