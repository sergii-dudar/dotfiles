-- MapStruct Completion Source for blink.cmp
-- Provides path completion for MapStruct @Mapping annotations
-- This is a thin wrapper around the isolated MapStruct module

local mapstruct = require("utils.java.mapstruct")
local logging_util = require("utils.logging-util")
local log = logging_util.new({ name = "MapStruct.BlinkSource", filename = "mapstruct-source.log" })

--- @class blink.cmp.Source
local source = {}

-- Initialize the source
function source.new(opts)
    local self = setmetatable({}, { __index = source })
    self.opts = opts or {}

    -- Initialize the underlying MapStruct module
    local success = mapstruct.setup(opts)
    if not success then
        return self
    end

    -- Setup user commands (delegated to isolated module)
    mapstruct.setup_commands()

    -- Set log level for the blink source wrapper
    if self.opts.log_level then
        local numeric_level = logging_util.level_to_number(self.opts.log_level)
        log.set_level(numeric_level)
        log.info("Blink.cmp MapStruct source initialized")
    end

    return self
end

-- Enable the source only for Java files with *Mapper.java naming pattern
function source:enabled()
    return mapstruct.is_mapper_file()
end

-- Trigger on dot character
function source:get_trigger_characters()
    return { "." }
end

-- Helper function to simplify type names
local function simplify_type(type_name)
    if not type_name then
        return "Unknown"
    end
    -- Extract simple name from fully qualified name
    -- e.g., "java.lang.String" -> "String"
    local simple = type_name:match("%.([^%.]+)$") or type_name
    return simple
end

-- Helper function to get full type with package
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

-- Process completion results into blink.cmp items
local function process_completions(result, completion_ctx)
    if not result or not result.completions then
        return {}
    end

    local items = {}
    local completions = result.completions or {}

    for _, field_info in ipairs(completions) do
        -- Use appropriate kind based on field kind
        local kind
        local kind_label

        if field_info.kind == "PARAMETER" then
            kind = require("blink.cmp.types").CompletionItemKind.Variable
            kind_label = "Parameter"
        elseif field_info.kind == "GETTER" then
            kind = require("blink.cmp.types").CompletionItemKind.Field
            kind_label = "Getter Method"
        elseif field_info.kind == "SETTER" then
            kind = require("blink.cmp.types").CompletionItemKind.Property
            kind_label = "Setter Method"
        else
            kind = require("blink.cmp.types").CompletionItemKind.Field
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

-- Get completions
function source:get_completions(ctx, callback)
    -- Get cursor position (ctx uses 1-indexed line, 0-indexed character)
    local params = {
        bufnr = ctx.bufnr,
        row = ctx.cursor[1] - 1, -- Convert to 0-indexed
        col = ctx.cursor[2], -- Already 0-indexed
    }

    -- Get completion context first
    local completion_ctx = mapstruct.get_context(params)

    if not completion_ctx then
        -- Not in a valid MapStruct context
        callback({ items = {}, is_incomplete_forward = false, is_incomplete_backward = false })
        return function() end
    end

    -- Get completions from MapStruct module
    mapstruct.get_completions(params, function(result, err)
        if err then
            log.warn("get_completions failed:", err)
            callback({ items = {}, is_incomplete_forward = false, is_incomplete_backward = false })
            return
        end

        -- Process completions into blink.cmp format
        local items = process_completions(result, completion_ctx)

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

-- Resolve additional details for an item
function source:resolve(item, callback)
    -- All information is provided upfront
    callback(item)
end

return source