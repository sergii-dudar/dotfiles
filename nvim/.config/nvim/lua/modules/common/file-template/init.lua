-- Generic, filetype-agnostic file-template engine.
--
-- Fills a freshly created file with a language template instead of leaving an
-- empty buffer behind. Works with any way of creating a file (neo-tree, fyler,
-- oil, snacks explorer, yazi, plain `:e`), because the trigger is the buffer
-- itself: a blank buffer whose file is missing or zero bytes on disk.
--
-- Templates are LuaSnip snippets resolved by trigger, selected by ordered
-- per-language rules (see `modules/<lang>/file-template/rules.lua`).
--
-- Public entry points:
--   setup()            install the autocmd + `:FileTemplate` command
--   apply(opts)        resolve and expand a template into a buffer
--   pick(opts)         choose a template interactively
--   resolve(bufnr)     inspect what would be applied (used by tests/debugging)

require("modules.common.file-template.types")

local M = {}

local expand = require("modules.common.file-template.expand")
local matcher = require("modules.common.file-template.matcher")
local registry = require("modules.common.file-template.registry")

local uv = vim.uv or vim.loop

---@class file_template.Config
---@field enabled boolean Master switch for automatic expansion.
---@field notify boolean Notify which template was applied automatically.
M.config = {
    enabled = true,
    notify = false,
}

local setup_done = false

--- A buffer is a template candidate only when the user cannot lose anything:
--- normal, writable, empty buffer whose file is missing or empty on disk.
---@param bufnr integer
---@return boolean
local function is_blank_buffer(bufnr)
    if not vim.api.nvim_buf_is_loaded(bufnr) then
        return false
    end
    if vim.bo[bufnr].buftype ~= "" then
        return false
    end
    if not vim.bo[bufnr].modifiable or vim.bo[bufnr].readonly then
        return false
    end
    if vim.bo[bufnr].modified then
        return false
    end

    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    if #lines > 1 then
        return false
    end
    return #lines == 0 or lines[1] == ""
end

---@param path string
---@return boolean
local function is_blank_on_disk(path)
    local stat = uv.fs_stat(path)
    return stat == nil or (stat.type == "file" and stat.size == 0)
end

--- Build the match context for a buffer and resolve its language adapter.
---@param bufnr integer|nil
---@return file_template.Context|nil ctx, file_template.Adapter|nil adapter
function M.context(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()
    if not vim.api.nvim_buf_is_valid(bufnr) then
        return nil, nil
    end

    local filetype = vim.bo[bufnr].filetype
    local adapter = registry.get(filetype)
    if not adapter then
        return nil, nil
    end

    local path = vim.api.nvim_buf_get_name(bufnr)
    if path == "" then
        return nil, nil
    end

    ---@type file_template.Context
    local ctx = {
        bufnr = bufnr,
        filetype = filetype,
        path = path,
        dir = vim.fn.fnamemodify(path, ":h"),
        filename = vim.fn.fnamemodify(path, ":t"),
        basename = vim.fn.fnamemodify(path, ":t:r"),
        ext = vim.fn.fnamemodify(path, ":e"),
    }

    if adapter.context then
        ctx = vim.tbl_extend("force", ctx, adapter.context(ctx) or {})
    end

    return ctx, adapter
end

---@param adapter file_template.Adapter
---@param ctx file_template.Context
---@return string[]
local function snippet_filetypes(adapter, ctx)
    return adapter.snippet_filetypes or { ctx.filetype }
end

---@param adapter file_template.Adapter
---@param ctx file_template.Context
---@return file_template.Rule[]
local function adapter_rules(adapter, ctx)
    if not adapter.rules then
        return {}
    end
    local ok, rules = pcall(adapter.rules, ctx)
    if not ok then
        vim.notify("file-template: rules failed for " .. adapter.lang .. ": " .. tostring(rules), vim.log.levels.ERROR)
        return {}
    end
    return rules or {}
end

--- Resolve the rule that would be applied to a buffer, without touching it.
---@param bufnr integer|nil
---@return file_template.Rule|nil rule, file_template.Context|nil ctx, file_template.Adapter|nil adapter
function M.resolve(bufnr)
    local ctx, adapter = M.context(bufnr)
    if not ctx or not adapter then
        return nil, nil, nil
    end
    if adapter.enabled and not adapter.enabled(ctx) then
        return nil, ctx, adapter
    end
    return matcher.first_match(adapter_rules(adapter, ctx), ctx), ctx, adapter
end

--- Expand a template into a buffer.
--- Without `opts.trigger` the template is resolved from the adapter rules.
---@param opts? { bufnr?: integer, trigger?: string, choice?: integer, notify?: boolean }
---@return boolean applied
function M.apply(opts)
    opts = opts or {}
    local bufnr = opts.bufnr or vim.api.nvim_get_current_buf()

    local ctx, adapter = M.context(bufnr)
    if not ctx or not adapter then
        return false
    end

    local trigger, choice = opts.trigger, opts.choice
    if not trigger then
        if adapter.enabled and not adapter.enabled(ctx) then
            return false
        end
        local rule = matcher.first_match(adapter_rules(adapter, ctx), ctx)
        if not rule then
            return false
        end
        trigger, choice = rule.snippet, rule.choice
    end

    if bufnr ~= vim.api.nvim_get_current_buf() then
        return false
    end

    local ok, err = expand.expand(snippet_filetypes(adapter, ctx), trigger, { choice = choice })
    if not ok then
        vim.notify("file-template: " .. tostring(err), vim.log.levels.WARN)
        return false
    end

    vim.b[bufnr].file_template_applied = trigger
    if opts.notify or M.config.notify then
        vim.notify("file-template: applied '" .. trigger .. "' to " .. ctx.filename, vim.log.levels.INFO)
    end
    return true
end

--- Distinct template triggers offered by the buffer's adapter, in rule order.
---@param bufnr integer|nil
---@return { trigger: string, choice: integer|nil, desc: string }[]
function M.candidates(bufnr)
    local ctx, adapter = M.context(bufnr)
    if not ctx or not adapter then
        return {}
    end

    local seen, items = {}, {}
    for _, rule in ipairs(adapter_rules(adapter, ctx)) do
        local key = rule.snippet .. ":" .. tostring(rule.choice)
        if rule.snippet and not seen[key] then
            seen[key] = true
            items[#items + 1] = {
                trigger = rule.snippet,
                choice = rule.choice,
                desc = rule.desc or rule.snippet,
            }
        end
    end
    return items
end

--- Interactive template selection for the current buffer.
---@param opts? { bufnr?: integer }
function M.pick(opts)
    opts = opts or {}
    local bufnr = opts.bufnr or vim.api.nvim_get_current_buf()

    local items = M.candidates(bufnr)
    if #items == 0 then
        vim.notify(
            "file-template: no templates registered for filetype: " .. tostring(vim.bo[bufnr].filetype),
            vim.log.levels.WARN
        )
        return
    end

    vim.ui.select(items, {
        prompt = "File template",
        format_item = function(item)
            if item.desc == item.trigger then
                return item.trigger
            end
            return ("%-14s %s"):format(item.trigger, item.desc)
        end,
    }, function(item)
        if not item then
            return
        end
        M.apply({ bufnr = bufnr, trigger = item.trigger, choice = item.choice, notify = true })
    end)
end

--- Automatic entry point: a buffer of a supported filetype became available.
---@param bufnr integer
function M.on_filetype(bufnr)
    if not M.config.enabled or vim.g.file_template_enabled == false then
        return
    end
    if not vim.api.nvim_buf_is_valid(bufnr) then
        return
    end
    if vim.b[bufnr].file_template_applied or vim.b[bufnr].file_template_disabled then
        return
    end

    -- Deferred so the buffer content is fully read and the window is settled.
    vim.schedule(function()
        if not vim.api.nvim_buf_is_valid(bufnr) or vim.api.nvim_get_current_buf() ~= bufnr then
            return
        end
        if vim.b[bufnr].file_template_applied then
            return
        end

        local path = vim.api.nvim_buf_get_name(bufnr)
        if path == "" or not is_blank_on_disk(path) or not is_blank_buffer(bufnr) then
            return
        end

        M.apply({ bufnr = bufnr })
    end)
end

---@param opts? file_template.Config
function M.setup(opts)
    if setup_done then
        return
    end
    setup_done = true

    M.config = vim.tbl_extend("force", M.config, opts or {})

    local filetypes = registry.filetypes()
    if #filetypes > 0 then
        vim.api.nvim_create_autocmd("FileType", {
            group = vim.api.nvim_create_augroup("CommonFileTemplate", { clear = true }),
            pattern = filetypes,
            desc = "Fill a newly created empty file with its language template",
            callback = function(event)
                M.on_filetype(event.buf)
            end,
        })
    end

    vim.api.nvim_create_user_command("FileTemplate", function(cmd)
        if cmd.args ~= "" then
            M.apply({ trigger = cmd.args, notify = true })
        else
            M.pick()
        end
    end, {
        nargs = "?",
        desc = "Insert a file template into the current buffer",
        complete = function(arg_lead)
            local seen, triggers = {}, {}
            for _, item in ipairs(M.candidates()) do
                if not seen[item.trigger] and item.trigger:find(arg_lead, 1, true) == 1 then
                    seen[item.trigger] = true
                    triggers[#triggers + 1] = item.trigger
                end
            end
            return triggers
        end,
    })
end

return M
