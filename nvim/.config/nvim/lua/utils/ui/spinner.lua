local M = {}

-- stylua: ignore
local spinner_frames = { "⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏" }

---@param msg string
---@param opts? { id?: string, title?: string }
function M.start(msg, opts)
    opts = opts or {}
    local id = opts.id or "spinner"
    local title = opts.title or ""
    vim.notify(msg, "info", {
        id = id,
        title = title,
        timeout = false,
        opts = function(notif)
            notif.icon = spinner_frames[math.floor(vim.uv.hrtime() / (1e6 * 80)) % #spinner_frames + 1]
        end,
    })
end

---@param msg string
---@param opts? { id?: string, title?: string }
function M.update(msg, opts)
    M.start(msg, opts)
end

---@param success boolean
---@param msg string
---@param opts? { id?: string, title?: string }
function M.stop(success, msg, opts)
    opts = opts or {}
    local id = opts.id or "spinner"
    local title = opts.title or ""
    local icon = success and "✅" or "❌"
    local level = success and "info" or "error"
    vim.notify(msg, level, {
        id = id,
        title = title,
        icon = icon,
    })
end

---@param opts? { id?: string }
function M.cancel(opts)
    opts = opts or {}
    local id = opts.id or "spinner"
    Snacks.notifier.hide(id)
end

return M
