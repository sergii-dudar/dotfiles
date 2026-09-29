-- Terminal spinner widget: animated progress indicator for async operations.
--
-- • start — create and start a spinner with title
-- • update — update spinner message text
-- • stop — stop spinner with success message
-- • cancel — stop spinner with cancellation message

local M = {}

-- stylua: ignore
local spinner_frames = { "⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏" }
-- local spinner_frames = { "⠁","⠂","⠄","⡀","⢀","⠠","⠐","⠈" },
-- local spinner_frames = { "⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏" },
-- local spinner_frames = { "⠋","⠙","⠚","⠒","⠂","⠂","⠒","⠲","⠴","⠦","⠖","⠒","⠐","⠐","⠒","⠓","⠋" },
-- local spinner_frames = { "⠁","⠉","⠙","⠚","⠒","⠂","⠂","⠒","⠲","⠴","⠤","⠄","⠄","⠤","⠴","⠲","⠒","⠂","⠂","⠒","⠚","⠙","⠉","⠁" },
-- local spinner_frames = { "◐","◓","◑","◒" },
-- local spinner_frames = { "◴","◷","◶","◵" },
-- local spinner_frames = { "▖","▘","▝","▗" },
-- local spinner_frames = { "▌","▀","▐","▄" },
-- local spinner_frames = { "←","↖","↑","↗","→","↘","↓","↙" },
-- local spinner_frames = { "⣾","⣽","⣻","⢿","⡿","⣟","⣯","⣷" },
-- local spinner_frames = { "🭑","🭓","🭕","🭒" },
-- local spinner_frames = { "🌝", "🌑","🌒","🌓","🌔","🌕","🌖","🌗","🌘", "🌚" },
-- local spinner_frames = { "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█" },
-- local spinner_frames = { "🕛", "🕧", "🕐", "🕜", "🕑", "🕝", "🕒", "🕞", "🕓", "🕟", "🕔", "🕠", "🕕", "🕡", "🕖", "🕢", "🕗", "🕣", "🕘", "🕤", "🕙", "🕥", "🕚", "🕦" },

-- Animation state: self-managed timer avoids Snacks.notifier `opts = function()`
-- re-render-every-tick path that causes nvim__redraw({ flush = true }) every 50ms
-- and disrupts Neovim's typeahead/keymap state machine (e.g., leader key).
--
-- One animation per notification id, so concurrent spinners (e.g. the MapStruct server
-- start and a test report) neither share a timer nor stop/hijack each other. Callers that
-- pass no id share the default id, exactly as before.
local DEFAULT_ID = "spinner"

---@class spinner.Anim
---@field timer uv.uv_timer_t|nil
---@field frame integer
---@field msg string
---@field title string

---@type table<string, spinner.Anim>
local anims = {}

---@param anim spinner.Anim
local function next_icon(anim)
    anim.frame = (anim.frame % #spinner_frames) + 1
    return spinner_frames[anim.frame]
end

-- Highlight for the optional dim tail of a stop message (e.g. " in 1.23s"): Comment's
-- colour (gray in most schemes) plus italic. `default = true` never overrides a user
-- definition, and the group is re-created after `:colorscheme` clears it.
local DIM_HL = "SpinnerNotifyDim"

local function ensure_dim_hl()
    Snacks.util.set_hl({ [DIM_HL] = { fg = Snacks.util.color("Comment"), italic = true } }, { default = true })
end

--- Notification style that renders with the globally configured Snacks style, then
--- highlights `tail` where it ends a message line (searched from the last line up).
---@param tail string
---@return snacks.notifier.render
local function dim_tail_style(tail)
    return function(buf, notif, ctx)
        ctx.notifier:get_render()(buf, notif, ctx)
        ensure_dim_hl()
        local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
        for row = #lines, 1, -1 do
            local line = lines[row]
            if #line >= #tail and line:sub(-#tail) == tail then
                pcall(vim.api.nvim_buf_set_extmark, buf, ctx.ns, row - 1, #line - #tail, {
                    end_col = #line,
                    hl_group = DIM_HL,
                })
                return
            end
        end
    end
end

---@param id string
local function stop_animation(id)
    local anim = anims[id]
    if not anim then
        return
    end
    anims[id] = nil
    if anim.timer then
        anim.timer:stop()
        anim.timer:close()
        anim.timer = nil
    end
end

---@param msg string
---@param opts? { id?: string, title?: string }
function M.start(msg, opts)
    opts = opts or {}
    local id = opts.id or DEFAULT_ID
    stop_animation(id)
    local anim = { timer = nil, frame = 0, msg = msg, title = opts.title or "" }
    anims[id] = anim

    Snacks.notifier.notify(msg, "info", {
        id = id,
        title = anim.title,
        timeout = false,
        icon = next_icon(anim),
    })

    anim.timer = vim.uv.new_timer()
    anim.timer:start(
        200,
        200,
        vim.schedule_wrap(function()
            if anims[id] ~= anim then
                return
            end
            Snacks.notifier.notify(anim.msg, "info", {
                id = id,
                title = anim.title,
                timeout = false,
                icon = next_icon(anim),
            })
        end)
    )
end

---@param msg string
---@param opts? { id?: string, title?: string }
function M.update(msg, opts)
    opts = opts or {}
    local id = opts.id or DEFAULT_ID
    local anim = anims[id]
    if anim then
        anim.msg = msg
        if opts.title then
            anim.title = opts.title
        end
    end
    Snacks.notifier.notify(msg, "info", {
        id = id,
        title = opts.title or (anim and anim.title) or "",
        timeout = false,
        icon = anim and next_icon(anim) or spinner_frames[1],
    })
end

---@param success boolean
---@param msg? string
---@param opts? { id?: string, title?: string, timeout?: number, dim_tail?: string }
---  dim_tail: trailing part of `msg` (e.g. " in 1.23s") rendered gray + italic.
function M.stop(success, msg, opts)
    opts = opts or {}
    local id = opts.id or DEFAULT_ID
    local anim = anims[id]
    stop_animation(id)
    local title = opts.title or (anim and anim.title) or ""
    local icon = success and "✅" or "❌"
    local level = success and "info" or "error"
    local text = msg or (success and "Done" or "Failed")
    local style = nil
    if type(opts.dim_tail) == "string" and opts.dim_tail ~= "" then
        style = dim_tail_style(opts.dim_tail)
    end
    Snacks.notifier.notify(text, level, {
        id = id,
        title = title,
        icon = icon,
        timeout = opts.timeout,
        style = style,
    })
end

---@param opts? { id?: string }
function M.cancel(opts)
    opts = opts or {}
    local id = opts.id or DEFAULT_ID
    stop_animation(id)
    Snacks.notifier.hide(id)
end

return M
