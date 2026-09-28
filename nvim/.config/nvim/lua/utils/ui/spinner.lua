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
local anim_timer = nil
local anim_frame = 0
local anim_id = nil
local anim_msg = nil
local anim_title = nil

local function next_icon()
    anim_frame = (anim_frame % #spinner_frames) + 1
    return spinner_frames[anim_frame]
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

local function stop_animation()
    if anim_timer then
        anim_timer:stop()
        anim_timer:close()
        anim_timer = nil
    end
end

---@param msg string
---@param opts? { id?: string, title?: string }
function M.start(msg, opts)
    opts = opts or {}
    anim_id = opts.id or "spinner"
    anim_title = opts.title or ""
    anim_msg = msg

    Snacks.notifier.notify(msg, "info", {
        id = anim_id,
        title = anim_title,
        timeout = false,
        icon = next_icon(),
    })

    stop_animation()
    anim_timer = vim.uv.new_timer()
    anim_timer:start(
        200,
        200,
        vim.schedule_wrap(function()
            if not anim_timer then
                return
            end
            Snacks.notifier.notify(anim_msg, "info", {
                id = anim_id,
                title = anim_title,
                timeout = false,
                icon = next_icon(),
            })
        end)
    )
end

---@param msg string
---@param opts? { id?: string, title?: string }
function M.update(msg, opts)
    opts = opts or {}
    anim_msg = msg
    if opts.id then
        anim_id = opts.id
    end
    if opts.title then
        anim_title = opts.title
    end
    Snacks.notifier.notify(msg, "info", {
        id = anim_id,
        title = anim_title,
        timeout = false,
        icon = next_icon(),
    })
end

---@param success boolean
---@param msg? string
---@param opts? { id?: string, title?: string, timeout?: number, dim_tail?: string }
---  dim_tail: trailing part of `msg` (e.g. " in 1.23s") rendered gray + italic.
function M.stop(success, msg, opts)
    stop_animation()
    opts = opts or {}
    local id = opts.id or anim_id or "spinner"
    local title = opts.title or ""
    local icon = success and "✅" or "❌"
    local level = success and "info" or "error"
    local text = msg or (success and "Done" or "Failed")
    anim_id = nil
    anim_msg = nil
    anim_title = nil
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
    stop_animation()
    opts = opts or {}
    local id = opts.id or anim_id or "spinner"
    anim_id = nil
    anim_msg = nil
    anim_title = nil
    Snacks.notifier.hide(id)
end

return M
