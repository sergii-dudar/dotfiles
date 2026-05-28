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
---@param opts? { id?: string, title?: string, timeout?: number }
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
    Snacks.notifier.notify(text, level, {
        id = id,
        title = title,
        icon = icon,
        timeout = opts.timeout,
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
