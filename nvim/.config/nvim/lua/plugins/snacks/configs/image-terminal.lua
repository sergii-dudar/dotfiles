-- Whether this nvim is drawn by a terminal that speaks the kitty graphics protocol, decided once at startup so
-- snacks.image can be switched on only there. Snacks' doc rendering does not check terminal support itself (and
-- `force = true` skipped the remaining check), so in alacritty or foot it would open empty image floats.
--
-- Inside tmux only the attached client's XTVERSION reply (#{client_termtype}) is reliable: GHOSTTY_* / KITTY_* /
-- TERM_PROGRAM in a pane belong to whichever terminal started the tmux server. Same rule as
-- scripts/terminal-themes/common.sh. An nvim keeps its decision when the tmux session is later attached from another
-- terminal; restart it there.
local M = {}

-- kitty and ghostty draw images inline (unicode placeholders). wezterm dropped after a trial on 2026-10-16: outside
-- tmux only a float under the cursor (needs `config.enable_kitty_graphics = true` in wezterm.lua), inside tmux images
-- land misplaced, go missing, or show as a row of placeholder characters (0000...).
-- local GRAPHICS = { "kitty", "ghostty", "wezterm" }
local GRAPHICS = { "kitty", "ghostty" }

---@return string terminal name in lower case, "" when unknown
function M.name()
    if vim.env.TMUX then
        -- -t this pane: without it tmux answers for the most recently active session, which may be attached from
        -- another terminal
        local cmd = { "tmux", "display-message", "-p", "#{client_termtype}" }
        if vim.env.TMUX_PANE then
            cmd = { "tmux", "display-message", "-p", "-t", vim.env.TMUX_PANE, "#{client_termtype}" }
        end
        local res = vim.system(cmd, { text = true }):wait(1000)
        return res.code == 0 and vim.trim(res.stdout or ""):lower() or ""
    end
    local term, program = (vim.env.TERM or ""):lower(), (vim.env.TERM_PROGRAM or ""):lower()
    if term == "xterm-kitty" or vim.env.KITTY_PID then
        return "kitty"
    elseif term == "xterm-ghostty" or program == "ghostty" then
        return "ghostty"
    elseif program == "wezterm" or vim.env.WEZTERM_PANE then
        return "wezterm"
    end
    -- vim.notify(program ~= "" and program or term)
    return program ~= "" and program or term
end

---@return boolean
function M.supports_images()
    local name = M.name()
    for _, t in ipairs(GRAPHICS) do
        if name:find(t, 1, true) then
            return true
        end
    end
    return false
end

return M
