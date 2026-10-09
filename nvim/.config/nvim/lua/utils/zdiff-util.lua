-- zdiff.nvim as a bottom review panel.
--
-- zdiff.nvim has no layout options: open() takes over the current window and its goto_file action (<CR>) runs
-- `:edit` in that same window, so reviewing means bouncing between the diff and the file inside one window.
--
-- • open — show the diff in a dedicated bottom split (reuses the panel when it is already open) and focus it
-- • goto_file — <CR> or a double click in the panel: open the file in an editor window and focus it; the panel
--   stays as it is.
--   On a file header line the cursor lands on the file's first change (zdiff itself goes to line 1); gitsigns
--   is pointed at the base zdiff compares against, so the gutter shows the reviewed changes (`]h` / `[h` walk them)
-- • editor_win — pick that window: previous window → window active when the panel was opened → largest regular
--   window; a new top split when there is none
--
-- zdiff keeps its state and goto_source() local, so the wrapper works through what the plugin exposes:
-- • the original <CR> callback is read from the buffer-local mapping zdiff installs and called with the editor
--   window focused: goto_source() reads the cursor from the panel window (state.win) and `:edit`s in the
--   current window
-- • `:edit <file>` for the file already shown in the target reloads it from disk and fails (E37) when the
--   buffer is modified, which is common while reviewing; vim.cmd is intercepted for that one call and the edit
--   is skipped when the file is already there (only the cursor moves)
-- • zdiff restores the window's number/signcolumn/winbar on BufLeave (it expects the window to be reused for
--   the file); a WinLeave hook re-applies the diff look so the panel does not change when focus leaves it
-- • the mode is read from the panel's first line (" zdiff: Changes vs <ref>" / " zdiff: Uncommitted changes"),
--   which also follows the in-place `m` toggle; a file header line is "<icon> <status> <path>  +N -N" while diff
--   lines are indented by two spaces
-- • zdiff diffs `<ref>...HEAD` in ref mode and the working tree vs HEAD in uncommitted mode. The first-change jump
--   and the per-buffer gitsigns base come from utils/git-review-util (shared with the Neo-tree changes view): a
--   file opened with <CR> gets the base, the files already on screen get it when the mode changes (open, `m`),
--   and every changed buffer is reverted when the zdiff buffer goes away (panel closed or ref switched).
--   open_default_branch() (<leader>zD) resolves the branch per repository instead of assuming "main"
-- • Neo-tree follows the panel the way `:Neotree <ref>` does (utils/neotree-util remember_git_base / set_git_base /
--   restore_git_base): while the panel is open the trees of this tab compare against its ref (Neo-tree's default
--   in uncommitted mode) and get the bases they had before the panel back when it closes. Updated on open, on the
--   in-place `m` toggle and on close

local review = require("utils.git-review-util")

local M = {}

M.config = {
    -- rows when >= 1, fraction of the editor height when < 1
    height = 0.4,
    -- <CR> on a file header line: land on the file's first change instead of line 1
    jump_to_first_change = true,
    -- in ref mode, point gitsigns at the base zdiff compares against for files opened from the panel
    sync_gitsigns_base = true,
    -- in ref mode, compare the Neo-tree trees against the panel's ref while the panel is open (`:Neotree <ref>`)
    sync_neotree_base = true,
    -- a double click on a panel line does what <CR> does (the first click already moves the cursor there)
    double_click_opens = true,
}

---@class ZdiffPanelState
---@field panel_win integer|nil window holding the zdiff buffer
---@field editor_win integer|nil window that was active when the panel was opened
---@field orig table<integer, table<string, fun()>> zdiff's own callbacks per zdiff buffer, by keymap name
---@field wrapper table<integer, table<string, fun()>> the panel-aware callbacks per zdiff buffer, by keymap name
---@field switching boolean true while open() replaces the zdiff buffer for another ref
local S = { panel_win = nil, editor_win = nil, orig = {}, wrapper = {}, switching = false }

local AUGROUP = "zdiff_panel"
local ZDIFF_AUGROUP = "zdiff" -- augroup zdiff.nvim registers its own autocmds in

--- Check whether a window is a regular file window: valid, not floating, not the panel, normal buftype.
---@param win integer|nil
---@param panel integer|nil
---@return boolean
local function is_editor_win(win, panel)
    if not win or win == panel or not vim.api.nvim_win_is_valid(win) then
        return false
    end
    if vim.api.nvim_win_get_config(win).relative ~= "" then
        return false
    end
    local buf = vim.api.nvim_win_get_buf(win)
    return vim.bo[buf].buftype == "" and vim.bo[buf].filetype ~= "zdiff"
end

--- Check whether a window currently shows a zdiff buffer.
---@param win integer|nil
---@return boolean
local function is_zdiff_win(win)
    return win ~= nil and vim.api.nvim_win_is_valid(win) and vim.bo[vim.api.nvim_win_get_buf(win)].filetype == "zdiff"
end

--- Resolve the panel height in rows from M.config.height.
---@return integer
local function panel_height()
    local height = M.config.height
    if height < 1 then
        height = vim.o.lines * height
    end
    return math.max(3, math.floor(height))
end

--- Re-apply zdiff's window look (mirrors its apply_zdiff_window_opts) and recompute the sticky file header.
---@param win integer
---@param buf integer zdiff buffer shown in win
local function restyle(win, buf)
    if not vim.api.nvim_win_is_valid(win) then
        return
    end
    vim.wo[win].number = false
    vim.wo[win].relativenumber = false
    vim.wo[win].signcolumn = "no"
    vim.wo[win].wrap = false
    vim.wo[win].cursorline = true
    -- zdiff's CursorMoved handler recomputes the winbar for the current window
    pcall(vim.api.nvim_exec_autocmds, "CursorMoved", { group = ZDIFF_AUGROUP, buffer = buf })
end

--- Find the callback of a buffer-local normal-mode mapping.
---@param buf integer
---@param lhs string
---@return fun()|nil
local function buf_keymap_callback(buf, lhs)
    local want = vim.api.nvim_replace_termcodes(lhs, true, true, true)
    for _, map in ipairs(vim.api.nvim_buf_get_keymap(buf, "n")) do
        if vim.api.nvim_replace_termcodes(map.lhs, true, true, true) == want then
            return map.callback
        end
    end
    return nil
end

--- Compare two paths by their resolved location.
---@param a string
---@param b string
---@return boolean
local function same_file(a, b)
    if a == "" or b == "" then
        return false
    end
    local ra = vim.uv.fs_realpath(a) or vim.fn.fnamemodify(a, ":p")
    local rb = vim.uv.fs_realpath(b) or vim.fn.fnamemodify(b, ":p")
    return ra == rb
end

--- Run fn with `vim.cmd("edit <file>")` calls routed through a reload-free edit; every other vim.cmd use passes
--- through untouched. Returns whether fn asked to edit a file at all.
---@param fn fun()
---@return boolean edited
local function with_edit_guard(fn)
    local real_cmd = vim.cmd
    local edited = false
    vim.cmd = setmetatable({}, {
        __index = function(_, key)
            return real_cmd[key]
        end,
        __call = function(_, command, ...)
            local escaped = type(command) == "string" and command:match("^edit%s+(.+)$") or nil
            if not escaped then
                return real_cmd(command, ...)
            end
            edited = true
            local path = (escaped:gsub("\\(.)", "%1")) -- undo fnameescape()
            if same_file(path, vim.api.nvim_buf_get_name(0)) then
                real_cmd("normal! m'") -- keep <C-o> working, like the jump :edit would have added
                return
            end
            return real_cmd(command, ...)
        end,
    })
    local ok, err = pcall(fn)
    vim.cmd = real_cmd
    if not ok then
        error(err, 0)
    end
    return edited
end

--- Read the ref the panel compares against from its first line; nil in uncommitted mode.
---@param buf integer zdiff buffer
---@return string|nil
local function panel_ref(buf)
    local first = vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1] or ""
    local ref = first:match("^ zdiff: Changes vs (.+)$")
    if ref then
        ref = (ref:gsub("%s*%(loading%.%.%.%)$", ""))
    end
    return ref
end

--- Check whether a panel line is a file header ("<icon> <status> <path>  +N -N"); diff lines are indented.
---@param line string
---@return boolean
local function is_file_header(line)
    return not line:find("^  ") and line:match("%+%d+ %-%d+$") ~= nil
end

--- Make the Neo-tree trees compare against the panel's ref (nil = uncommitted mode = Neo-tree's default). The
--- bases in place when the panel first touches them are remembered for release_tree().
---@param ref string|nil
local function sync_tree(ref)
    if not M.config.sync_neotree_base then
        return
    end
    local ok, neotree = pcall(require, "utils.neotree-util")
    if not ok then
        return
    end
    neotree.remember_git_base()
    neotree.set_git_base(ref)
end

--- Everything that follows the panel's mode: the Neo-tree trees and the gitsigns base of the file buffers on
--- screen (files opened later get theirs in goto_file()).
---@param ref string|nil
local function follow_mode(ref)
    sync_tree(ref)
    if M.config.sync_gitsigns_base then
        review.sync_visible_signs(ref)
    end
end

--- Give the Neo-tree trees the bases they had before the panel back, and let the file buffers on screen follow
--- that base again (the panel's close reset every gitsigns base it or the Git explorer had changed).
local function release_tree()
    if not M.config.sync_neotree_base then
        return
    end
    local ok, neotree = pcall(require, "utils.neotree-util")
    if not ok then
        return
    end
    neotree.restore_git_base()
    if M.config.sync_gitsigns_base then
        review.sync_visible_signs(neotree.git_base())
    end
end

--- Pick the window a file should open in: previous window, window active when the panel was opened, largest
--- regular window.
---@param panel integer
---@return integer|nil
function M.editor_win(panel)
    local candidates = {}
    local prev_nr = vim.fn.winnr("#")
    if prev_nr > 0 then
        table.insert(candidates, vim.fn.win_getid(prev_nr))
    end
    if S.editor_win then
        table.insert(candidates, S.editor_win)
    end
    for _, win in ipairs(candidates) do
        if is_editor_win(win, panel) then
            return win
        end
    end

    local best, best_area = nil, 0
    for _, win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
        if is_editor_win(win, panel) then
            local area = vim.api.nvim_win_get_width(win) * vim.api.nvim_win_get_height(win)
            if area > best_area then
                best, best_area = win, area
            end
        end
    end
    return best
end

--- <CR> in the panel: run zdiff's goto_file with an editor window focused so the file opens there, then land on
--- the first change for a header line and align gitsigns with the reviewed base.
---@param buf integer zdiff buffer
function M.goto_file(buf)
    local orig = S.orig[buf] and S.orig[buf].goto_file
    if not orig then
        return
    end
    local panel = vim.api.nvim_get_current_win()
    local lnum = vim.api.nvim_win_get_cursor(panel)[1]
    local header = is_file_header(vim.api.nvim_buf_get_lines(buf, lnum - 1, lnum, false)[1] or "")
    local ref = panel_ref(buf)

    local target = M.editor_win(panel)
    local created = false
    if target then
        vim.api.nvim_set_current_win(target)
    else
        vim.cmd("topleft new")
        target = vim.api.nvim_get_current_win()
        created = true
        -- a split made from the panel inherits its look; go back to the global defaults
        for _, opt in ipairs({ "number", "relativenumber", "signcolumn", "wrap", "cursorline" }) do
            vim.wo[target][opt] = vim.go[opt]
        end
    end

    local edited = with_edit_guard(orig)
    if not edited then
        -- nothing to open on this line (separator, deleted file or deleted line): zdiff has notified, stay put
        if created then
            vim.api.nvim_win_close(target, true)
        end
        if vim.api.nvim_win_is_valid(panel) then
            vim.api.nvim_set_current_win(panel)
        end
        return
    end

    local file_buf = vim.api.nvim_get_current_buf()
    if header and M.config.jump_to_first_change then
        review.jump_to_first_change(vim.api.nvim_get_current_win(), file_buf, ref)
    end
    if M.config.sync_gitsigns_base then
        review.sync_signs_base(file_buf, ref)
    end
end

--- Replace one of zdiff's buffer-local mappings with a wrapper built from the original callback. Idempotent.
---@param buf integer zdiff buffer
---@param name string zdiff keymap name (`goto_file`, `toggle_mode`, …)
---@param desc string
---@param make fun(orig: fun()): fun()
local function wrap_key(buf, name, desc, make)
    local lhs = require("zdiff").config.keymaps[name]
    local current = type(lhs) == "string" and buf_keymap_callback(buf, lhs) or nil
    if not current or current == S.wrapper[buf][name] then
        return
    end
    S.orig[buf][name] = current
    S.wrapper[buf][name] = make(current)
    vim.keymap.set("n", lhs, S.wrapper[buf][name], { buffer = buf, silent = true, desc = desc })
end

--- Wrap zdiff's <CR> and `m` on the panel buffer and keep the panel look when focus leaves it. Idempotent.
---@param buf integer zdiff buffer
local function attach(buf)
    S.orig[buf] = S.orig[buf] or {}
    S.wrapper[buf] = S.wrapper[buf] or {}
    wrap_key(buf, "goto_file", "Open file in editor window", function()
        return function()
            M.goto_file(buf)
        end
    end)
    if M.config.double_click_opens and S.wrapper[buf].goto_file then
        vim.keymap.set(
            "n",
            "<2-LeftMouse>",
            S.wrapper[buf].goto_file,
            { buffer = buf, silent = true, desc = "Open file in editor window" }
        )
    end
    wrap_key(buf, "toggle_mode", "Toggle uncommitted / branch mode", function(orig)
        return function()
            orig()
            -- toggle_mode() re-renders the mode line synchronously before its async refresh
            follow_mode(panel_ref(buf))
        end
    end)

    local group = vim.api.nvim_create_augroup(AUGROUP, { clear = false })
    vim.api.nvim_clear_autocmds({ group = group, buffer = buf })
    -- BufLeave (zdiff restores the window options there) runs before WinLeave, and only a window switch fires
    -- WinLeave: `:edit` inside the panel keeps zdiff's native behaviour
    vim.api.nvim_create_autocmd("WinLeave", {
        group = group,
        buffer = buf,
        desc = "zdiff panel: keep the diff look after zdiff restores the window options",
        callback = function()
            local win = vim.api.nvim_get_current_win()
            if win == S.panel_win then
                restyle(win, buf)
            end
        end,
    })
    vim.api.nvim_create_autocmd("BufWipeout", {
        group = group,
        buffer = buf,
        desc = "zdiff panel: forget the wrapped mappings, give gitsigns and Neo-tree their bases back",
        callback = function()
            S.orig[buf] = nil
            S.wrapper[buf] = nil
            review.reset_signs_bases()
            if not S.switching then
                -- the panel window is closing right now; let Neo-tree re-render once the layout has settled
                vim.schedule(release_tree)
            end
        end,
    })
end

--- Open zdiff in the bottom panel (creating it when needed) and focus it. <leader>zd, `:ZdiffPanel [ref]`.
---@param ref? string git ref to diff against; nil shows uncommitted changes
function M.open(ref)
    local cur = vim.api.nvim_get_current_win()
    local panel = is_zdiff_win(S.panel_win) and S.panel_win or nil
    if cur ~= panel and is_editor_win(cur, panel) then
        S.editor_win = cur
    end

    local created, parked = false, nil
    if panel then
        vim.api.nvim_set_current_win(panel)
        -- park a scratch buffer: zdiff wipes its buffer when the ref changes, and wiping the window's current
        -- buffer would close the panel window
        parked = vim.api.nvim_win_get_buf(panel)
        local scratch = vim.api.nvim_create_buf(false, true)
        vim.bo[scratch].bufhidden = "wipe"
        vim.api.nvim_win_set_buf(panel, scratch)
    else
        vim.cmd("botright split")
        panel = vim.api.nvim_get_current_win()
        vim.api.nvim_win_set_height(panel, panel_height())
        vim.wo[panel].winfixheight = true
        created = true
    end
    S.panel_win = panel

    -- a ref change wipes the old zdiff buffer: keep its BufWipeout from resetting Neo-tree in between
    S.switching = true
    local ok, err = pcall(require("zdiff").open, ref)
    S.switching = false
    if not ok then
        error(err, 0)
    end

    local buf = vim.api.nvim_win_get_buf(panel)
    if vim.bo[buf].filetype ~= "zdiff" then
        -- zdiff refused (not a git repository, unknown ref) and has notified; undo the panel change
        if created then
            vim.api.nvim_win_close(panel, true)
            S.panel_win = nil
        elseif parked and vim.api.nvim_buf_is_valid(parked) then
            vim.api.nvim_win_set_buf(panel, parked)
        end
        return
    end
    attach(buf)
    -- zdiff's `m` toggles between uncommitted and config.default_branch (the plugin ships "main"): keep it on the
    -- ref reviewed here, or on this repository's default branch when the panel was opened in uncommitted mode
    local zdiff = require("zdiff")
    zdiff.config.default_branch = ref or review.default_branch(vim.uv.cwd()) or zdiff.config.default_branch
    follow_mode(ref)
end

--- Open the panel against the repository's default branch (origin/HEAD, else main/master/…), see
--- git-review-util.default_branch(); warns when none is found.
function M.open_default_branch()
    local branch = review.default_branch(vim.uv.cwd())
    if not branch then
        vim.notify("zdiff: no default branch found (origin/HEAD, main, master, develop, trunk)", vim.log.levels.WARN)
        return
    end
    M.open(branch)
end

return M
