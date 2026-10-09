-- zdiff.nvim as a bottom review panel.
--
-- zdiff.nvim has no layout options: open() takes over the current window and its goto_file action (<CR>) runs
-- `:edit` in that same window, so reviewing means bouncing between the diff and the file inside one window.
--
-- • open — show the diff in a dedicated bottom split (reuses the panel when it is already open) and focus it
-- • goto_file — <CR> in the panel: open the file in an editor window and focus it; the panel stays as it is.
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
-- • zdiff diffs `<ref>...HEAD` in ref mode and the working tree vs HEAD in uncommitted mode. The first change is
--   taken from `git diff -U0` with that same target. gitsigns' base is changed per buffer to merge-base(ref, HEAD)
--   in ref mode and left at its default (index, staged hunks have their own signs) in uncommitted mode; buffers
--   whose base was changed are reverted when the zdiff buffer goes away (panel closed or ref switched). The
--   change waits until gitsigns has attached the buffer, see sync_signs_base()
-- • Neo-tree follows the panel the way `:Neotree <ref>` does (utils/neotree-util set_git_base / reset_git_base):
--   the trees of this tab compare against the panel's ref while it is open, in ref mode only, and get their
--   previous base back when the panel closes. Updated on open, on the in-place `m` toggle and on close

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
}

---@class ZdiffPanelState
---@field panel_win integer|nil window holding the zdiff buffer
---@field editor_win integer|nil window that was active when the panel was opened
---@field orig table<integer, table<string, fun()>> zdiff's own callbacks per zdiff buffer, by keymap name
---@field wrapper table<integer, table<string, fun()>> the panel-aware callbacks per zdiff buffer, by keymap name
---@field signs_base table<integer, string> gitsigns base this module set per file buffer (absent = default)
---@field switching boolean true while open() replaces the zdiff buffer for another ref
local S = { panel_win = nil, editor_win = nil, orig = {}, wrapper = {}, signs_base = {}, switching = false }

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

--- Run git in the directory of file and return its stdout lines, or nil on failure.
---@param file string
---@param args string[]
---@return string[]|nil
local function git_lines(file, args)
    local cmd = { "git", "-C", vim.fs.dirname(file) }
    vim.list_extend(cmd, args)
    local out = vim.fn.systemlist(cmd)
    if vim.v.shell_error ~= 0 then
        return nil
    end
    return out
end

--- First changed line of file in the diff zdiff shows: `<ref>...HEAD` in ref mode, working tree vs HEAD otherwise.
---@param file string absolute path
---@param ref string|nil
---@return integer|nil
local function first_changed_line(file, ref)
    local target = ref and (ref .. "...HEAD") or "HEAD"
    local out = git_lines(file, { "diff", "-U0", target, "--", file })
    for _, line in ipairs(out or {}) do
        local start = line:match("^@@ %-%d+,?%d* %+(%d+)")
        if start then
            return math.max(1, tonumber(start))
        end
    end
    return nil
end

--- Call fn once gitsigns is attached to buf (get_hunks() is nil until then); gives up after about five seconds
--- or when the buffer is gone.
---@param gitsigns table the gitsigns module
---@param buf integer file buffer
---@param fn fun()
---@param attempt? integer
local function when_attached(gitsigns, buf, fn, attempt)
    if not vim.api.nvim_buf_is_valid(buf) then
        return
    end
    if gitsigns.get_hunks(buf) ~= nil then
        fn()
        return
    end
    attempt = attempt or 0
    if attempt >= 100 then
        return
    end
    vim.defer_fn(function()
        when_attached(gitsigns, buf, fn, attempt + 1)
    end, 50)
end

--- Change the gitsigns base of a file buffer (per buffer, not global); errors only warn.
---@param gitsigns table the gitsigns module
---@param buf integer file buffer
---@param base string|nil nil = gitsigns' default base
local function change_base(gitsigns, buf, base)
    -- change_base() reads the current buffer synchronously before its first await
    vim.api.nvim_buf_call(buf, function()
        gitsigns.change_base(base, false, function(err)
            if err then
                vim.notify("[zdiff panel] gitsigns base: " .. tostring(err), vim.log.levels.WARN)
            end
        end)
    end)
end

--- Point gitsigns at the base zdiff compares against for a file buffer: merge-base(ref, HEAD) in ref mode, its
--- default otherwise. No-op when gitsigns is absent or the buffer already has that base.
---@param buf integer file buffer
---@param ref string|nil
local function sync_signs_base(buf, ref)
    local ok, gitsigns = pcall(require, "gitsigns")
    if not ok then
        return
    end
    local base = nil
    if ref then
        local out = git_lines(vim.api.nvim_buf_get_name(buf), { "merge-base", ref, "HEAD" })
        base = out and out[1] ~= "" and out[1] or ref
    end
    if S.signs_base[buf] == base then
        return
    end
    -- gitsigns throttles attach() per buffer: a call made while its own BufRead attach of a freshly opened file is
    -- still running is dropped (not queued), and change_base() silently does nothing for a buffer that is not
    -- attached yet. attach() here covers a buffer gitsigns never picked up; the base changes once it is attached.
    gitsigns.attach({ bufnr = buf }, function() end)
    when_attached(gitsigns, buf, function()
        change_base(gitsigns, buf, base)
        S.signs_base[buf] = base
    end)
end

--- Revert every buffer whose gitsigns base this module changed back to the default base.
local function reset_signs_bases()
    local ok, gitsigns = pcall(require, "gitsigns")
    for buf, _ in pairs(S.signs_base) do
        if ok and vim.api.nvim_buf_is_valid(buf) then
            change_base(gitsigns, buf, nil)
        end
    end
    S.signs_base = {}
end

--- Make the Neo-tree trees compare against the panel's ref (nil = uncommitted mode = Neo-tree's own base).
---@param ref string|nil
local function sync_tree(ref)
    if not M.config.sync_neotree_base then
        return
    end
    local ok, neotree = pcall(require, "utils.neotree-util")
    if not ok then
        return
    end
    if ref then
        neotree.set_git_base(ref)
    else
        neotree.reset_git_base()
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
    local file = vim.api.nvim_buf_get_name(file_buf)
    if header and M.config.jump_to_first_change then
        local first = first_changed_line(file, ref)
        if first then
            vim.api.nvim_win_set_cursor(0, { math.min(first, vim.api.nvim_buf_line_count(file_buf)), 0 })
            vim.cmd("normal! zz")
        end
    end
    if M.config.sync_gitsigns_base then
        sync_signs_base(file_buf, ref)
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
    wrap_key(buf, "toggle_mode", "Toggle uncommitted / branch mode", function(orig)
        return function()
            orig()
            -- toggle_mode() re-renders the mode line synchronously before its async refresh
            sync_tree(panel_ref(buf))
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
            reset_signs_bases()
            if not S.switching then
                -- the panel window is closing right now; let Neo-tree re-render once the layout has settled
                vim.schedule(function()
                    sync_tree(nil)
                end)
            end
        end,
    })
end

--- Open zdiff in the bottom panel (creating it when needed) and focus it.
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
    sync_tree(ref)
end

return M
