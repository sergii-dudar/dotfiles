-- Helpers shared by the review views (zdiff panel, Neo-tree changes view) for a file opened from them.
--
-- Both views compare either the working tree against HEAD ("uncommitted", ref = nil) or the branch against a ref
-- the way zdiff does (`<ref>...HEAD`, i.e. from the merge-base), and want the opened file to match that view:
--
-- • default_branch / ref_exists / complete_refs — the branch to review against (origin/HEAD, then
--   main/master/develop/trunk), ref validation, command-line completion of branches and tags
-- • first_changed_line / jump_to_first_change — first hunk of the file in that diff (`git diff -U0`, sync)
-- • sync_signs_base — point gitsigns at the same base for the buffer: merge-base(ref, HEAD) in ref mode, its
--   default (index; staged hunks have their own signs) in uncommitted mode. Per buffer, never global
-- • sync_visible_signs — the same for every file buffer shown in the current tab (used when a view changes mode,
--   so the file already on screen follows without being reopened)
-- • set_active_base / active_ref — while a view is in ref mode, every buffer of that repository loaded or
--   entered later (picker, gf, LSP jump, `:e`) gets the base as well
-- • reset_signs_bases — give every buffer touched by sync_signs_base its default base back
--
-- gitsigns notes: change_base() is per *current* buffer and silently does nothing for a buffer that is not
-- attached yet; attach() is throttled per buffer and a call made while the BufRead auto attach of a freshly
-- opened file is still running is dropped (not queued). sync_signs_base() therefore attaches (covers a buffer
-- gitsigns never picked up) and changes the base once gitsigns reports the buffer.

local M = {}

---@type table<integer, string> gitsigns base set per file buffer (absent = default)
local signs_base = {}

---@type table<integer, true> buffers that already have the unload hook below
local unload_hooked = {}

--- Bumped whenever the review changes mode or ends: work queued for an earlier mode (a buffer still attaching,
--- a verification timer) checks it and gives up instead of applying a stale base.
local generation = 0

--- Drop a buffer's entry when the buffer is unloaded (`:bdelete` keeps the number, gitsigns detaches and
--- re-attaches with its default base on the next load) or wiped (the number may be reused for another file).
--- One hook per buffer lifetime.
---@param buf integer
local function forget_on_unload(buf)
    if unload_hooked[buf] then
        return
    end
    unload_hooked[buf] = true
    local ids = {}
    local function forget()
        signs_base[buf] = nil
        unload_hooked[buf] = nil
        -- `once` would only remove the entry that fired; drop both so no entry lingers until a later wipeout
        for _, id in ipairs(ids) do
            pcall(vim.api.nvim_del_autocmd, id)
        end
    end
    for _, event in ipairs({ "BufUnload", "BufWipeout" }) do
        ids[#ids + 1] = vim.api.nvim_create_autocmd(event, {
            buffer = buf,
            desc = "git review: forget this buffer's gitsigns base",
            callback = forget,
        })
    end
end

--- Run git in dir and return its stdout lines, or nil on failure.
---@param dir string
---@param args string[]
---@return string[]|nil
local function git_in(dir, args)
    local cmd = { "git", "-C", dir }
    vim.list_extend(cmd, args)
    local out = vim.fn.systemlist(cmd)
    if vim.v.shell_error ~= 0 then
        return nil
    end
    return out
end

--- Run git in the directory of file and return its stdout lines, or nil on failure.
---@param file string
---@param args string[]
---@return string[]|nil
local function git_lines(file, args)
    return git_in(vim.fs.dirname(file), args)
end

--- Check that ref names an existing commit in the repository containing dir.
---@param dir string
---@param ref string
---@return boolean
function M.ref_exists(dir, ref)
    return git_in(dir, { "rev-parse", "--verify", "--quiet", ref .. "^{commit}" }) ~= nil
end

--- Branch to review against in the repository containing dir: the branch origin/HEAD points to (its local
--- branch when there is one, the remote ref otherwise), else the first of main, master, develop, trunk that
--- exists; nil when none does.
---@param dir? string defaults to the cwd
---@return string|nil
function M.default_branch(dir)
    dir = dir or vim.uv.cwd()
    local out = git_in(dir, { "symbolic-ref", "--short", "refs/remotes/origin/HEAD" })
    local remote = out and out[1] ~= "" and out[1] or nil
    if remote then
        local name = remote:match("^[^/]+/(.+)$")
        if name and M.ref_exists(dir, name) then
            return name
        end
        return remote
    end
    for _, name in ipairs({ "main", "master", "develop", "trunk" }) do
        if M.ref_exists(dir, name) then
            return name
        end
    end
    return nil
end

--- Fork point of ref and HEAD (the commit zdiff's `<ref>...HEAD` starts from); nil without a common ancestor or
--- for an unknown ref.
---@param dir string
---@param ref string
---@return string|nil commit
function M.merge_base(dir, ref)
    local out = git_in(dir, { "merge-base", ref, "HEAD" })
    return out and out[1] ~= "" and out[1] or nil
end

--- Command-line completion for a git ref: local branches, remote branches and tags of the cwd repository that
--- start with arglead.
---@param arglead string
---@return string[]
function M.complete_refs(arglead)
    local out =
        git_in(vim.uv.cwd(), { "for-each-ref", "--format=%(refname:short)", "refs/heads", "refs/remotes", "refs/tags" })
    return vim.tbl_filter(function(ref)
        return ref ~= "" and vim.startswith(ref, arglead)
    end, out or {})
end

--- First changed line of file: `<ref>...HEAD` in ref mode, working tree vs HEAD otherwise.
---@param file string absolute path
---@param ref string|nil
---@return integer|nil
function M.first_changed_line(file, ref)
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

--- Move the cursor of win (showing buf) to the file's first change and center it; no-op without changes.
---@param win integer
---@param buf integer file buffer shown in win
---@param ref string|nil
---@return integer|nil line the cursor was moved to
function M.jump_to_first_change(win, buf, ref)
    if not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= buf then
        return nil
    end
    local first = M.first_changed_line(vim.api.nvim_buf_get_name(buf), ref)
    if not first then
        return nil
    end
    local line = math.min(first, vim.api.nvim_buf_line_count(buf))
    vim.api.nvim_win_set_cursor(win, { line, 0 })
    vim.api.nvim_win_call(win, function()
        vim.cmd("normal! zz")
    end)
    return line
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

---@type table<integer, string|false> base requested last per buffer while an earlier change is still running
local wanted_base = {}
---@type table<integer, true> buffers with a gitsigns change_base() in flight
local base_in_flight = {}

--- Change the gitsigns base of a file buffer (per buffer, not global); errors only warn. Changes for one buffer
--- run one after another so the last request wins: gitsigns runs each change_base() as its own asynchronous
--- task, and two of them racing (reset to default, then a new base) could otherwise end on the older one.
---@param gitsigns table the gitsigns module
---@param buf integer file buffer
---@param base string|nil nil = gitsigns' default base
local function change_base(gitsigns, buf, base)
    if base == nil then
        wanted_base[buf] = false -- `x and false or y` would turn this into nil
    else
        wanted_base[buf] = base
    end
    if base_in_flight[buf] then
        return
    end
    local function run()
        local want = wanted_base[buf]
        wanted_base[buf] = nil
        if want == nil or not vim.api.nvim_buf_is_valid(buf) then
            base_in_flight[buf] = nil
            return
        end
        base_in_flight[buf] = true
        -- change_base() reads the current buffer synchronously before its first await
        vim.api.nvim_buf_call(buf, function()
            gitsigns.change_base(want or nil, false, function(err)
                if err then
                    vim.notify("[git review] gitsigns base: " .. tostring(err), vim.log.levels.WARN)
                end
                base_in_flight[buf] = nil
                if wanted_base[buf] ~= nil then
                    run() -- a newer request arrived meanwhile
                end
            end)
        end)
    end
    run()
end

---@class GitReviewActiveBase
---@field root string worktree root the review runs in
---@field ref string ref being reviewed
---@field base string commit gitsigns compares against (fork point of ref and HEAD)

--- The review in progress (a view is in ref mode), so that buffers loaded while it lasts get its base too.
---@type GitReviewActiveBase|nil
local active_base = nil

local apply_signs_base

--- gitsigns tears an attachment down from the asynchronous detach callback of the buffer's previous load, and
--- under load (jdtls starting, big repository) that callback can land after the new load was attached: the
--- buffer then shows no signs at all. Check once the dust has settled and attach again when that happened.
---@param gitsigns table the gitsigns module
---@param buf integer file buffer
---@param base string|nil
---@param retries integer
local function verify_attached(gitsigns, buf, base, retries)
    local gen = generation
    vim.defer_fn(function()
        if gen ~= generation or not vim.api.nvim_buf_is_loaded(buf) then
            return
        end
        if signs_base[buf] ~= base or gitsigns.get_hunks(buf) ~= nil then
            return
        end
        signs_base[buf] = nil
        apply_signs_base(gitsigns, buf, base, retries - 1)
    end, 1500)
end

--- Give a file buffer the gitsigns base (a commit, nil = default) once gitsigns has attached it; no-op when it
--- already has that base, or when asked for the default on a buffer that was never changed.
---@param gitsigns table the gitsigns module
---@param buf integer file buffer
---@param base string|nil
---@param retries? integer re-attach attempts left when gitsigns drops the attachment afterwards
apply_signs_base = function(gitsigns, buf, base, retries)
    if base == nil and signs_base[buf] == nil then
        return
    end
    -- the record alone is not enough: a buffer that was unloaded and loaded again is attached afresh by gitsigns
    -- with its default base, so only an attached buffer can be trusted to still carry the recorded one
    if signs_base[buf] == base and gitsigns.get_hunks(buf) ~= nil then
        return
    end
    retries = retries or 2
    local gen = generation
    gitsigns.attach({ bufnr = buf }, function() end)
    when_attached(gitsigns, buf, function()
        if gen ~= generation then
            return -- the review changed mode or ended while this buffer was attaching
        end
        change_base(gitsigns, buf, base)
        if base then
            forget_on_unload(buf)
        end
        signs_base[buf] = base
        if base and retries > 0 then
            verify_attached(gitsigns, buf, base, retries)
        end
    end)
end

--- Point gitsigns at the reviewed base for a file buffer: merge-base(ref, HEAD) in ref mode, its default
--- otherwise. No-op when gitsigns is absent, the ref does not exist, or the buffer already has that base.
---@param buf integer file buffer
---@param ref string|nil
function M.sync_signs_base(buf, ref)
    local ok, gitsigns = pcall(require, "gitsigns")
    if not ok then
        return
    end
    local base = nil
    if ref then
        local name = vim.api.nvim_buf_get_name(buf)
        if active_base and active_base.ref == ref and vim.startswith(name, active_base.root .. "/") then
            base = active_base.base -- already resolved for this review, no git calls
        else
            local dir = vim.fs.dirname(name)
            if not M.ref_exists(dir, ref) then
                return
            end
            base = M.merge_base(dir, ref) or ref -- no merge-base (unrelated histories): the ref itself
        end
    end
    apply_signs_base(gitsigns, buf, base)
end

local AUGROUP = "git_review_active_base"

--- Give a freshly loaded buffer of the reviewed repository the active base.
---@param buf integer
local function follow_active_base(buf)
    if not active_base or vim.bo[buf].buftype ~= "" then
        return
    end
    local name = vim.api.nvim_buf_get_name(buf)
    if name == "" or not vim.startswith(name, active_base.root .. "/") then
        return
    end
    local ok, gitsigns = pcall(require, "gitsigns")
    if ok then
        apply_signs_base(gitsigns, buf, active_base.base)
    end
end

--- Declare the review in progress for the repository containing dir (nil ends it): every buffer of that
--- repository loaded or entered from now on gets the base, like the ones opened from the views. Does not touch
--- buffers already open (the views sync those) and does not reset anything on nil (the views do).
---@param dir string|nil
---@param ref string|nil
function M.set_active_base(dir, ref)
    generation = generation + 1
    active_base = nil
    local group = vim.api.nvim_create_augroup(AUGROUP, { clear = true }) -- no idle autocmd outside a review
    if not dir or not ref then
        return
    end
    local out = git_in(dir, { "rev-parse", "--show-toplevel" })
    local root = out and out[1] ~= "" and out[1] or nil
    if not root or not M.ref_exists(root, ref) then
        return
    end
    active_base = { root = root, ref = ref, base = M.merge_base(root, ref) or ref }
    vim.api.nvim_create_autocmd({ "BufReadPost", "BufEnter" }, {
        group = group,
        desc = "git review: buffers of the reviewed repository follow the active gitsigns base",
        callback = function(args)
            follow_active_base(args.buf)
        end,
    })
end

--- The ref under review, when a view is in ref mode.
---@return string|nil
function M.active_ref()
    return active_base and active_base.ref or nil
end

--- sync_signs_base() for every regular file buffer shown in a window of the current tab.
---@param ref string|nil
function M.sync_visible_signs(ref)
    for _, win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
        local buf = vim.api.nvim_win_get_buf(win)
        if vim.bo[buf].buftype == "" and vim.api.nvim_buf_get_name(buf) ~= "" then
            M.sync_signs_base(buf, ref)
        end
    end
end

--- Revert every buffer whose gitsigns base sync_signs_base() changed back to the default base.
function M.reset_signs_bases()
    generation = generation + 1
    local ok, gitsigns = pcall(require, "gitsigns")
    for buf, _ in pairs(signs_base) do
        if ok and vim.api.nvim_buf_is_valid(buf) then
            change_base(gitsigns, buf, nil)
        end
    end
    signs_base = {}
end

return M
