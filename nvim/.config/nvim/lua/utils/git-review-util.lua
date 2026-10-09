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
-- • reset_signs_bases — give every buffer touched by sync_signs_base its default base back
--
-- gitsigns notes: change_base() is per *current* buffer and silently does nothing for a buffer that is not
-- attached yet; attach() is throttled per buffer and a call made while the BufRead auto attach of a freshly
-- opened file is still running is dropped (not queued). sync_signs_base() therefore attaches (covers a buffer
-- gitsigns never picked up) and changes the base once gitsigns reports the buffer.

local M = {}

---@type table<integer, string> gitsigns base set per file buffer (absent = default)
local signs_base = {}

--- Drop a buffer's entry when the buffer is wiped: its number may be reused for another file.
---@param buf integer
local function forget_on_wipeout(buf)
    vim.api.nvim_create_autocmd("BufWipeout", {
        buffer = buf,
        once = true,
        callback = function()
            signs_base[buf] = nil
        end,
    })
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

--- Change the gitsigns base of a file buffer (per buffer, not global); errors only warn.
---@param gitsigns table the gitsigns module
---@param buf integer file buffer
---@param base string|nil nil = gitsigns' default base
local function change_base(gitsigns, buf, base)
    -- change_base() reads the current buffer synchronously before its first await
    vim.api.nvim_buf_call(buf, function()
        gitsigns.change_base(base, false, function(err)
            if err then
                vim.notify("[git review] gitsigns base: " .. tostring(err), vim.log.levels.WARN)
            end
        end)
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
        local dir = vim.fs.dirname(vim.api.nvim_buf_get_name(buf))
        if not M.ref_exists(dir, ref) then
            return
        end
        base = M.merge_base(dir, ref) or ref -- no merge-base (unrelated histories): the ref itself
    end
    if signs_base[buf] == base then
        return
    end
    gitsigns.attach({ bufnr = buf }, function() end)
    when_attached(gitsigns, buf, function()
        change_base(gitsigns, buf, base)
        if base and signs_base[buf] == nil then
            forget_on_wipeout(buf)
        end
        signs_base[buf] = base
    end)
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
    local ok, gitsigns = pcall(require, "gitsigns")
    for buf, _ in pairs(signs_base) do
        if ok and vim.api.nvim_buf_is_valid(buf) then
            change_base(gitsigns, buf, nil)
        end
    end
    signs_base = {}
end

return M
