--- Fast, project-scoped references for the Java method under the cursor (`gR`).
---
--- `gr` asks jdtls, which searches every project *and* dependency jar and, for an
--- overriding method, also reports every call made through the interface /
--- superclass (polymorphic search). This module answers a narrower, much faster
--- question: "where in *this* project (cwd) is `<Type>#<method>` called on a
--- receiver whose declared type is exactly `<Type>`?"
---
--- Pipeline:
--- 1. `resolver.resolve_target` names the `<Type>#<method>` under the cursor
---    (treesitter on a declaration; sync LSP definition on a call site).
--- 2. `Snacks.picker.grep` runs one ripgrep over cwd for `.method(` / `::method` /
---    bare `method(` (Java files only, gitignore respected).
--- 3. A picker `transform` parses each hit's file once with treesitter and keeps
---    only hits whose receiver resolves to `<Type>` (`resolver.classify`).
---
--- Picker keys: `<C-a>` toggles `strict` (resolved to the type) / `all` (every
--- textual call, labelled with the resolved type, `?` when unresolved). The
--- title shows how many hits strict mode is hiding.
---
--- Public API:
--- - `M.find(opts)`  open the picker for the cursor position; returns `true, picker`,
---                    or false when no `<Type>#<method>` could be determined (caller may fall back).
--- - `M.settings`    tweakable defaults (mode, file type, excludes, LSP timeout).

local resolver = require("modules.java.project-references.resolver")

local M = {}

M.settings = {
    -- "strict": only calls whose receiver resolves to the target type; "all": every textual call
    default_mode = "strict",
    -- ripgrep file type
    ft = "java",
    -- ripgrep globs to skip (build output / generated sources); gitignore is honoured anyway
    exclude = { "**/target/**", "**/build/generated/**" },
    -- sync `textDocument/definition` budget when the cursor is on a call site
    lsp_timeout_ms = 2000,
    -- list the target method's own declaration too (like `gr`)
    include_declaration = true,
}

-- picker labels: unresolved receiver, the target's own declaration, Mockito verify/when chains
local LABELS = { candidate = "?", declaration = "decl", mock = "mock" }

---@class java.ProjectRefs.State
---@field mode "strict"|"all"
---@field cache table<string, java.ProjectRefs.Entry|false> parsed files, per invocation
---@field counts table<java.ProjectRefs.Verdict, integer> verdicts seen by the last finder run
---@field done boolean finder finished (title can show hidden counts)
---@field reported_error? boolean a per-hit failure was already notified

--- Escape a literal for a ripgrep (Rust regex) pattern.
---@param s string
---@return string
local function escape_rg(s)
    return (s:gsub("[%^%$%(%)%%%.%[%]%*%+%-%?%{%}%|\\]", "\\%0"))
end

--- ripgrep pattern for every textual call shape of a method: `.m(`, `::m`, bare `m(`.
--- The bare form also matches the declaration; the treesitter pass sorts that out.
---@param method string
---@return string
local function rg_pattern(method)
    local m = escape_rg(method)
    return ("(\\.|::)\\s*%s\\b|(^|[^\\w.])%s\\s*\\("):format(m, m)
end

--- Picker title for the current mode, with the hidden-hit count once the finder is done.
---@param target java.ProjectRefs.Target
---@param state java.ProjectRefs.State
---@return string
local function make_title(target, state)
    local mode = state.mode == "strict" and "strict" or "all calls"
    local suffix = ""
    if state.done and state.mode == "strict" then
        local hidden = (state.counts.candidate or 0) + (state.counts.other or 0)
        if hidden > 0 then
            suffix = (" · %d hidden, <C-a> shows all"):format(hidden)
        end
    end
    return ("%s#%s · project references [%s%s]"):format(target.class, target.method, mode, suffix)
end

--- Run `fn` on the main loop and hand its result back to the finder coroutine.
---
--- Snacks' own `Async:schedule` cannot be used here: the grep finder shares this
--- coroutine and calls `resume()` on every stdout chunk, which wakes the single
--- `suspend()` inside `schedule` before the scheduled function ran (its `ret` is
--- still nil). So keep re-suspending until *our* result has arrived. A spurious
--- wake-up only costs a loop turn; the proc finder re-checks its own queue once
--- the transform returns, so no grep output is lost.
---@generic T
---@param async snacks.picker.Async the running finder coroutine
---@param fn fun(): T
---@return T
local function on_main_loop(async, fn)
    local ret ---@type { [1]: boolean, [2]: any }|nil
    vim.schedule(function()
        ret = { pcall(fn) }
        async:resume()
    end)
    while ret == nil do
        async:suspend()
    end
    if not ret[1] then
        error(ret[2])
    end
    return ret[2]
end

--- Parsed entry for a hit's file. Snacks runs finders inside a libuv check
--- callback (fast context), so the treesitter parse is hopped onto the main loop
--- through the finder's own async handle; one hop per file, not per hit.
---@param path string
---@param state java.ProjectRefs.State
---@return java.ProjectRefs.Entry|nil
local function entry_for(path, state)
    local entry = state.cache[path]
    if entry ~= nil then
        return entry or nil
    end
    local function load()
        local ok, loaded = pcall(resolver.load, path, state.cache)
        if not ok then
            state.cache[path] = false
            return nil
        end
        return loaded
    end
    local async = require("snacks.picker.util.async").running()
    if async then
        return on_main_loop(async, load)
    end
    return load()
end

--- Verdict + name column for one grep item, or nil when it is not a call of the target.
---@param item snacks.picker.finder.Item
---@param target java.ProjectRefs.Target
---@param state java.ProjectRefs.State
---@return java.ProjectRefs.Verdict|nil verdict
---@return string|nil info
---@return integer|nil name_col 0-indexed column of the method name
local function classify_item(item, target, state)
    local path = Snacks.picker.util.path(item)
    local entry = path and entry_for(path, state)
    if not entry then
        return nil
    end
    -- grep items carry `file:line:col:text`; find the method name at/after the match start
    local line_text = item.text:match("^.-:%d+:%d+:(.*)$")
    local name_col = line_text and line_text:find(target.method, item.pos[2] + 1, true)
    if not name_col then
        return nil
    end
    local verdict, info = resolver.classify(entry, item.pos[1] - 1, name_col - 1, target)
    return verdict, info, name_col - 1
end

--- Build the per-hit transform: verify the hit with treesitter, drop or label it,
--- and move its position onto the method name. A failure on one hit drops that
--- hit (reported once) instead of killing the whole finder.
---@param target java.ProjectRefs.Target
---@param state java.ProjectRefs.State
---@return snacks.picker.transform
local function make_transform(target, state)
    return function(item)
        local ok, verdict, info, name_col = pcall(classify_item, item, target, state)
        if not ok then
            local err = verdict
            -- the finder abort is signalled through this same error path; let it through
            if tostring(err):match("aborted") then
                error(err, 0)
            end
            if not state.reported_error then
                state.reported_error = true
                vim.schedule(function()
                    vim.notify("[Project references] hit skipped: " .. tostring(err), vim.log.levels.WARN)
                end)
            end
            return false
        end
        if not verdict or verdict == "drop" then
            return false
        end
        if info == "declaration" and not M.settings.include_declaration then
            return false
        end
        state.counts[verdict] = (state.counts[verdict] or 0) + 1
        if state.mode == "strict" and verdict ~= "match" then
            return false
        end

        item.pos = { item.pos[1], name_col }
        if verdict == "candidate" then
            item.label = LABELS.candidate
        elseif verdict == "other" then
            item.label = info or LABELS.candidate
        elseif info then
            item.label = LABELS[info] or info
        end
    end
end

--- Refresh the picker title once the current finder run has finished.
---@param picker snacks.Picker
---@param target java.ProjectRefs.Target
---@param state java.ProjectRefs.State
local function title_when_done(picker, target, state)
    local function finished()
        if picker.closed then
            return
        end
        state.done = true
        picker.title = make_title(target, state)
        picker:update_titles()
    end
    local task = picker.finder and picker.finder.task
    if task and task:running() then
        task:on("done", vim.schedule_wrap(finished))
    else
        finished()
    end
end

--- `<C-a>`: flip strict / all and re-run the (cheap) search.
---@param picker snacks.Picker
---@param target java.ProjectRefs.Target
---@param state java.ProjectRefs.State
local function toggle_mode(picker, target, state)
    state.mode = state.mode == "strict" and "all" or "strict"
    state.counts = {}
    state.done = false
    picker.title = make_title(target, state)
    picker:update_titles()
    picker:find({ refresh = true })
    title_when_done(picker, target, state)
end

--- Open the project-references picker for the method at the cursor.
---@param opts? { row?: integer, col?: integer } 0-indexed position; defaults to the cursor
---@return boolean handled false when no `<Type>#<method>` could be determined
---@return snacks.Picker|nil picker the opened picker (tests / callers that want to drive it)
function M.find(opts)
    opts = opts or {}
    local bufnr = vim.api.nvim_get_current_buf()
    local cursor = vim.api.nvim_win_get_cursor(0)
    local row = opts.row or (cursor[1] - 1)
    local col = opts.col or cursor[2]

    ---@type java.ProjectRefs.State
    local state = { mode = M.settings.default_mode, cache = {}, counts = {}, done = false }

    local target, err = resolver.resolve_target(bufnr, row, col, {
        cache = state.cache,
        lsp_timeout_ms = M.settings.lsp_timeout_ms,
    })
    if not target then
        vim.notify("[Project references] " .. (err or "unsupported symbol"), vim.log.levels.INFO)
        return false
    end

    local picker = Snacks.picker.grep({
        title = make_title(target, state),
        search = rg_pattern(target.method),
        regex = true,
        live = false,
        supports_live = false,
        ft = M.settings.ft,
        exclude = M.settings.exclude,
        args = { "--case-sensitive" },
        transform = make_transform(target, state),
        on_show = function(picker)
            title_when_done(picker, target, state)
        end,
        win = {
            input = {
                keys = {
                    ["<c-a>"] = { "toggle_mode", mode = { "n", "i" }, desc = "Toggle strict / all calls" },
                },
            },
        },
        actions = {
            toggle_mode = function(p)
                toggle_mode(p, target, state)
            end,
        },
    })
    return true, picker
end

return M
