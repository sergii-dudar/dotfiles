-- Neo-tree helpers: context-aware explorer routing and cross-instance shared clipboard.
--
-- • toggle_context_explorer — route special buffers to their explorer, otherwise reveal them in the
--   Neo-tree source already on screen (file tree, git changes, …)
-- • open_explorer — open a Neo-tree source in the sidebar, revealing the current file
-- • toggle_git_explorer — open_explorer bound to the git_status source
-- • git_base / set_git_base — read or set the git ref the trees of this tab compare against (what `:Neotree <ref>`
--   does) without opening a closed sidebar
-- • remember_git_base / restore_git_base — snapshot the bases and put them back (used by the zdiff panel)
-- • toggle_git_base — <leader>gE: Git explorer flipping between uncommitted changes and changes vs the default
--   branch (origin/HEAD, else main/master/…); gitsigns of the files on screen follow
-- • on_file_opened — `file_opened` handler: a file opened from the Git explorer lands on its first change and
--   gets gitsigns' base aligned with the explorer (utils/git-review-util); other sources untouched
-- • copy_to_shared_clipboard — copy file/dir to shared clipboard
-- • paste_from_shared_clipboard — paste from shared clipboard into neo-tree target
-- • shared_copy / shared_copy_visual — copy current/selected buffer lines to clipboard file
-- • shared_paste — paste from clipboard file into current buffer

local M = {}

---@class NeoTreeExplorerContext
---@field bufnr integer
---@field filetype string
---@field name string

---@class NeoTreeExplorerRoute
---@field matches fun(context: NeoTreeExplorerContext): boolean
---@field toggle fun(context: NeoTreeExplorerContext)

---@type NeoTreeExplorerRoute[]
local explorer_routes = {
    {
        --- Check whether the current buffer is a Java dependency opened by JDTLS.
        ---@param context NeoTreeExplorerContext
        ---@return boolean
        matches = function(context)
            return context.filetype == "java" and vim.startswith(context.name, "jdt://")
        end,
        --- Open the Java dependency outline, recreating it when already open.
        toggle = function()
            local java_deps = require("java-deps")
            java_deps.toggle_outline()
            java_deps.open_outline()
        end,
    },
}

local clipboard_dir = vim.fn.stdpath("data") .. "/neo-tree-clipboard"

local function ensure_clipboard_dir()
    vim.fn.mkdir(clipboard_dir, "p")
end

local function clear_clipboard_dir()
    if vim.fn.isdirectory(clipboard_dir) == 1 then
        vim.fn.delete(clipboard_dir, "rf")
    end
    vim.fn.mkdir(clipboard_dir, "p")
end

local function get_folder_for_node(node)
    if node.type == "directory" then
        return node:get_id()
    end
    return vim.fn.fnamemodify(node:get_id(), ":h")
end

--- Return the source of the Neo-tree window currently on screen, if any.
---@return string|nil
local function visible_explorer_source()
    for _, winid in ipairs(vim.api.nvim_list_wins()) do
        local bufnr = vim.api.nvim_win_get_buf(winid)
        local ok, source = pcall(vim.api.nvim_buf_get_var, bufnr, "neo_tree_source")
        if ok and source then
            return source
        end
    end
    return nil
end

--- Toggle the explorer registered for the current buffer, or reveal it in Neo-tree.
function M.toggle_context_explorer()
    local bufnr = vim.api.nvim_get_current_buf()
    local context = {
        bufnr = bufnr,
        filetype = vim.api.nvim_get_option_value("filetype", { buf = bufnr }),
        name = vim.api.nvim_buf_get_name(bufnr),
    }

    for _, route in ipairs(explorer_routes) do
        if route.matches(context) then
            route.toggle(context)
            return
        end
    end

    -- Follow the tree already on screen, so this reveals inside the git changes view while it is
    -- open instead of swapping it back to the file tree. Plain filesystem keeps the original
    -- `Neotree reveal show`, whose out-of-cwd prompt is the way to jump the tree to another repo.
    local source = visible_explorer_source()
    if source and source ~= "filesystem" then
        M.open_explorer({ source = source, action = "show", toggle = false })
        return
    end

    vim.cmd("Neotree reveal show")
end

--- Resolve the path to reveal for the current buffer, or nil when it cannot be revealed safely.
--- Neo-tree prompts to change the cwd when the revealed file lives outside of it, and virtual
--- buffers (jdt://, term://, …) are not files at all, so both cases skip the reveal instead.
---@return string|nil
local function resolve_reveal_file()
    local path = require("neo-tree.sources.manager").get_path_to_reveal()
    if not path or vim.fn.filereadable(path) == 0 then
        return nil
    end
    if not require("neo-tree.utils").is_subpath(vim.uv.cwd(), path) then
        return nil
    end
    return path
end

--- Open a Neo-tree source in the sidebar, revealing the current file when it is part of the cwd.
--- With `action = "show"` the cursor stays in the current file and a repeated call re-reveals it
--- (the reveal path forces a re-navigate), so leave `toggle` off for that variant.
---@param opts { source: string?, position: string?, action: string?, toggle: boolean? }?
---       defaults: "filesystem" / "left" / "focus" / true
function M.open_explorer(opts)
    opts = opts or {}
    local reveal_file = resolve_reveal_file()

    require("neo-tree.command").execute({
        source = opts.source or "filesystem",
        action = opts.action or "focus",
        position = opts.position or "left",
        toggle = opts.toggle ~= false,
        reveal = reveal_file ~= nil,
        reveal_file = reveal_file,
    })
end

--- Open the Neo-tree git_status view; see `open_explorer` for the accepted options.
---@param opts { position: string?, action: string?, toggle: boolean? }?
function M.toggle_git_explorer(opts)
    return M.open_explorer(vim.tbl_extend("keep", opts or {}, { source = "git_status" }))
end

--- Snapshot taken by remember_git_base(): the base each Neo-tree state had (false = Neo-tree's default).
---@type table<table, string|false>|nil
local remembered_git_base = nil

--- Existing Neo-tree states of the current tab, each with its worktree root (no state is created here).
---@return {state: table, root: string}[]
local function tab_states()
    local tabid = vim.api.nvim_get_current_tabpage()
    local git = require("neo-tree.git")
    local states = {}
    for _, state in ipairs(require("neo-tree.sources.manager")._get_all_states()) do
        if state.tabid == tabid then
            -- a state that has not been navigated yet will open on the cwd
            local root = git.find_worktree_info(state.path or vim.uv.cwd())
            if root then
                table.insert(states, { state = state, root = root })
            end
        end
    end
    return states
end

--- Drop neo-tree's cached `git status` text for the given worktree roots. git.status() returns its cached result
--- whenever that text is unchanged and then skips the diff against the base (git/init.lua,
--- `raw_status_text_cache`), so a base change alone would leave the Git explorer's list stale. The cache is a
--- local upvalue; if the lookup fails after an upstream rename, the trees still refresh and only the git_status
--- list may lag until the working tree changes.
---@param roots table<string, true>
local function invalidate_status_cache(roots)
    local status = require("neo-tree.git").status
    local i = 1
    while true do
        local name, value = debug.getupvalue(status, i)
        if not name then
            return
        end
        if name == "raw_status_text_cache" and type(value) == "table" then
            for root in pairs(roots) do
                value[root] = nil
            end
            return
        end
        i = i + 1
    end
end

--- Re-render the visible trees of this tab (recomputing git status for the given worktree roots) and mark the
--- closed ones dirty so they re-navigate when shown.
---@param roots table<string, true>
local function refresh_trees(roots)
    invalidate_status_cache(roots)
    require("neo-tree.sources.manager").refresh("git_base")
end

--- Git ref the trees of this tab compare against; nil = Neo-tree's default (plain `git status`).
---@return string|nil
function M.git_base()
    if not package.loaded["neo-tree"] then
        return nil
    end
    for _, entry in ipairs(tab_states()) do
        local lookup = entry.state.git_base_by_worktree
        if lookup and lookup[entry.root] then
            return lookup[entry.root]
        end
    end
    return nil
end

--- Compare the Neo-tree sources of this tab against a git ref, like `:Neotree <ref>` does (`git diff <ref> HEAD`
--- status merged into the tree markers), without opening a closed sidebar. nil goes back to Neo-tree's default.
--- No-op while Neo-tree is not loaded.
---@param ref string|nil
function M.set_git_base(ref)
    if not package.loaded["neo-tree"] then
        return
    end
    local changed = {}
    for _, entry in ipairs(tab_states()) do
        local state = entry.state
        state.git_base_by_worktree = state.git_base_by_worktree or {}
        if state.git_base_by_worktree[entry.root] ~= ref then
            state.git_base_by_worktree[entry.root] = ref
            changed[entry.root] = true
        end
    end
    if next(changed) then
        refresh_trees(changed)
    end
end

--- Remember the current base of every tree so restore_git_base() can put it back; a snapshot already held is
--- kept (the first caller owns it).
function M.remember_git_base()
    if remembered_git_base or not package.loaded["neo-tree"] then
        return
    end
    remembered_git_base = setmetatable({}, { __mode = "k" })
    for _, entry in ipairs(tab_states()) do
        local lookup = entry.state.git_base_by_worktree
        remembered_git_base[entry.state] = lookup and lookup[entry.root] or false
    end
end

--- Put back the bases remembered by remember_git_base() and drop the snapshot.
function M.restore_git_base()
    local snapshot = remembered_git_base
    remembered_git_base = nil
    if not snapshot or not package.loaded["neo-tree"] then
        return
    end
    local changed = {}
    for _, entry in ipairs(tab_states()) do
        local previous = snapshot[entry.state]
        if previous ~= nil then
            local lookup = entry.state.git_base_by_worktree or {}
            entry.state.git_base_by_worktree = lookup
            local want = previous or nil
            if lookup[entry.root] ~= want then
                lookup[entry.root] = want
                changed[entry.root] = true
            end
        end
    end
    if next(changed) then
        refresh_trees(changed)
    end
end

--- Source of the Neo-tree window in the current tab, if any (other tabs may show trees of their own).
---@return string|nil
local function tab_explorer_source()
    for _, winid in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
        local ok, source = pcall(vim.api.nvim_buf_get_var, vim.api.nvim_win_get_buf(winid), "neo_tree_source")
        if ok and source then
            return source
        end
    end
    return nil
end

--- Flip the trees between uncommitted changes and changes against ref (the repository's default branch when
--- nil, see git-review-util.default_branch), showing the Git explorer when it is not on screen. The file
--- buffers on screen get gitsigns' base aligned right away (bound to <leader>gE).
---@param ref? string
function M.toggle_git_base(ref)
    require("neo-tree")
    local review = require("utils.git-review-util")
    local dir = vim.uv.cwd()
    ref = ref or review.default_branch(dir)
    if not ref then
        vim.notify("Neo-tree: no default branch found (origin/HEAD, main, master, develop, trunk)", vim.log.levels.WARN)
        return
    end
    if not review.ref_exists(dir, ref) then
        vim.notify("Neo-tree: unknown git ref " .. ref, vim.log.levels.WARN)
        return
    end
    local base = M.git_base() ~= ref and ref or nil
    -- the git_status source ignores a refresh while its first render is loading: make sure its state exists and
    -- carries the base before the explorer is shown
    require("neo-tree.sources.manager").get_state("git_status")
    M.set_git_base(base)
    if base then
        review.sync_visible_signs(base)
    else
        -- files opened from the explorer in ref mode got gitsigns' base changed, see on_file_opened()
        review.reset_signs_bases()
    end
    if tab_explorer_source() ~= "git_status" then
        M.toggle_git_explorer({ toggle = false })
    end
    vim.notify("Neo-tree: " .. (base and ("changes vs " .. base) or "uncommitted changes"))
end

--- Neo-tree `file_opened` handler: a file opened from the Git explorer lands on its first change and gets
--- gitsigns' base aligned with what the explorer compares against (utils/git-review-util). Files opened from
--- any other source are left alone.
---@param path string
function M.on_file_opened(path)
    if tab_explorer_source() ~= "git_status" then
        return
    end
    local buf = vim.api.nvim_get_current_buf()
    if vim.api.nvim_buf_get_name(buf) ~= path then
        buf = vim.fn.bufnr(path)
        if buf == -1 then
            return
        end
    end
    local review = require("utils.git-review-util")
    local ref = M.git_base()
    local win = vim.fn.bufwinid(buf)
    if win ~= -1 then
        review.jump_to_first_change(win, buf, ref)
    end
    review.sync_signs_base(buf, ref)
end

--- Copy files or directories to the shared clipboard.
function M.copy_to_shared_clipboard(paths)
    clear_clipboard_dir()
    local copied = {}
    for _, path in ipairs(paths) do
        local name = vim.fn.fnamemodify(path, ":t")
        local dest = clipboard_dir .. "/" .. name
        if vim.fn.isdirectory(path) == 1 then
            vim.fn.system({ "cp", "-r", path, dest })
        else
            vim.fn.system({ "cp", path, dest })
        end
        table.insert(copied, name)
    end
    vim.notify("Copied to shared clipboard:\n" .. table.concat(copied, "\n"), vim.log.levels.INFO)
end

--- Paste shared clipboard contents into the target directory.
function M.paste_from_shared_clipboard(dest_dir)
    if vim.fn.isdirectory(clipboard_dir) == 0 then
        vim.notify("Shared clipboard is empty", vim.log.levels.WARN)
        return
    end
    local items = vim.fn.readdir(clipboard_dir)
    if #items == 0 then
        vim.notify("Shared clipboard is empty", vim.log.levels.WARN)
        return
    end
    local pasted = {}
    for _, name in ipairs(items) do
        local src = clipboard_dir .. "/" .. name
        local dest = dest_dir .. "/" .. name
        if vim.fn.filereadable(dest) == 1 or vim.fn.isdirectory(dest) == 1 then
            local base = vim.fn.fnamemodify(name, ":r")
            local ext = vim.fn.fnamemodify(name, ":e")
            local counter = 1
            repeat
                local new_name = base .. "_" .. counter .. (ext ~= "" and ("." .. ext) or "")
                dest = dest_dir .. "/" .. new_name
                counter = counter + 1
            until vim.fn.filereadable(dest) == 0 and vim.fn.isdirectory(dest) == 0
            name = vim.fn.fnamemodify(dest, ":t")
        end
        if vim.fn.isdirectory(src) == 1 then
            vim.fn.system({ "cp", "-r", src, dest })
        else
            vim.fn.system({ "cp", src, dest })
        end
        table.insert(pasted, name)
    end
    clear_clipboard_dir()
    vim.notify("Pasted from shared clipboard:\n" .. table.concat(pasted, "\n"), vim.log.levels.INFO)
end

--- Copy the current neo-tree node to the shared clipboard.
function M.shared_copy(state)
    local node = state.tree:get_node()
    if node and node.type ~= "message" then
        ensure_clipboard_dir()
        M.copy_to_shared_clipboard({ node:get_id() })
    end
end

--- Copy the selected neo-tree nodes to the shared clipboard.
function M.shared_copy_visual(state, selected_nodes)
    local paths = {}
    for _, node in ipairs(selected_nodes) do
        if node.type ~= "message" then
            table.insert(paths, node:get_id())
        end
    end
    if #paths > 0 then
        ensure_clipboard_dir()
        M.copy_to_shared_clipboard(paths)
    end
end

--- Paste shared clipboard contents into the current neo-tree target.
function M.shared_paste(state)
    local node = state.tree:get_node()
    if not node then
        return
    end
    local dest = get_folder_for_node(node)
    M.paste_from_shared_clipboard(dest)
    require("neo-tree.sources.manager").refresh("filesystem")
end

return M
