-- Neo-tree helpers: context-aware explorer routing and cross-instance shared clipboard.
--
-- • toggle_context_explorer — route special buffers to their explorer, otherwise reveal them in the
--   Neo-tree source already on screen (file tree, git changes, …)
-- • open_explorer — open a Neo-tree source in the sidebar, revealing the current file
-- • toggle_git_explorer — open_explorer bound to the git_status source
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
