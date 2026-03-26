local M = {}

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

function M.shared_copy(state)
    local node = state.tree:get_node()
    if node and node.type ~= "message" then
        ensure_clipboard_dir()
        M.copy_to_shared_clipboard({ node:get_id() })
    end
end

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