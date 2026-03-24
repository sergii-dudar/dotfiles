local M = {}

---Resolve an absolute file path from an item with `file` and optional `cwd` fields.
---@param item { file?: string, cwd?: string }
---@return string?
local resolve_path = function(item)
    local file = item.file
    if not file then
        return nil
    end
    if item.cwd and not vim.startswith(file, "/") then
        file = item.cwd .. "/" .. file
    end
    return vim.fs.normalize(file)
end

---Open two files side-by-side in diff mode in a new tab.
---@param file1 string absolute path to the first file
---@param file2 string absolute path to the second file
function M.diff_files(file1, file2)
    vim.cmd("tabnew " .. vim.fn.fnameescape(file1))
    vim.cmd("diffthis")
    vim.cmd("vsplit " .. vim.fn.fnameescape(file2))
    vim.cmd("diffthis")
end

---Validate and diff two selected items. Shows a warning if not exactly 2 items.
---@param items { file?: string, cwd?: string }[]
---@return boolean ok true if diff was initiated
function M.diff_selected(items)
    if #items ~= 2 then
        local msg = #items < 2 and "Select exactly 2 files to diff (use <Tab> to select)"
            or ("Select exactly 2 files to diff (got " .. #items .. ")")
        vim.notify(msg, vim.log.levels.WARN)
        return false
    end
    local file1 = resolve_path(items[1])
    local file2 = resolve_path(items[2])
    if not file1 or not file2 then
        vim.notify("Could not resolve file paths", vim.log.levels.ERROR)
        return false
    end
    M.diff_files(file1, file2)
    return true
end

return M
