-- Custom winbar rendering: per-filetype path shapers with a generic default.
-- Java renders module/root/package as a dotted FQN; other filetypes fall back to a
-- cwd-relative path. Register more variants with M.register(ft, shaper).
--
-- • register — register a path shaper for a filetype
-- • eval — evaluate winbar expression for current buffer (returns statusline-style string)

local M = {}

local existing_win_bar_hi = vim.api.nvim_get_hl(0, { name = "WinBar", link = false })
local existing_path_hi = vim.api.nvim_get_hl(0, { name = "WinBarPath", link = false })

vim.api.nvim_set_hl(0, "WinBar", { fg = "#ff8700", bg = existing_win_bar_hi.bg, bold = false, italic = true })
vim.api.nvim_set_hl(0, "WinBarPath", {
    fg = "#676868",
    bg = existing_path_hi.bg, --[[bold = true italic = true]]
})

-- filetype -> fun(raw_path: string): string  (rendered path portion of the winbar)
local shapers = {}

--- Register a winbar path shaper for a filetype.
---@param ft string
---@param shaper fun(raw_path: string): string
function M.register(ft, shaper)
    shapers[ft] = shaper
end

-- Strip the leading "<cwd-name>/" so the path shows relative to the project.
local function cwd_relative(str)
    local cwd = vim.fn.getcwd():gsub("/$", "")
    if str:sub(1, #cwd + 1) == cwd .. "/" then
        return str:sub(#cwd + 2)
    end

    local dir_name = vim.fn.fnamemodify(cwd, ":t")
    local parts = require("utils.string-util").split(str, dir_name .. "/")
    return #parts == 2 and parts[2] or str
end

-- Default shaper: cwd-relative path with ❯ separators.
local function default_shaper(str)
    return cwd_relative(str):gsub("/", " ❯ ")
end

local function java_root_markers()
    local java = require("utils.lang.registry").for_filetype("java")
    return (java and java.project and java.project.markers) or {}
end

local function has_java_root_marker(dir)
    if not dir or dir == "" then
        return false
    end

    for _, marker in ipairs(java_root_markers()) do
        if vim.uv.fs_stat(dir .. "/" .. marker) then
            return true
        end
    end

    return false
end

local function is_nested_java_module(module_path)
    local absolute_module_path = vim.fn.fnamemodify(module_path, ":p"):gsub("/$", "")
    local parent_path = vim.fn.fnamemodify(absolute_module_path, ":h")
    return parent_path ~= absolute_module_path and has_java_root_marker(parent_path)
end

-- Java shaper: "module 󰏗 root 󰬷 package" for multi-module projects,
-- or "root 󰬷 package" when the project root is the Java module.
local function java_shaper(str)
    local module_path, root, package = str:match("(.+)/src/([^/]+)/java/(.*)")
    local module = module_path and vim.fn.fnamemodify(module_path, ":t")
    if not package then
        root, package = str:match("^src/([^/]+)/java/(.*)")
    end

    local base
    if package then
        if not module_path or not is_nested_java_module(module_path) then
            module = nil
        end

        local prefix = module and (module .. " 󰏗 ") or ""
        base = prefix .. root .. " 󰬷 " .. package
    else
        base = cwd_relative(str)
    end
    return base:gsub("/", "."):gsub("%.java", "")
end

M.register("java", java_shaper)

--- Evaluate the winbar expression for the current buffer.
function M.eval()
    local raw = vim.api.nvim_buf_get_name(0)
    if raw == "" then
        raw = vim.api.nvim_eval_statusline("%f", {}).str
    end
    local shaper = shapers[vim.bo.filetype] or default_shaper
    local file_path = " " .. shaper(raw)

    local ext = vim.fn.fnamemodify(raw, ":e")
    local filename = vim.fn.fnamemodify(raw, ":t")
    local dev_icon = require("nvim-web-devicons").get_icon(filename, ext)
    dev_icon = (dev_icon and " " .. dev_icon .. " " or "")

    return "%#WinBarSeparator#"
        .. "%*"
        .. "%#WinBarPath#"
        .. file_path
        .. dev_icon
        .. "%*"
        .. "%#WinBarSeparator#"
        .. "%*"
end

return M
