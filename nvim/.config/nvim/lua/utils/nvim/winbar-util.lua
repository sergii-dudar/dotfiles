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
    local dir_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
    local parts = require("utils.string-util").split(str, dir_name .. "/")
    return #parts == 2 and parts[2] or str
end

-- Default shaper: cwd-relative path with ❯ separators.
local function default_shaper(str)
    return cwd_relative(str):gsub("/", " ❯ ")
end

-- Java shaper: "module 󰏗 root 󰬷 package" as a dotted FQN, sans .java.
local function java_shaper(str)
    local module, root, package = str:match("([^/]+)/src/(%w+)/java/(.*)")
    if not package then
        root, package = str:match("^.*src/(%w*)/java/(.*)")
    end

    local base
    if package then
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
    local raw = vim.api.nvim_eval_statusline("%f", {}).str
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
