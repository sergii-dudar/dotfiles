local M = {}

local existing_win_bar_hi = vim.api.nvim_get_hl(0, { name = 'WinBar', link = false })
local existing_path_hi = vim.api.nvim_get_hl(0, { name = 'WinBarPath', link = false })
local existing_hi = vim.api.nvim_get_hl(0, { name = 'WinBarContentModified', link = false })

vim.api.nvim_set_hl(0, 'WinBar', { fg = '#ff8700', bg = existing_win_bar_hi.bg, bold = true })
vim.api.nvim_set_hl(0, 'WinBarPath', { fg = '#676868', bg = existing_path_hi.bg, --[[bold = true italic = true]] })
vim.api.nvim_set_hl(0, 'WinBarContentModified', { fg = '#f54257', bg = existing_hi.bg })

function split_str_by_src(str)
    local root, package = str:match("^.*/src/(%w*)/java/(.*)")

    if package then
        return root.."  "..package
    end

    return str
end

M.eval = function()

	--local file_path = vim.api.nvim_eval_statusline('%f', {}).str
    local file_path = split_str_by_src(vim.api.nvim_eval_statusline('%f', {}).str)

	local has_modified = vim.api.nvim_eval_statusline('%m', {}).str == '[+]'
	local modified_status = has_modified and '  ' or ''

	file_path = ' '..file_path:gsub('/', ' ➤ ')

    --vim.notify(file_path)
	return '%#WinBarSeparator#'
		.. '%*'
		.. '%#WinBarPath#'
		.. file_path
		.. '%*'
		.. '%#WinBarContentModified#'
		.. modified_status
		.. '%*'
		.. '%#WinBarSeparator#'
		.. '%*'
        --.. ' '
        --.. "%{%v:lua.require'nvim-navic'.get_location()%}"
end

return M
