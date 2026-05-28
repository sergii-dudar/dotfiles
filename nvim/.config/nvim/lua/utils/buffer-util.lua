-- Buffer management utilities: list active buffers, open/close/focus by path.
--
-- • get_active_ls_buffers — list loaded work-filetype buffers (like :ls)
-- • find_buf_by_path — find buffer number by file path
-- • close_buffer_by_path — close a buffer matching a path
-- • open_buffer_by_path — open or focus a buffer by path
-- • focus_right_if_neotree — move focus right when neo-tree is active
-- • open_scratch_split — open a scratch split buffer with given content

local window_util = require("utils.nvim.window-util")
local M = {}

local work_buffer_types = {
    java = true,
    lua = true,
}

-- get loaded bufferIds list, same as by `:ls` command or telescope buffers
--- Return loaded buffers with work filetypes.
function M.get_active_ls_buffers()
    local buffers = vim.api.nvim_list_bufs()
    local active_buffers = {}
    for _, buf in ipairs(buffers) do
        -- local buff_file_type = vim.api.nvim_buf_get_option(buf, "filetype")
        local buff_file_type = vim.api.nvim_get_option_value("filetype", { buf = buf })
        if
            vim.api.nvim_buf_is_loaded(buf)
            -- and (vim.api.nvim_buf_get_option(buf, "buflisted") or work_buffer_types[buff_file_type])
            and (vim.api.nvim_get_option_value("buflisted", { buf = buf }) or work_buffer_types[buff_file_type])
        then
            table.insert(active_buffers, buf)
        end
    end

    return active_buffers
end

---@return integer|nil
function M.find_buf_by_path(path)
    path = vim.fn.fnamemodify(path, ":p")
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(buf) then
            local name = vim.api.nvim_buf_get_name(buf)
            if name ~= "" and vim.fn.fnamemodify(name, ":p") == path then
                return buf
            end
        end
    end

    return nil
end

--- Close the buffer matching the given path.
function M.close_buffer_by_path(path)
    local buf = M.find_buf_by_path(path)
    if buf then
        vim.api.nvim_buf_delete(buf, { force = false })
        return true
    end
    return false
end

--- Open or switch to the buffer at the given path.
function M.open_buffer_by_path(path)
    vim.cmd.edit(path)
end

--- Move focus right if neo-tree is active.
function M.focus_right_if_neotree()
    local buff_name = vim.fn.expand("%") -- neo-tree filesystem [1]
    if require("utils.string-util").starts_with(buff_name, "neo-tree filesystem") then
        vim.cmd("wincmd l")
    end
end

---Open a scratch buffer in a bottom split with standard options.
---Sets buftype=nofile, bufhidden=wipe, filetype=log, modifiable=false.
---Adds `q` keymap to close the split and return to the previous window.
---@param bufnr integer Buffer to show (should already have content set)
---@param opts? { max_height?: integer, filetype?: string }
---@return integer prev_win Window that was active before opening the split
function M.open_scratch_split(bufnr, opts)
    opts = opts or {}
    local constants = require("utils.constants")
    local max_height = opts.max_height or constants.output.height_rows
    -- window_util.save_position()

    vim.bo[bufnr].buftype = "nofile"
    vim.bo[bufnr].bufhidden = "wipe"
    vim.bo[bufnr].filetype = opts.filetype or "log"
    vim.bo[bufnr].modifiable = false

    window_util.bot_split()

    local split_win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(split_win, bufnr)

    local line_count = vim.api.nvim_buf_line_count(bufnr)
    vim.api.nvim_win_set_height(split_win, math.min(line_count + 1, max_height))

    vim.wo[split_win].wrap = true
    vim.keymap.set("n", "q", function()
        if vim.api.nvim_win_is_valid(split_win) then
            vim.api.nvim_win_close(split_win, true)
        end
        window_util.restore_position()
    end, { buffer = bufnr, silent = true })
end

return M
