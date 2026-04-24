local M = {}

---Relativize an absolute path against a list of base directories.
---Returns the shortest relative match, or the original path if no match.
---@param abs_path string
---@param base_dirs string[]
---@return string
local function relativize_to_dirs(abs_path, base_dirs)
    abs_path = vim.fs.normalize(abs_path)
    for _, dir in ipairs(base_dirs) do
        dir = vim.fs.normalize(dir)
        if not dir:match("/$") then
            dir = dir .. "/"
        end
        if abs_path:sub(1, #dir) == dir then
            return abs_path:sub(#dir + 1)
        end
    end
    return abs_path
end

---Get the content of the enclosing string literal at cursor via treesitter.
---@return { text: string, start_row: integer, start_col: integer, end_row: integer, end_col: integer }|nil
local function get_string_at_cursor()
    local ok, node = pcall(vim.treesitter.get_node)
    if not ok or not node then
        return nil
    end
    while node do
        local ntype = node:type()
        if ntype:match("string") and not ntype:match("string_interpolation") then
            local sr, sc, er, ec = node:range()
            local text = vim.treesitter.get_node_text(node, 0)
            -- Strip surrounding quotes
            if text:match('^["\']') and #text >= 2 then
                text = text:sub(2, -2)
                sc = sc + 1
                ec = ec - 1
            end
            return { text = text, start_row = sr, start_col = sc, end_row = er, end_col = ec }
        end
        node = node:parent()
    end
    return nil
end

---Get the visual selection text and range.
---@return { text: string, start_row: integer, start_col: integer, end_row: integer, end_col: integer }|nil
local function get_visual_selection()
    local s = vim.fn.getpos("'<")
    local e = vim.fn.getpos("'>")
    local sr, sc = s[2] - 1, s[3] - 1
    local er, ec = e[2] - 1, e[3]
    local end_line = vim.api.nvim_buf_get_lines(0, er, er + 1, false)[1] or ""
    ec = math.min(ec, #end_line)
    local lines = vim.api.nvim_buf_get_text(0, sr, sc, er, ec, {})
    local text = table.concat(lines, "\n")
    if text == "" then
        return nil
    end
    return { text = text, start_row = sr, start_col = sc, end_row = er, end_col = ec }
end

---@class PickContext
---@field resource_dirs string[]
---@field dirs_narrowed boolean
---@field dir_prefix string
---@field replace_range { text: string, start_row: integer, start_col: integer, end_row: integer, end_col: integer }|nil

---@param ctx PickContext
---@param abs_path string
---@return string
local function make_rel_path(ctx, abs_path)
    local rel = relativize_to_dirs(abs_path, ctx.resource_dirs)
    if ctx.dirs_narrowed then
        rel = ctx.dir_prefix .. rel
    end
    return rel
end

---@param ctx PickContext
---@param _ snacks.Picker
---@param item snacks.picker.Item
local function copy_res_path(ctx, _, item)
    local abs_path = item and Snacks.picker.util.path(item)
    if abs_path then
        local rel = make_rel_path(ctx, abs_path)
        vim.fn.setreg("+", rel)
        Snacks.notify("Copied to clipboard:\n" .. rel, { title = "Snacks Picker" })
    end
end

---@param ctx PickContext
---@param picker snacks.Picker
---@param item snacks.picker.Item
local function paste_res_path(ctx, picker, item)
    local abs_path = item and Snacks.picker.util.path(item)
    if abs_path then
        local rel = make_rel_path(ctx, abs_path)
        picker:close()
        vim.schedule(function()
            if ctx.replace_range then
                vim.api.nvim_buf_set_text(
                    0,
                    ctx.replace_range.start_row,
                    ctx.replace_range.start_col,
                    ctx.replace_range.end_row,
                    ctx.replace_range.end_col,
                    { rel }
                )
            else
                vim.api.nvim_paste(rel, true, -1)
            end
        end)
    end
end

---Handle explorer item: toggle directory or delegate to file action.
---@param ctx PickContext
---@param action fun(ctx: PickContext, picker: snacks.Picker, item: snacks.picker.Item)
---@param picker snacks.Picker
---@param item snacks.picker.explorer.Item
local function explorer_dir_or_action(ctx, action, picker, item)
    if not item then
        return
    end
    if item.dir then
        local Tree = require("snacks.explorer.tree")
        Tree:toggle(item.file)
        require("snacks.explorer.actions").update(picker, { refresh = true })
        return
    end
    action(ctx, picker, item)
end

---Recursively expand all directories under cwd in the explorer tree.
---@param cwd string
local function expand_all_dirs(cwd)
    local Tree = require("snacks.explorer.tree")
    Tree:refresh(cwd)
    local has_new = true
    while has_new do
        has_new = false
        Tree:get(cwd, function(node)
            if node.dir and not node.open then
                node.open = true
                has_new = true
            end
        end)
    end
end

---Open a file picker scoped to context-aware directories.
---For Java: scoped to module resource dirs. For other registered types: their resolver.
---Fallback: project CWD. Enter pastes the relative path at cursor.
---In normal mode: detects enclosing string literal via treesitter, narrows by dir prefix, replaces on confirm.
---In visual mode: uses selection as partial path, narrows by dir prefix, replaces selection on confirm.
---@param opts? { mode: "n"|"v", picker: "files"|"explorer" }
function M.pick(opts)
    opts = opts or {}
    local picker_type = opts.picker or "files"
    local uv = vim.uv or vim.loop
    local resolver = require("utils.resource-cwd-resolver")
    local result = resolver.resolve()

    local base_dirs, title
    if result then
        base_dirs = result.dirs
        title = result.title
    else
        local cwd = vim.fn.getcwd()
        base_dirs = { cwd }
        title = "Files [" .. vim.fn.fnamemodify(cwd, ":t") .. "]"
    end

    -- Get partial path context based on invocation mode
    local context = nil
    if opts.mode == "v" then
        context = get_visual_selection()
    elseif opts.mode == "n" then
        context = get_string_at_cursor()
    end

    -- Split partial path into directory prefix for narrowing
    local dir_prefix = ""
    local resource_dirs = base_dirs
    local dirs_narrowed = false
    local replace_range = context

    if context then
        local last_slash = context.text:match(".*()/" )
        if last_slash then
            dir_prefix = context.text:sub(1, last_slash)
            local narrowed = vim.tbl_filter(function(d)
                return uv.fs_stat(d) ~= nil
            end, vim.tbl_map(function(d)
                return vim.fs.normalize(d .. "/" .. dir_prefix)
            end, base_dirs))
            if #narrowed > 0 then
                resource_dirs = narrowed
                dirs_narrowed = true
            end
        end
        title = title .. " > " .. context.text
    end

    ---@type PickContext
    local ctx = {
        resource_dirs = resource_dirs,
        dirs_narrowed = dirs_narrowed,
        dir_prefix = dir_prefix,
        replace_range = replace_range,
    }

    if picker_type == "explorer" then
        local cwd = resource_dirs[1]
        expand_all_dirs(cwd)

        Snacks.picker.explorer({
            cwd = cwd,
            title = title,
            auto_close = true,
            follow_file = false,
            focus = "input",
            actions = {
                paste_resource_path = function(picker, item)
                    explorer_dir_or_action(ctx, paste_res_path, picker, item)
                end,
                copy_resource_path = function(picker, item)
                    explorer_dir_or_action(ctx, copy_res_path, picker, item)
                end,
            },
            win = {
                input = {
                    keys = {
                        ["<CR>"] = { "paste_resource_path", mode = { "i", "n" } },
                        ["<c-s>y"] = { "copy_resource_path", mode = { "i", "n" } },
                        ["<c-s>p"] = { "paste_resource_path", mode = { "i", "n" } },
                    },
                },
                list = {
                    keys = {
                        ["<CR>"] = "paste_resource_path",
                        ["l"] = "paste_resource_path",
                        ["<c-s>y"] = { "copy_resource_path", mode = { "i", "n" } },
                        ["<c-s>p"] = { "paste_resource_path", mode = { "i", "n" } },
                    },
                },
            },
        })
    else
        Snacks.picker.files({
            dirs = resource_dirs,
            title = title,
            confirm = "paste_resource_path",
            actions = {
                copy_resource_path = function(picker, item) copy_res_path(ctx, picker, item) end,
                paste_resource_path = function(picker, item) paste_res_path(ctx, picker, item) end,
            },
            win = {
                input = {
                    keys = {
                        ["<c-s>y"] = { "copy_resource_path", mode = { "i", "n" } },
                        ["<c-s>p"] = { "paste_resource_path", mode = { "i", "n" } },
                    },
                },
                list = {
                    keys = {
                        ["<c-s>y"] = { "copy_resource_path", mode = { "i", "n" } },
                        ["<c-s>p"] = { "paste_resource_path", mode = { "i", "n" } },
                    },
                },
            },
        })
    end
end

return M
