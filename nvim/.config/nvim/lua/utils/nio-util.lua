-- Async nvim-nio wrappers: bridging Snacks pickers and vim.ui to coroutine-based flow.
--
-- • select — async single-item picker (wraps Snacks.picker.select)
-- • input — async text input (wraps Snacks.input)
-- • multi_select — async multi-select picker with tab-toggle
-- • run — run an async function in a new nio coroutine

local nio = require("nio")

local M = {}

M.select = nio.wrap(function(items, prompt, callback)
    Snacks.picker.select(items, { prompt = prompt }, callback)
end, 3)

M.input = nio.wrap(function(prompt, callback)
    Snacks.input({ prompt = prompt }, callback)
end, 2)

--- Multi-select picker bridged to nio.
--- Items must have a `.name` field for display.
--- Optional item `.chunks` or `.format()` customizes Snacks-highlighted display.
--- Tab toggles selection, Enter confirms.
--- Returns selected items list or nil on cancel.
---@param items table[]
---@param prompt string
---@return table[]|nil
M.multi_select = nio.wrap(function(items, prompt, callback)
    local completed = false

    --- Complete the picker callback at most once.
    local function complete(result)
        if completed then
            return
        end
        completed = true
        callback(result)
    end

    Snacks.picker.pick({
        title = prompt,
        layout = { preview = false, layout = { width = 0.4, height = 0.3 } },
        items = vim.tbl_map(function(item)
            return { text = item.name, item = item }
        end, items),
        format = function(picker_item)
            local item = picker_item.item or {}
            if type(item.format) == "function" then
                return item.format(item, picker_item)
            end
            if type(item.chunks) == "table" then
                return item.chunks
            end
            return { { picker_item.text } }
        end,
        confirm = function(picker, _)
            if completed then
                return
            end

            local selected = picker:selected({ fallback = true })
            local result = vim.tbl_map(function(sel)
                return sel.item
            end, selected)

            completed = true
            picker:close()
            callback(#result > 0 and result or nil)
        end,
        on_close = function()
            complete(nil)
        end,
    })
end, 3)

M.run = nio.run

return M
