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
--- Tab toggles selection, Enter confirms.
--- Returns selected items list or nil on cancel.
---@param items table[]
---@param prompt string
---@return table[]|nil
M.multi_select = nio.wrap(function(items, prompt, callback)
    Snacks.picker.pick({
        title = prompt,
        layout = { preview = false, layout = { width = 0.4, height = 0.3 } },
        items = vim.tbl_map(function(item)
            return { text = item.name, item = item }
        end, items),
        format = function(picker_item)
            return { { picker_item.text } }
        end,
        confirm = function(picker, picker_item)
            local selected = picker:selected({ fallback = true })
            picker:close()
            local result = vim.tbl_map(function(sel)
                return sel.item
            end, selected)
            callback(#result > 0 and result or nil)
        end,
        cancel = function(picker)
            picker:close()
            callback(nil)
        end,
    })
end, 3)

M.run = nio.run

return M
