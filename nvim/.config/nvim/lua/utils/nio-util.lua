local nio = require("nio")

local M = {}

M.select = nio.wrap(function(items, prompt, callback)
    Snacks.picker.select(items, { prompt = prompt }, callback)
end, 3)

M.input = nio.wrap(function(prompt, callback)
    Snacks.input({ prompt = prompt }, callback)
end, 2)

M.run = nio.run

return M
