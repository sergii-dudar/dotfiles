local awful = require("awful")

M = {}

M.notify = function(msg)
    awful.util.spawn('notify-send "' .. msg .. '" -t 700')
end

return M
