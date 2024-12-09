local wibox = require("wibox")
local beautiful = require("beautiful")
local awful = require("awful")

local interval = 10

awful.widget.watch('sh -c "top -bn1 | grep "Cpu""', interval, function(_, stdout)
    local us = tonumber(string.match(stdout, " (.*) us"))
    local sy = tonumber(string.match(stdout, ", *(.*) sy"))
    local usage = us + sy
    awesome.emit_signal("status::cpu", usage)
end)

local cpu = wibox.widget({
    widget = wibox.widget.textbox,
})

awesome.connect_signal("status::cpu", function(usage)
    cpu.font = beautiful.font
    local markup = "<span foreground='#5B6268'> </span>" .. usage .. "%"

    cpu.markup = markup
end)

return cpu
