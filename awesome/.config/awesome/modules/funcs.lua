local awful = require("awful")

local M = {}

M.keychord = function(chords)
    local g = awful.keygrabber({
        stop_key = "Escape",
        keypressed_callback = function(self, _, key)
            if chords[key] then
                chords[key]()
            end
            self:stop()
        end,
    })
    return function()
        g:start()
    end
end

return M