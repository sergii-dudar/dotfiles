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

M.run_shell_autostarts = function()
    -- Autostart applications
    awful.spawn.with_shell([[
        if [ ! -f /tmp/awesome_startup_done ]; then
            ~/dotfiles/bin/wmscripts/autostart_once.sh awesome
            touch /tmp/awesome_startup_done
        fi
    ]])

    awful.spawn.with_shell("~/dotfiles/bin/wmscripts/autostart_always.sh awesome")
end

return M
