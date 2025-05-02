module Module.RunnerTemplate where

import qualified Util.Common as U
import qualified Util.Element as E
import qualified Util.Variable as V

import Xmobar

dateRunner :: String
dateRunner =
    E.action V.appsGnomeClocks 1
        . E.action V.appsGnomeCalendar 3
        $ "%date%"

kbdRunner :: String
kbdRunner =
    E.action V.runChangeLanguage 1
        . E.action V.runChangeLanguage 3
        $ "%kbd%"

alsaRunner :: String
alsaRunner =
    E.action V.runVolumeToggle 1
        . E.action V.appsVolumeControl 3
        . E.action V.runVolumeIncrease 4
        . E.action V.runVolumeDecrease 5
        $ "%alsa:default:Master%"

weatherRunner :: String
weatherRunner =
    E.action V.runWeatherWeb 1 $ "%openweather%"

{-

cpu.widget:buttons(awful.util.table.join(awful.button({}, 1, function()
    awful.util.spawn(vars.run.gnome_system_monitor)
end)))

mem.widget:buttons(awful.util.table.join(awful.button({}, 1, function()
    awful.util.spawn(vars.run.htop)
end)))

fs.widget:buttons(awful.util.table.join(awful.button({}, 1, function()
    awful.util.spawn(vars.run.disc_gdu)
end)))

-}
-- awful.spawn.with_shell(vars.run.gnome_clocks)
-- awful.spawn.with_shell(vars.run.gnome_calendar)