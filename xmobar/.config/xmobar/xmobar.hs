import Xmobar

import qualified Module.Battery as Battery
import qualified Module.Date as Date
import qualified Module.Runner as Runner
import qualified Module.Simple as Simple
import qualified Module.System as System
import qualified Module.Volume as Volume
import qualified Util.Common as U
import qualified Util.Variable as V

config :: Config
config =
    defaultConfig
        { overrideRedirect = False -- allows XMonad to manage/reserve space dynamically.
        , dpi = 96 -- default - 96.0
        , font = "CaskaydiaCove Nerd Font Bold 18"
        , additionalFonts =
            [ "CaskaydiaCove Nerd Font Bold 20" -- 1
            , "CaskaydiaCove Nerd Font Bold 22" -- 2
            , "CaskaydiaCove Nerd Font Bold 24" -- 3
            , "CaskaydiaCove Nerd Font Bold 26" -- 4
            , "CaskaydiaCove Nerd Font Bold 28" -- 5
            , "CaskaydiaCove Nerd Font Bold 30" -- 6
            , "CaskaydiaCove Nerd Font Bold 32" -- 7
            , "CaskaydiaCove Nerd Font Bold 34" -- 8
            , "CaskaydiaCove Nerd Font Bold 36" -- 9
            , "CaskaydiaCove Nerd Font Bold 38" -- 10
            , "Noto Color Emoji 20" -- 11
            , "Noto Color Emoji 22" -- 12
            , "Noto Color Emoji 24" -- 13
            , "Noto Color Emoji 26" -- 14
            , "Noto Color Emoji 28" -- 15
            , "Noto Color Emoji 30" -- 16
            , "Noto Color Emoji 32" -- 17
            , "Noto Color Emoji 34" -- 18
            , "Noto Color Emoji 36" -- 19
            , "Noto Color Emoji 38" -- 20
            ]
        , bgColor = "#232634"
        , fgColor = "#94928F" -- "#83a598" -- "#f8f8f2"
        , lowerOnStart = True
        , position = TopSize L 100 35
        , iconRoot = "/home/serhii/dotfiles/xmobar/.config/xmobar/resources"
        , commands =
            [ Run $ XPropertyLog "_XMONAD_TRAYPAD"
            , Run Simple.openWeatherCommand
            , Run Simple.kbdCommand
            , Run System.cpuCommand
            , Run System.memoryCommand
            , Run System.diskCommand
            , Run System.cpuTempCommand
            , Run Volume.alsaCommand
            , Run Date.dateCommand
            , Run Battery.batteryCommand
            , Run UnsafeXMonadLog
            ]
        , sepChar = "%"
        , alignSep = "}{"
        , template =
            concat
                [ "%UnsafeXMonadLog%"
                , " } "
                , "%date%"
                , " { "
                , "%kbd%"
                , "<hspace=3/>"
                , "%alsa:default:Master%"
                , "<hspace=3/>"
                , "%battery%"
                , "<hspace=3/>"
                , "%memory%"
                , "<hspace=3/>"
                , "%cpu%"
                , "<hspace=3/>"
                , "%coretemp%"
                , "<hspace=3/>"
                , "%disku%"
                , "<hspace=3/>"
                , "%openweather%"
                , "<hspace=3/>"
                , " | <icon=haskell.xpm/> "
                , "<hspace=3/>"
                , "<fc=#7C8377,#2E3440:0>%_XMONAD_TRAYPAD%</fc>"
                ]
                -- <action=`~/.config/rofi/scripts/launcher_t1` button=1><fc=#ed8274,#212733><fn=8>  </fn></fc></action>🧸
        }

-- modules-left = applicaitons run-gnome-control-center run-sys-monitor run-htop run-disc-monitor run-torrent run-kitty run-wezterm run-ghostty run-pipette run-idea run-browser powermenu
-- modules-right =  temperature
-- "<hspace=5/>"

main :: IO ()
main = xmobar config