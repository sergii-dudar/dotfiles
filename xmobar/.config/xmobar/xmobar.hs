import Xmobar

import qualified Module.Battery as Battery
import qualified Module.Date as Date
import qualified Module.RunnerApp as RunnerApp
import qualified Module.RunnerTemplate as RunnerTemplate
import qualified Module.Simple as Simple
import qualified Module.System as System
import qualified Module.Volume as Volume
import qualified Util.Common as U
import qualified Util.Variable as V

config :: Config
config =
    defaultConfig
        { overrideRedirect = False -- allows XMonad to manage/reserve space dynamically.
        , dpi = 96
        , font = V.defaultFont
        , additionalFonts = V.additionalFonts
        , bgColor = "#232634"
        , fgColor = "#94928F" -- "#83a598" -- "#f8f8f2"
        , lowerOnStart = True
        , position = TopSize L 100 35
        , iconRoot = V.xmobarHomeDir ++ "/resources"
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
                , RunnerTemplate.dateRunner -- "%date%"
                , " { "
                , RunnerTemplate.kbdRunner -- "%kbd%"
                , "<hspace=3/>"
                , RunnerTemplate.alsaRunner
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
                , RunnerTemplate.weatherRunner
                , "<hspace=3/>"
                , "<action=`~/.config/rofi/scripts/launcher_t1` button=1><action=`~/.config/rofi/scripts/powermenu_t1` button=3><icon=haskell.xpm/></action></action>"
                , " | <icon=haskell.xpm/> <icon=settings.xpm/> "
                , "<hspace=3/>"
                , "<fc=#7C8377,#2E3440:0>%_XMONAD_TRAYPAD%</fc>"
                ]
                -- <action=`~/.config/rofi/scripts/launcher_t1` button=1><fc=#ed8274,#212733><fn=8>  </fn></fc></action>🧸
        }

-- modules-left = applicaitons run-gnome-control-center run-sys-monitor run-htop run-disc-monitor run-torrent run-kitty run-wezterm run-ghostty run-pipette run-idea run-browser powermenu
-- "<hspace=5/>"

main :: IO ()
main = xmobar config