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
            [ "CaskaydiaCove Nerd Font Bold 22" -- 1
            , "CaskaydiaCove Nerd Font Bold 24" -- 2
            , "CaskaydiaCove Nerd Font Bold 26" -- 3
            , "CaskaydiaCove Nerd Font Bold 28" -- 4
            , "CaskaydiaCove Nerd Font Bold 30" -- 5
            , "CaskaydiaCove Nerd Font Bold 32" -- 6
            , "CaskaydiaCove Nerd Font Bold 34" -- 7
            , "CaskaydiaCove Nerd Font Bold 36" -- 8
            ]
        , bgColor = "#232634"
        , fgColor = "#94928F" -- "#83a598" -- "#f8f8f2"
        , lowerOnStart = True
        , position = TopSize L 100 35
        , commands =
            [ Run $ XPropertyLog "_XMONAD_TRAYPAD"
            , Run $
                Weather
                    "UKWW"
                    [ "--template"
                    , "<weather> <tempC>°C"
                    , "-L"
                    , "0"
                    , "-H"
                    , "25"
                    , "--low"
                    , "lightblue"
                    , "--normal"
                    , "#f8f8f2"
                    , "--high"
                    , "red"
                    ]
                    36000
            , Run Simple.kbdCommand
            , Run System.cpuCommand
            , Run Volume.alsaCommand
            , Run System.memoryCommand
            , Run Date.dateCommand
            , Run $ Swap [] 10
            , Run UnsafeXMonadLog -- XMonadLog
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
                , " | %cpu%"
                , " | %memory% * %swap%"
                , " | %EGPF%"
                , " | %_XMONAD_TRAYPAD%"
                ]
                -- <action=`~/.config/rofi/scripts/launcher_t1` button=1><fc=#ed8274,#212733><fn=8>  </fn></fc></action>🧸
        }

-- modules-left = applicaitons run-gnome-control-center run-sys-monitor run-htop run-disc-monitor run-torrent run-kitty run-wezterm run-ghostty run-pipette run-idea run-browser powermenu
-- modules-right = keyboard pulseaudio battery memory cpu temperature filesystem open-weather systray
-- "<hspace=5/>"

main :: IO ()
main = xmobar config