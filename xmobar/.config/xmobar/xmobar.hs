import Xmobar

import qualified Module.System as S
import qualified Module.Variable as V
import qualified Util.Common as U

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
            , Run S.cpuCommand
            , Run S.alsaCommand
            , Run S.memoryCommand
            , Run $ Swap [] 10
            , Run $ Date "%a %Y-%m-%d <fc=#8be9fd>%H:%M</fc>" "date" 10
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
                , "%alsa:default:Master%"
                , " | %cpu%"
                , " | %memory% * %swap%"
                , " | %EGPF%"
                , " | %_XMONAD_TRAYPAD%"
                ]
                -- <action=`~/.config/rofi/scripts/launcher_t1` button=1><fc=#ed8274,#212733><fn=8>  </fn></fc></action>🧸
        }

main :: IO ()
main = xmobar config