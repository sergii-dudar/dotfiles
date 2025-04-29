Config { overrideRedirect = False -- allows XMonad to manage/reserve space dynamically.
       , dpi = 96 -- default - 96.0
       , font = "CaskaydiaCove Nerd Font Bold 20"
       , additionalFonts = [
          "CaskaydiaCove Nerd Font Bold 22" -- 1
        , "CaskaydiaCove Nerd Font Bold 24" -- 2
        , "CaskaydiaCove Nerd Font Bold 26" -- 3
       ]
       , bgColor  = "#232634"
       , fgColor  = "#94928F" -- "#83a598" -- "#f8f8f2"
       , lowerOnStart = True 
       , position       = TopSize L 100 35 
       , commands = [ 
                        Run XPropertyLog "_XMONAD_TRAYPAD"
                    ,   Run Weather "EGPF"
                        [ "--template", "<weather> <tempC>°C"
                        , "-L", "0"
                        , "-H", "25"
                        , "--low"   , "lightblue"
                        , "--normal", "#f8f8f2"
                        , "--high"  , "red"
                        ] 36000
                    , Run Cpu
                        [ "-L", "3"
                        , "-H", "50"
                        , "--high"  , "red"
                        , "--normal", "green"
                        ] 10
                    , Run Alsa "default" "Master"
                        [ "--template", "<volumestatus>"
                        , "--suffix"  , "True"
                        , "--"
                        , "--on", ""
                        ]
                    , Run Memory ["--template", "Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %Y-%m-%d <fc=#8be9fd>%H:%M</fc>" "date" 10
                    , Run UnsafeXMonadLog -- XMonadLog
                    ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "%UnsafeXMonadLog% } %date% { %alsa:default:Master% | %cpu% | %memory% * %swap% | %EGPF% | %_XMONAD_TRAYPAD%"
       }