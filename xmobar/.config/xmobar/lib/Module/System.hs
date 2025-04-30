module Module.System where

import Xmobar

cpuCommand :: Rate -> Monitors
cpuCommand =
    Cpu
        [ "-L"
        , "3"
        , "-H"
        , "50"
        , "--high"
        , "red"
        , "--normal"
        , "green"
        ]

memoryCommand :: Rate -> Monitors
memoryCommand = Memory ["--template", "Mem: <usedratio>%"]

alsaCommand :: Monitors
alsaCommand =
    Alsa
        "default"
        "Master"
        [ "--template"
        , "<volumestatus>"
        , "--suffix"
        , "True"
        , "--"
        , "--on"
        , ""
        ]