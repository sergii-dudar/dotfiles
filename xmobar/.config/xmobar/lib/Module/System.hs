module Module.System where

import Xmobar

cpuCommand :: Monitors
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
        10

memoryCommand :: Monitors
memoryCommand = Memory ["--template", "Mem: <usedratio>%"] 10
