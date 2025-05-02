module Module.System
    ( cpuCommand
    , memoryCommand
    , cpuTempCommand
    , diskCommand
    ) where

import Xmobar

cpuTemplate :: String
cpuTemplate = buildSystemTemplate "\xf2db " "#8caaee" "<total>" 1 "%"
cpuCommand :: Monitors
cpuCommand = buildSystemMonitor Cpu "50" "3" cpuTemplate 50

memoryTemplate :: String
memoryTemplate = buildSystemTemplate "\xf233 " "#a6e3a1" "<usedratio>" 5 "%"
memoryCommand :: Monitors
memoryCommand = buildSystemMonitor Memory "80" "10" memoryTemplate 50

cpuTempTemplate :: String
cpuTempTemplate = buildSystemTemplate "\xf2c7" "#a6e3a1" "<core0>" 5 "°C"
cpuTempCommand :: Monitors
cpuTempCommand = buildSystemMonitor CoreTemp "80" "70" cpuTempTemplate 100

diskTemplate :: String
diskTemplate = buildSystemTemplate "\xf0a0 " "#e5c890" "<usedp>" 5 "%<hspace=4/>SSD"
diskCommand :: Monitors
diskCommand = DiskU [("/", diskTemplate)] [] 36000

buildSystemTemplate :: String -> String -> String -> Int -> String -> String
buildSystemTemplate icon iconColor value unitSpace unit =
    concat
        [ "<fc=#94928F,#2E3440:0>"
        , "<hspace=12/>"
        , "<fc=" ++ iconColor ++ ",#2E3440:0>" ++ icon ++ "</fc>"
        , "<hspace=" ++ show unitSpace ++ "/>"
        , value
        , "<hspace=1/>"
        , "<fc=#6272a4,#2E3440:0>" ++ unit ++ "</fc><hspace=12/>"
        , "</fc>"
        ]

buildSystemMonitor :: (Args -> Rate -> Monitors) -> String -> String -> String -> Int -> Monitors
buildSystemMonitor monitorCtor high low template =
    monitorCtor
        [ "--template"
        , template
        , "--ppad"
        , "2"
        , "--High"
        , high
        , "--Low"
        , low
        ]