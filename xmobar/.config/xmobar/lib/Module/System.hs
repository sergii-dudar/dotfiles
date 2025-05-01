module Module.System where

import Xmobar

cpuCommand :: Monitors
cpuCommand =
    Cpu
        [ "--template"
        , concat
            [ "<fc=#94928F,#2E3440:0>"
            , "<hspace=12/>"
            , "<fc=#8caaee,#2E3440:0>\xf2db </fc>"
            , "<hspace=1/><total><hspace=1/>"
            , "<fc=#6272a4,#2E3440:0>%</fc><hspace=12/>"
            , "</fc>"
            ]
        , "--ppad"
        , "2" -- Add zero padding to make volume two digits wide
        , "--Low"
        , "3"
        , "--High"
        , "50"
        ]
        50

memoryCommand :: Monitors
memoryCommand =
    Memory
        [ "--template"
        , concat
            [ "<fc=#94928F,#2E3440:0>"
            , "<hspace=12/>"
            , "<fc=#a6e3a1,#2E3440:0>\xf233 </fc>"
            , "<hspace=5/><usedratio><hspace=1/>"
            , "<fc=#6272a4,#2E3440:0>%</fc><hspace=12/>"
            , "</fc>"
            ]
        , "--ppad"
        , "2" -- Add zero padding to make volume two digits wide
        , "--High"
        , "80"
        , "--Low"
        , "10"
        ]
        50

diskCommand :: Monitors
diskCommand =
    DiskU
        [
            ( "/"
            , concat
                [ "<fc=#94928F,#2E3440:0>"
                , "<hspace=12/>"
                , "<fc=#e5c890,#2E3440:0>\xf0a0 </fc>"
                , "<hspace=5/><usedp><hspace=1/>"
                , "<fc=#6272a4,#2E3440:0>%</fc>"
                , "<hspace=4/><fc=#6272a4,#2E3440:0>SSD</fc><hspace=12/>"
                , "</fc>"
                ]
            )
        ]
        []
        36000

cpuTempCommand :: Monitors
cpuTempCommand =
    -- cpu core temperature monitor
    CoreTemp
        [ "--template"
        , concat
            [ "<fc=#94928F,#2E3440:0>"
            , "<hspace=12/>"
            , "<fc=#a6e3a1,#2E3440:0>\xf2c7</fc>"
            , "<hspace=5/><core0><hspace=1/>"
            , "<fc=#6272a4,#2E3440:0>°C</fc><hspace=12/>"
            , "</fc>"
            ]
        , "--Low"
        , "70" -- units: °C
        , "--High"
        , "80" -- units: °C
        -- , "--low"
        -- , "darkgreen"
        -- , "--normal"
        -- , "darkorange"
        -- , "--high"
        -- , "darkred"
        ]
        50