module Module.Volume where

import Xmobar

alsaCommand :: Monitors
alsaCommand =
    Alsa
        "default"
        "Master"
        [ "--template"
        , concat
            [ "<fc=#94928F,#2E3440:0> "
            , "<status>"
            , "<hspace=5/><volume>"
            , "<hspace=1/><fc=#6272a4,#2E3440:0>%</fc>"
            , "<hspace=12/>"
            , "</fc>"
            ]
        , "--ppad"
        , "2" -- Add zero padding to make volume two digits wide
        , "--"
        , "--on"
        , ""
        , "--off"
        , "<fc=#d35f5e,#2E3440:0>\xeee8 </fc>"
        , -- , "--offc", "#d35f5e"
          "--highv"
        , "10"
        , "--lowv"
        , "5"
        , "--highs"
        , "<fc=#ca9ee6,#2E3440:0>\xf028 </fc>"
        , "--mediums"
        , "<fc=#ca9ee6,#2E3440:0>\xf027 </fc>"
        , "--lows"
        , "<fc=#ca9ee6,#2E3440:0>\xf026 </fc>"
        ]