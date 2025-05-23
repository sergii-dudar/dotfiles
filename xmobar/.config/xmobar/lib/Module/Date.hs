module Module.Date where

import Xmobar

dateCommand :: Date
dateCommand =
    Date
        ( concat
            [ "<fc=#7C8377,#2E3440:0>"
            , " \xf073 "
            , "<fc=#6272a4,#2E3440:0>%a,</fc>"
            , "<hspace=5/>%b"
            , "<hspace=5/>%d<hspace=7/>"
            , "</fc>"
            , "<hspace=3/>"
            , "<fc=#7C8377,#2E3440:0>"
            , "<hspace=9/><fc=#bd93f9,#2E3440:0>\xf017 </fc>"
            , "<hspace=5/><fc=#a6d189,#2E3440:0>%I:%M</fc>"
            , "<hspace=5/><fc=#8caaee,#2E3440:0>%p</fc>"
            , "<hspace=9/></fc>"
            ]
        )
        "date"
        10