module Module.Simple where

import Xmobar

kbdCommand :: Kbd
kbdCommand =
    Kbd
        [ ("ua", "<fc=#cba6f7,#2E3440:0> 🇺🇦<hspace=5/>UA<hspace=10/></fc>")
        , ("us", "<fc=#cba6f7,#2E3440:0> 🇺🇸<hspace=5/>US<hspace=10/></fc>")
        ]