module Module.Simple where

import Xmobar

kbdCommand :: Kbd
kbdCommand =
    Kbd
        [ ("ua", "<fc=#cba6f7,#2E3440:0> 🇺🇦<hspace=5/>UA<hspace=10/></fc>")
        , ("us", "<fc=#cba6f7,#2E3440:0> 🇺🇸<hspace=5/>US<hspace=10/></fc>")
        ]

openWeatherCommand :: Command
openWeatherCommand =
    Com
        "/home/serhii/dotfiles/xmobar/.config/xmobar/shell/module.open-weather"
        []
        "openweather"
        6000 -- 600 + 0 # once a 10 min (bash script using cached value updated not ofter as once 30 min)