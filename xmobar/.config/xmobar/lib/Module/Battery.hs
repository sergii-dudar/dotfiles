module Module.Battery (batteryCommand, batteryExists) where

import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Xmobar

batteryCommand :: Monitors
batteryCommand =
    Battery
        [ "--template"
        , concat
            [ "<fc=#94928F,#2E3440:0>"
            , "<hspace=12/><acstatus><hspace=10/>"
            , "</fc>"
            ]
        , "--Low"
        , "10" -- units: %
        , "--High"
        , "60" -- units: %
        , "--ppad"
        , "2" -- Add zero padding to make volume two digits wide
        , "--" -- battery specific options
        -- discharging status
        , "-o"
        , toBatteryStatus "\xf241"
        , -- AC "on" status
          "-O"
        , toBatteryStatus "\xf240"
        , -- charged status
          "-i"
        , toBatteryStatus "\xf240"
        ]
        600

toBatteryStatus :: String -> String
toBatteryStatus iconCode =
    "<fc=#a6d189,#2E3440:0><fn=1>"
        ++ iconCode
        ++ " </fn></fc><hspace=5/><left><hspace=1/><fc=#6272a4,#2E3440:0>%</fc>"

batteryExists :: IO Bool
batteryExists = do
    let powerSupplyPath = "/sys/class/power_supply"
    exists <- doesDirectoryExist powerSupplyPath
    if not exists
        then return False
        else do
            entries <- listDirectory powerSupplyPath
            return $ any ("BAT" `isPrefixOf`) entries